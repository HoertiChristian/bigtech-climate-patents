import pandas as pd
from pathlib import Path

OUTPUT_DIR = Path(__file__).resolve().parent / "bertopic_output"

df = pd.read_csv(OUTPUT_DIR / "document_topics.csv")

def extract_subclasses(code_string):
    """Extract unique Y02/Y04 subclasses (e.g., Y02D, Y02E) from the semicolon-separated string."""
    if pd.isna(code_string) or not code_string:
        return []
    subclasses = set()
    for code in str(code_string).split(";"):
        code = code.strip()
        if len(code) >= 4:
            subclasses.add(code[:4])
    return list(subclasses)

# Create the subclasses column BEFORE exploding
df["subclasses"] = df["cpc_climate_codes"].apply(extract_subclasses)

print(f"Rows in df: {len(df)}")
print(f"Columns: {list(df.columns)}")
print(f"Rows with at least one subclass: {(df['subclasses'].str.len() > 0).sum()}")

# Explode: one row per (patent, subclass)
exploded = df.explode("subclasses").dropna(subset=["subclasses"]).reset_index(drop=True)
exploded = exploded[exploded["subclasses"].astype(str).str.len() > 0]

print(f"Rows after explode: {len(exploded)}")
print(f"Unique subclasses found: {sorted(exploded['subclasses'].unique())}")

# Cross-tab: topics × CPC subclass
crosstab = pd.crosstab(exploded["topic"], exploded["subclasses"])
crosstab["TOTAL_PATENTS"] = df.groupby("topic").size()

# Normalize to row percentages
pct = crosstab.drop(columns="TOTAL_PATENTS").div(crosstab["TOTAL_PATENTS"], axis=0) * 100
pct = pct.round(1)
pct["TOTAL_PATENTS"] = crosstab["TOTAL_PATENTS"]

print("\n" + "=" * 80)
print("CPC subclass composition by topic (% of patents in topic tagged with each subclass)")
print("Rows may sum >100% because patents can carry multiple subclasses")
print("=" * 80)
print(pct.sort_values("TOTAL_PATENTS", ascending=False).to_string())

pct.to_csv(OUTPUT_DIR / "topic_cpc_composition.csv")
print(f"\nSaved to {OUTPUT_DIR / 'topic_cpc_composition.csv'}")

# Focused print for mega-clusters
print("\n" + "=" * 80)
print("MEGA-CLUSTER DIAGNOSTIC")
print("=" * 80)
for tid in [-1, 0]:
    if tid in pct.index:
        row = pct.loc[tid].drop("TOTAL_PATENTS").sort_values(ascending=False)
        total = int(pct.loc[tid, "TOTAL_PATENTS"])
        print(f"\nTopic {tid} ({total} patents):")
        for subclass, share in row.items():
            if share > 1:
                print(f"  {subclass}: {share:.1f}%")






import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path

OUTPUT_DIR = Path(__file__).resolve().parent / "bertopic_output"

pct = pd.read_csv(OUTPUT_DIR / "topic_cpc_composition.csv", index_col=0)

# Separate out the totals column, sort topics by size descending
totals = pct["TOTAL_PATENTS"].astype(int)
heatmap_data = pct.drop(columns="TOTAL_PATENTS")

order = totals.sort_values(ascending=False).index
heatmap_data = heatmap_data.loc[order]
totals = totals.loc[order]

# Keep only Y02/Y04 columns that actually have signal (>0 anywhere)
heatmap_data = heatmap_data.loc[:, (heatmap_data > 0).any(axis=0)]

# Build y-axis labels: "Topic X (n=123)"
y_labels = [
    f"Outliers (n={totals.loc[t]})" if t == -1 else f"Topic {t} (n={totals.loc[t]})"
    for t in heatmap_data.index
]

fig, ax = plt.subplots(
    figsize=(10, 0.38 * len(heatmap_data) + 2),
    dpi=200,
)

im = ax.imshow(
    heatmap_data.values,
    aspect="auto",
    cmap="YlGnBu",
    vmin=0,
    vmax=100,
)

# Axis ticks and labels
ax.set_xticks(np.arange(len(heatmap_data.columns)))
ax.set_xticklabels(heatmap_data.columns, fontsize=10)
ax.set_yticks(np.arange(len(heatmap_data)))
ax.set_yticklabels(y_labels, fontsize=9)
ax.set_xlabel("CPC subclass", fontsize=11)
ax.set_title(
    "CPC subclass composition by BERTopic cluster\n(% of patents in topic tagged with each subclass)",
    fontsize=12, fontweight="bold", pad=12,
)

# Annotate each cell with its percentage
for i in range(heatmap_data.shape[0]):
    for j in range(heatmap_data.shape[1]):
        val = heatmap_data.values[i, j]
        if val >= 1:
            # White text on dark cells, black on light
            color = "white" if val > 55 else "black"
            ax.text(
                j, i, f"{val:.0f}",
                ha="center", va="center",
                fontsize=8, color=color,
            )

# Colorbar
cbar = fig.colorbar(im, ax=ax, shrink=0.6, pad=0.02)
cbar.set_label("% of topic's patents", fontsize=10)

# Grid separating cells
ax.set_xticks(np.arange(-0.5, len(heatmap_data.columns)), minor=True)
ax.set_yticks(np.arange(-0.5, len(heatmap_data)), minor=True)
ax.grid(which="minor", color="white", linewidth=1.5)
ax.tick_params(which="minor", length=0)

fig.tight_layout()
out_path = OUTPUT_DIR / "fig_topic_cpc_heatmap.png"
fig.savefig(out_path, bbox_inches="tight")
plt.close(fig)
print(f"Saved {out_path}")