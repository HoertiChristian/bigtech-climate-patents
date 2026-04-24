"""
Firm × Topic Distribution (Percentage)
======================================
Generates a normalized (percentage-wise) topic distribution per firm
from existing BERTopic outputs. Each firm's bar sums to 100%, making
it easy to compare *relative focus* across firms regardless of their
absolute patent counts.

Reads:
  - bertopic_output/company_topic_distribution.csv
  - bertopic_output/topic_info.csv

Outputs:
  - bertopic_output/fig_firm_topic_distribution_pct.png
  - bertopic_output/company_topic_distribution_pct.csv
"""

import pandas as pd
import numpy as np
from pathlib import Path
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# ---------------------------------------------------------------------------
# CONFIGURATION
# ---------------------------------------------------------------------------
SCRIPT_DIR = Path(__file__).resolve().parent
OUTPUT_DIR = SCRIPT_DIR / "bertopic_output"

COMPANY_TOPIC_CSV = OUTPUT_DIR / "company_topic_distribution.csv"
TOPIC_INFO_CSV = OUTPUT_DIR / "topic_info.csv"

# How many top topics to include in the chart (by total size across firms).
# Remaining topics are grouped into "Other" so every firm's bar still sums to 100%.
TOP_N_TOPICS = 15

# Fixed firm order (matches the rest of the project)
FIRM_ORDER = ["Apple", "Microsoft", "Amazon", "Meta", "Alphabet"]

FIGSIZE = (14, 6)
DPI = 200

OTHER_COLOR = "#bcbcbc"

# ---------------------------------------------------------------------------
# LOAD DATA
# ---------------------------------------------------------------------------
print("Loading topic distribution data...")
company_topic = pd.read_csv(COMPANY_TOPIC_CSV)
topic_info = pd.read_csv(TOPIC_INFO_CSV)

# Exclude outlier topic (-1)
company_topic = company_topic[company_topic["topic"] != -1].copy()
topic_info = topic_info[topic_info["Topic"] != -1].copy()

# Label map
topic_label_map = topic_info.set_index("Topic")["Name"].to_dict()

# ---------------------------------------------------------------------------
# SELECT TOP-N TOPICS, GROUP REST AS "OTHER"
# ---------------------------------------------------------------------------
top_topics = (
    topic_info.nlargest(TOP_N_TOPICS, "Count")["Topic"].tolist()
)
print(f"Keeping top {TOP_N_TOPICS} topics, grouping the rest as 'Other'.")

company_topic["topic_group"] = company_topic["topic"].where(
    company_topic["topic"].isin(top_topics), other="Other"
)

grouped = (
    company_topic
    .groupby(["company", "topic_group"])["count"]
    .sum()
    .reset_index()
)

# ---------------------------------------------------------------------------
# PIVOT & NORMALIZE TO PERCENTAGES
# ---------------------------------------------------------------------------
pivot = grouped.pivot_table(
    index="company", columns="topic_group", values="count", fill_value=0
)

# Order firms consistently (only include firms that are actually in the data)
firm_order = [f for f in FIRM_ORDER if f in pivot.index]
pivot = pivot.loc[firm_order]

# Convert to row-wise percentages (each firm sums to 100)
row_totals = pivot.sum(axis=1)
pivot_pct = pivot.div(row_totals, axis=0) * 100

# Order columns by overall size (across all firms), "Other" last
top_cols_sorted = (
    pivot_pct.drop(columns="Other", errors="ignore")
    .sum()
    .sort_values(ascending=False)
    .index.tolist()
)
col_order = top_cols_sorted + (["Other"] if "Other" in pivot_pct.columns else [])
pivot_pct = pivot_pct[col_order]

# Save the percentage table
pivot_pct_out = pivot_pct.round(2).reset_index()
pivot_pct_out.to_csv(OUTPUT_DIR / "company_topic_distribution_pct.csv", index=False)
print(f"  Saved company_topic_distribution_pct.csv")

# ---------------------------------------------------------------------------
# PLOT: HORIZONTAL 100% STACKED BAR
# ---------------------------------------------------------------------------
print("Generating percentage-wise firm × topic chart...")

# Distinct color palette (tab20 + tab20b = 40 colors, ample headroom)
from matplotlib import colormaps
palette = list(colormaps["tab20"].colors) + list(colormaps["tab20b"].colors)


def short_label(label: str, max_len: int = 28) -> str:
    if not isinstance(label, str):
        label = str(label)
    return label if len(label) <= max_len else label[: max_len - 1] + "…"


fig, ax = plt.subplots(figsize=FIGSIZE, dpi=DPI)
left = np.zeros(len(pivot_pct))

for i, col in enumerate(pivot_pct.columns):
    if col == "Other":
        color = OTHER_COLOR
        label_str = "Other topics"
    else:
        color = palette[i % len(palette)]
        label_str = f"{col}: {short_label(topic_label_map.get(col, f'Topic {col}'))}"

    vals = pivot_pct[col].values
    ax.barh(
        pivot_pct.index, vals, left=left,
        color=color, alpha=0.9, label=label_str, height=0.6,
        edgecolor="white", linewidth=0.5,
    )

    # Annotate segments that are big enough to read
    for y_idx, (v, l) in enumerate(zip(vals, left)):
        if v >= 5:  # only label segments >= 5% to avoid clutter
            ax.text(
                l + v / 2, y_idx, f"{v:.0f}%",
                ha="center", va="center",
                fontsize=8, color="white", fontweight="bold",
            )

    left += vals

ax.set_xlim(0, 100)
ax.set_xlabel("Share of firm's climate patent portfolio (%)", fontsize=11)
ax.set_title(
    "Relative Topic Focus by Firm — climate patent portfolio composition",
    fontsize=13, fontweight="bold",
)
ax.invert_yaxis()  # keep FIRM_ORDER reading top-to-bottom
ax.xaxis.set_major_formatter(plt.FuncFormatter(lambda x, _: f"{int(x)}%"))
ax.legend(
    fontsize=7, loc="center left",
    bbox_to_anchor=(1.02, 0.5),
    framealpha=0.9, ncol=1, borderaxespad=0,
)
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)
ax.grid(axis="x", alpha=0.3, linestyle=":")
ax.set_axisbelow(True)

fig.tight_layout()
fig.savefig(
    OUTPUT_DIR / "fig_firm_topic_distribution_pct.png",
    bbox_inches="tight",
)
plt.close(fig)
print(f"  Saved fig_firm_topic_distribution_pct.png")

# ---------------------------------------------------------------------------
# SUMMARY
# ---------------------------------------------------------------------------
print("\nTop topic share per firm:")
for firm in pivot_pct.index:
    top3 = pivot_pct.loc[firm].nlargest(3)
    pretty = ", ".join(
        f"{short_label(topic_label_map.get(t, str(t)), 20)} ({v:.0f}%)"
        for t, v in top3.items()
    )
    print(f"  {firm:<10} → {pretty}")

print("\nDone.")
