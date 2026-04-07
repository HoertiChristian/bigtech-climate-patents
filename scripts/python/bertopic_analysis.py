"""
BERTopic Analysis of Big Tech Climate Patents
==============================================
Runs BERTopic on patent abstracts from patents_raw.json, generates
topic assignments and all planned visualisations for the project.

Methodology follows:
  - Grootendorst (2022), arXiv:2203.05794
  - Yun et al. (2024) on BERTopic for patent landscape analysis

Usage:
  Place this file in the repo root (alongside the data/ folder), then:
    uv run bertopic_analysis.py

  Or with pip:
    pip install bertopic sentence-transformers umap-learn hdbscan \
                scikit-learn pandas matplotlib safetensors
    python bertopic_analysis.py

Inputs  (in ./data/):
  - patents_raw.json         Climate patents by company
  - total_patent_counts.csv  Total patents per company per year (for intensity)

Outputs (in ./bertopic_output/):
  CSVs:
    - topic_info.csv, document_topics.csv, topic_terms.csv
    - topics_over_time.csv, company_topic_distribution.csv, year_topic_counts.csv
  Figures:
    - fig_intensity_evolution.png      Climate Innovation Intensity Index
    - fig_climate_counts_stacked.png   Absolute climate patent counts
    - fig_topic_overview_table.png     Topic Overview Table
    - fig_landscape_map.png            Technological Landscape Map (2D UMAP)
    - fig_topic_streamgraph.png        Topic Relevance Over Time (streamgraph)
    - fig_firm_topic_distribution.png  Firm × topic stacked bar chart
"""

import json
import pandas as pd
import numpy as np
from pathlib import Path
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
from matplotlib.colors import to_rgba

# ---------------------------------------------------------------------------
# 1. CONFIGURATION
# ---------------------------------------------------------------------------
# Resolve paths relative to this script's location,
# so it works whether you run from repo root or the python/ dir.
SCRIPT_DIR = Path(__file__).resolve().parent
DATA_DIR = SCRIPT_DIR / "data"
INPUT_FILE = DATA_DIR / "patents_raw.json"
TOTAL_COUNTS_FILE = DATA_DIR / "total_patent_counts.csv"
OUTPUT_DIR = SCRIPT_DIR / "bertopic_output"
OUTPUT_DIR.mkdir(exist_ok=True)

# =====================================================================
#  PRESET SELECTOR — change this one variable to switch configurations
# =====================================================================
#   "A"  →  Balanced       (~15-30 topics, good default)
#   "B"  →  Granular       (~30-60 topics, maximum splitting)
#   "C"  →  Semantic       (higher-quality embeddings, moderate topics)
#   "D"  →  Forced-30      (forces exactly 30 topics via hierarchical merge)
#
PRESET = "D"
# =====================================================================

PRESETS = {
    "A": {
        "name": "Balanced",
        "embedding_model": "all-MiniLM-L6-v2",
        "umap_n_neighbors": 10,
        "umap_n_components": 10,
        "umap_min_dist": 0.0,
        "hdbscan_min_cluster_size": 12,
        "hdbscan_min_samples": 5,
        "nr_topics": "auto",
    },
    "B": {
        "name": "Granular",
        "embedding_model": "all-MiniLM-L6-v2",
        "umap_n_neighbors": 8,
        "umap_n_components": 15,
        "umap_min_dist": 0.0,
        "hdbscan_min_cluster_size": 8,
        "hdbscan_min_samples": 2,
        "nr_topics": "auto",
    },
    "C": {
        "name": "Semantic (slower, higher quality embeddings)",
        "embedding_model": "all-mpnet-base-v2",
        "umap_n_neighbors": 10,
        "umap_n_components": 10,
        "umap_min_dist": 0.0,
        "hdbscan_min_cluster_size": 10,
        "hdbscan_min_samples": 3,
        "nr_topics": "auto",
    },
    "D": {
        "name": "Forced-30 (discovers topics then merges to 30)",
        "embedding_model": "all-MiniLM-L6-v2",
        "umap_n_neighbors": 8,
        "umap_n_components": 15,
        "umap_min_dist": 0.0,
        "hdbscan_min_cluster_size": 8,
        "hdbscan_min_samples": 2,
        "nr_topics": 30,           # hierarchically merge down to 30
    },
}

cfg = PRESETS[PRESET]

EMBEDDING_MODEL       = cfg["embedding_model"]
UMAP_N_NEIGHBORS      = cfg["umap_n_neighbors"]
UMAP_N_COMPONENTS     = cfg["umap_n_components"]
UMAP_MIN_DIST         = cfg["umap_min_dist"]
UMAP_METRIC           = "cosine"
HDBSCAN_MIN_CLUSTER_SIZE = cfg["hdbscan_min_cluster_size"]
HDBSCAN_MIN_SAMPLES   = cfg["hdbscan_min_samples"]
NR_TOPICS             = cfg["nr_topics"]
TOP_N_WORDS           = 10
SEED                  = 42

# Plotting
COMPANY_COLORS = {
    "Apple":     "#3266ad",
    "Microsoft": "#1D9E75",
    "Amazon":    "#D85A30",
    "Meta":      "#D4537E",
    "Alphabet":  "#7F77DD",
}
POLICY_EVENTS = [
    (2015, "Paris Agreement"),
    (2019, "EU Green Deal"),
    (2022, "US IRA"),
]
FIGSIZE = (12, 6)
DPI = 200

# ---------------------------------------------------------------------------
# PATENT BOILERPLATE CLEANING
# ---------------------------------------------------------------------------
import re

# Patent abstracts share heavy legal/structural language that makes
# embeddings cluster together regardless of actual technical content.
# Stripping this lets the model focus on the substantive technology.
PATENT_BOILERPLATE = re.compile(
    r"\b("
    r"present invention|present disclosure|various embodiments|"
    r"one or more embodiments|in accordance with|"
    r"according to (an|the|one|some|various|certain) (aspect|embodiment|implementation|example)s?|"
    r"methods? and (systems?|apparatus|devices?)|"
    r"systems? and methods?|apparatus and methods?|"
    r"disclosed herein|described herein|provided herein|"
    r"in some embodiments?|in certain embodiments?|in one embodiment|"
    r"at least one (of|processor|memory|non-transitory)|"
    r"non-transitory (computer|machine)[- ]readable (storage )?medium|"
    r"one or more processors?|"
    r"computer[- ]implemented method|"
    r"computer[- ]readable (storage )?medium|"
    r"are also disclosed|is also disclosed|"
    r"the method (further )?(includes?|comprises?)|"
    r"configured to|operable to|adapted to|"
    r"a plurality of|the plurality of|"
    r"(first|second|third) (device|apparatus|system|method|signal|component)"
    r")\b",
    re.IGNORECASE,
)

FILLER_PHRASES = re.compile(
    r"\b(may be|can be|is configured to|are configured to|"
    r"may include|may comprise|comprising|comprises|"
    r"thereof|therein|thereto|thereby|wherein|whereby)\b",
    re.IGNORECASE,
)


def clean_abstract(text: str) -> str:
    """Strip patent boilerplate to let technical content drive clustering."""
    text = PATENT_BOILERPLATE.sub(" ", text)
    text = FILLER_PHRASES.sub(" ", text)
    text = re.sub(r"\s{2,}", " ", text).strip()
    return text

# ---------------------------------------------------------------------------
# 2. LOAD & PREPARE DATA
# ---------------------------------------------------------------------------
print("=" * 60)
print("BERTopic Patent Analysis Pipeline")
print(f"  Preset {PRESET}: {cfg['name']}")
print("=" * 60)

print("\n[1/8] Loading patent data...")
with open(INPUT_FILE, "r", encoding="utf-8") as f:
    raw = json.load(f)

records = []
for company, patents in raw.items():
    for p in patents:
        abstract_text = ""
        if p.get("abstract"):
            for ab in p["abstract"]:
                if ab.get("text"):
                    abstract_text = ab["text"].strip()
                    if ab.get("lang", "en") == "en":
                        break

        title = ""
        titles = p.get("biblio", {}).get("invention_title", [])
        for t in titles:
            if t.get("text"):
                title = t["text"].strip()
                if t.get("lang", "en") == "en":
                    break

        date_pub = p.get("date_published", "")
        year = int(date_pub[:4]) if date_pub and len(date_pub) >= 4 else None

        cpc_codes = []
        cpc_data = (
            p.get("biblio", {})
            .get("classifications_cpc", {})
            .get("classifications", [])
        )
        for c in cpc_data:
            sym = c.get("symbol", "")
            if sym.startswith("Y02") or sym.startswith("Y04S"):
                cpc_codes.append(sym)

        records.append({
            "lens_id": p.get("lens_id", ""),
            "company": company,
            "year": year,
            "title": title,
            "abstract": abstract_text,
            "cpc_climate_codes": "; ".join(cpc_codes),
            "n_climate_codes": len(cpc_codes),
        })

df = pd.DataFrame(records)
print(f"  Total patents loaded: {len(df)}")
print(f"  Per company:")
for comp, count in df["company"].value_counts().items():
    n_abs = (df[df["company"] == comp]["abstract"].str.len() > 20).sum()
    print(f"    {comp}: {count} total, {n_abs} with abstracts")

# Filter to patents with usable abstracts
df = df[df["abstract"].str.len() > 20].copy()
df = df.reset_index(drop=True)

# Clean patent boilerplate from abstracts for better topic separation
df["abstract_clean"] = df["abstract"].apply(clean_abstract)
# Drop any that became too short after cleaning
df = df[df["abstract_clean"].str.len() > 20].copy()
df = df.reset_index(drop=True)

print(f"\n  Corpus size: {len(df)} patents with usable abstracts")
print(f"  Year range: {df['year'].min()} – {df['year'].max()}")

# ---------------------------------------------------------------------------
# 3. CLIMATE INNOVATION INTENSITY INDEX (RQ1 visualisations)
# ---------------------------------------------------------------------------
print("\n[2/8] Computing Climate Innovation Intensity Index...")

# Climate patents per company per year
climate_yearly = (
    pd.DataFrame(records)
    .groupby(["company", "year"])
    .size()
    .reset_index(name="climate_patents")
)

# Total patents per company per year
total_yearly = pd.read_csv(TOTAL_COUNTS_FILE)
total_yearly.columns = [c.strip() for c in total_yearly.columns]

intensity = climate_yearly.merge(
    total_yearly, on=["company", "year"], how="left"
)
intensity["intensity"] = np.where(
    intensity["total_patents"] > 0,
    intensity["climate_patents"] / intensity["total_patents"] * 100,
    np.nan,
)

# --- Fig 1: Climate Innovation Intensity Evolution ---
fig, ax = plt.subplots(figsize=FIGSIZE, dpi=DPI)
companies_in_data = [c for c in COMPANY_COLORS if c in intensity["company"].unique()]

for comp in companies_in_data:
    sub = intensity[intensity["company"] == comp].sort_values("year")
    ax.plot(
        sub["year"], sub["intensity"],
        color=COMPANY_COLORS[comp], linewidth=2.2, marker="o",
        markersize=4, label=comp, zorder=3,
    )

for yr, label in POLICY_EVENTS:
    ax.axvline(yr, color="gray", linestyle="--", linewidth=0.8, alpha=0.6)
    ax.text(yr + 0.15, ax.get_ylim()[1] * 0.95, label,
            fontsize=8, color="gray", va="top", rotation=0)

ax.set_xlabel("Year", fontsize=11)
ax.set_ylabel("Climate patent share (%)", fontsize=11)
ax.set_title("Climate Innovation Intensity Index (2010–2024)", fontsize=13, fontweight="bold")
ax.legend(fontsize=9, framealpha=0.9)
ax.set_xlim(2009.5, 2024.5)
ax.xaxis.set_major_locator(mticker.MaxNLocator(integer=True))
ax.grid(axis="y", alpha=0.3)
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)
fig.tight_layout()
fig.savefig(OUTPUT_DIR / "fig_intensity_evolution.png")
plt.close(fig)
print("  Saved fig_intensity_evolution.png")

# --- Fig 2: Absolute climate patent counts (stacked bar) ---
fig, ax = plt.subplots(figsize=FIGSIZE, dpi=DPI)
all_years = sorted(intensity["year"].dropna().unique().astype(int))
bottom = np.zeros(len(all_years))

for comp in companies_in_data:
    sub = intensity[intensity["company"] == comp].set_index("year").reindex(all_years).fillna(0)
    vals = sub["climate_patents"].values
    ax.bar(all_years, vals, bottom=bottom, label=comp,
           color=COMPANY_COLORS[comp], alpha=0.85, width=0.75)
    bottom += vals

for yr, label in POLICY_EVENTS:
    ax.axvline(yr, color="gray", linestyle="--", linewidth=0.8, alpha=0.6)

ax.set_xlabel("Year", fontsize=11)
ax.set_ylabel("Climate patents (Y02/Y04S)", fontsize=11)
ax.set_title("Climate Patent Counts by Company (2010–2024)", fontsize=13, fontweight="bold")
ax.legend(fontsize=9, framealpha=0.9)
ax.xaxis.set_major_locator(mticker.MaxNLocator(integer=True))
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)
fig.tight_layout()
fig.savefig(OUTPUT_DIR / "fig_climate_counts_stacked.png")
plt.close(fig)
print("  Saved fig_climate_counts_stacked.png")

# ---------------------------------------------------------------------------
# 4. CONFIGURE & RUN BERTOPIC
# ---------------------------------------------------------------------------
print("\n[3/8] Initialising BERTopic components...")

from sentence_transformers import SentenceTransformer
from umap import UMAP
from hdbscan import HDBSCAN
from sklearn.feature_extraction.text import CountVectorizer
from bertopic import BERTopic
from bertopic.vectorizers import ClassTfidfTransformer
from bertopic.representation import KeyBERTInspired

embedding_model = SentenceTransformer(EMBEDDING_MODEL)

umap_model = UMAP(
    n_neighbors=UMAP_N_NEIGHBORS,
    n_components=UMAP_N_COMPONENTS,
    min_dist=UMAP_MIN_DIST,
    metric=UMAP_METRIC,
    random_state=SEED,
)

hdbscan_model = HDBSCAN(
    min_cluster_size=HDBSCAN_MIN_CLUSTER_SIZE,
    min_samples=HDBSCAN_MIN_SAMPLES,
    metric="euclidean",
    prediction_data=True,
)

vectorizer_model = CountVectorizer(
    stop_words="english",
    min_df=2,
    max_df=0.95,
    ngram_range=(1, 2),
    token_pattern=r"(?u)\b[a-zA-Z][a-zA-Z0-9]{1,}\b",
)

ctfidf_model = ClassTfidfTransformer(reduce_frequent_words=True)
representation_model = KeyBERTInspired()

topic_model = BERTopic(
    embedding_model=embedding_model,
    umap_model=umap_model,
    hdbscan_model=hdbscan_model,
    vectorizer_model=vectorizer_model,
    ctfidf_model=ctfidf_model,
    representation_model=representation_model,
    top_n_words=TOP_N_WORDS,
    nr_topics=NR_TOPICS,
    verbose=True,
    calculate_probabilities=False,
)

print("\n[4/8] Fitting BERTopic model (this may take a few minutes)...")
docs = df["abstract_clean"].tolist()

# Pre-compute embeddings so we can reuse them for the 2D landscape map
print("  Computing document embeddings...")
embeddings = embedding_model.encode(docs, show_progress_bar=True)

topics, probs = topic_model.fit_transform(docs, embeddings=embeddings)
df["topic"] = topics

n_topics = len(set(topics)) - (1 if -1 in topics else 0)
n_outliers = (df["topic"] == -1).sum()
print(f"\n  Topics discovered: {n_topics}")
print(f"  Outlier documents (topic -1): {n_outliers} ({n_outliers/len(df)*100:.1f}%)")

# ---------------------------------------------------------------------------
# 5. EXTRACT & SAVE CSV OUTPUTS
# ---------------------------------------------------------------------------
print("\n[5/8] Saving CSV outputs...")

topic_info = topic_model.get_topic_info()
topic_info.to_csv(OUTPUT_DIR / "topic_info.csv", index=False)
print(f"  topic_info.csv ({len(topic_info)} topics)")

df.to_csv(OUTPUT_DIR / "document_topics.csv", index=False)
print(f"  document_topics.csv ({len(df)} rows)")

topic_terms = []
for topic_id in topic_model.get_topics():
    if topic_id == -1:
        continue
    words = topic_model.get_topic(topic_id)
    for rank, (word, score) in enumerate(words):
        topic_terms.append({
            "topic": topic_id,
            "rank": rank + 1,
            "word": word,
            "score": round(score, 5),
        })
topic_terms_df = pd.DataFrame(topic_terms)
topic_terms_df.to_csv(OUTPUT_DIR / "topic_terms.csv", index=False)
print(f"  topic_terms.csv ({len(topic_terms_df)} rows)")

# Build label map
topic_label_map = topic_info.set_index("Topic")["Name"].to_dict()

# Aggregated tables
topics_over_time = (
    df[df["topic"] != -1]
    .groupby(["company", "year", "topic"])
    .size()
    .reset_index(name="count")
)
topics_over_time["topic_label"] = topics_over_time["topic"].map(topic_label_map)
topics_over_time.to_csv(OUTPUT_DIR / "topics_over_time.csv", index=False)
print(f"  topics_over_time.csv ({len(topics_over_time)} rows)")

company_topic = (
    df[df["topic"] != -1]
    .groupby(["company", "topic"])
    .agg(count=("lens_id", "size"))
    .reset_index()
)
company_topic["topic_label"] = company_topic["topic"].map(topic_label_map)
company_topic.to_csv(OUTPUT_DIR / "company_topic_distribution.csv", index=False)
print(f"  company_topic_distribution.csv")

year_topic = (
    df[df["topic"] != -1]
    .groupby(["year", "topic"])
    .size()
    .reset_index(name="count")
)
year_topic["topic_label"] = year_topic["topic"].map(topic_label_map)
year_topic.to_csv(OUTPUT_DIR / "year_topic_counts.csv", index=False)
print(f"  year_topic_counts.csv")

# ---------------------------------------------------------------------------
# 6. GENERATE TOPIC VISUALISATIONS (RQ2)
# ---------------------------------------------------------------------------
print("\n[6/8] Generating topic visualisations...")

# Colour palette for topics (up to 25 distinct colours)
TOPIC_CMAP = plt.cm.get_cmap("tab20", max(20, n_topics))


def short_label(label: str, max_len: int = 35) -> str:
    """Truncate a BERTopic label for plot readability."""
    if len(label) <= max_len:
        return label
    return label[:max_len - 1] + "…"


# --- Fig 3: Topic Overview Table ---
# Top N topics by size, showing rank, label, count, and top-5 keywords
top_n_for_table = min(20, n_topics)
table_topics = topic_info[topic_info["Topic"] != -1].head(top_n_for_table)

fig, ax = plt.subplots(
    figsize=(14, 0.5 * top_n_for_table + 1.8), dpi=DPI
)
ax.axis("off")

table_data = []
for _, row in table_topics.iterrows():
    tid = row["Topic"]
    words = topic_model.get_topic(tid)
    top5 = ", ".join(w for w, _ in words[:5])
    table_data.append([
        f"Topic {tid}",
        short_label(row["Name"], 40),
        str(row["Count"]),
        top5,
    ])

table = ax.table(
    cellText=table_data,
    colLabels=["Topic", "Label", "Patents", "Top keywords"],
    loc="center",
    cellLoc="left",
    colWidths=[0.08, 0.30, 0.08, 0.54],
)
table.auto_set_font_size(False)
table.set_fontsize(9)
table.scale(1, 1.4)

# Style header
for j in range(4):
    cell = table[0, j]
    cell.set_text_props(fontweight="bold")
    cell.set_facecolor("#e8e8e8")

# Alternate row shading
for i in range(1, len(table_data) + 1):
    for j in range(4):
        cell = table[i, j]
        if i % 2 == 0:
            cell.set_facecolor("#f7f7f7")
        else:
            cell.set_facecolor("white")

ax.set_title(
    f"Topic Overview — top {top_n_for_table} topics by patent count",
    fontsize=13, fontweight="bold", pad=12,
)
fig.tight_layout()
fig.savefig(OUTPUT_DIR / "fig_topic_overview_table.png", bbox_inches="tight")
plt.close(fig)
print("  Saved fig_topic_overview_table.png")


# --- Fig 4: Technological Landscape Map (2D UMAP scatter) ---
print("  Computing 2D UMAP projection for landscape map...")
umap_2d = UMAP(
    n_neighbors=15,
    n_components=2,
    min_dist=0.3,
    metric="cosine",
    random_state=SEED,
)

# Re-use the pre-computed embeddings from step 4
coords_2d = umap_2d.fit_transform(embeddings)

fig, ax = plt.subplots(figsize=(12, 9), dpi=DPI)

# Plot outliers faintly
outlier_mask = df["topic"] == -1
ax.scatter(
    coords_2d[outlier_mask, 0], coords_2d[outlier_mask, 1],
    c="lightgray", s=6, alpha=0.25, label="Outliers", zorder=1,
)

# Plot each topic
unique_topics = sorted([t for t in df["topic"].unique() if t != -1])
for tid in unique_topics:
    mask = df["topic"] == tid
    color = TOPIC_CMAP(tid % 20)
    label_str = short_label(topic_label_map.get(tid, f"Topic {tid}"), 30)
    ax.scatter(
        coords_2d[mask, 0], coords_2d[mask, 1],
        c=[color], s=12, alpha=0.55, label=label_str, zorder=2,
    )

ax.set_xlabel("UMAP 1", fontsize=11)
ax.set_ylabel("UMAP 2", fontsize=11)
ax.set_title("Technological Landscape Map — Climate Patent Clusters", fontsize=13, fontweight="bold")

# Legend: show top 15 topics only to keep it readable
handles, labels = ax.get_legend_handles_labels()
max_legend = min(16, len(handles))  # outliers + 15 topics
ax.legend(
    handles[:max_legend], labels[:max_legend],
    fontsize=7, loc="upper right", framealpha=0.9,
    ncol=1, markerscale=2,
)
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)
fig.tight_layout()
fig.savefig(OUTPUT_DIR / "fig_landscape_map.png")
plt.close(fig)
print("  Saved fig_landscape_map.png")


# --- Fig 5: Topic Relevance Over Time (streamgraph / stacked area) ---
# Take top N topics for readability
top_n_stream = min(12, n_topics)
top_topics_by_size = (
    topic_info[topic_info["Topic"] != -1]
    .nlargest(top_n_stream, "Count")["Topic"]
    .tolist()
)

stream_data = year_topic[year_topic["topic"].isin(top_topics_by_size)].copy()
stream_pivot = stream_data.pivot_table(
    index="year", columns="topic", values="count", fill_value=0
)
# Reorder columns by total size descending
col_order = stream_pivot.sum().sort_values(ascending=False).index
stream_pivot = stream_pivot[col_order]

fig, ax = plt.subplots(figsize=FIGSIZE, dpi=DPI)
colors = [TOPIC_CMAP(tid % 20) for tid in stream_pivot.columns]
labels = [short_label(topic_label_map.get(tid, f"T{tid}"), 25) for tid in stream_pivot.columns]

ax.stackplot(
    stream_pivot.index, *[stream_pivot[c].values for c in stream_pivot.columns],
    labels=labels, colors=colors, alpha=0.8,
)

for yr, label in POLICY_EVENTS:
    if yr in stream_pivot.index:
        ax.axvline(yr, color="black", linestyle="--", linewidth=0.9, alpha=0.5)
        ax.text(yr + 0.15, ax.get_ylim()[1] * 0.97, label,
                fontsize=8, color="black", va="top")

ax.set_xlabel("Year", fontsize=11)
ax.set_ylabel("Number of patents", fontsize=11)
ax.set_title("Topic Relevance Over Time — Top Topics (streamgraph)", fontsize=13, fontweight="bold")
ax.legend(fontsize=7, loc="upper left", framealpha=0.9, ncol=2)
ax.xaxis.set_major_locator(mticker.MaxNLocator(integer=True))
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)
fig.tight_layout()
fig.savefig(OUTPUT_DIR / "fig_topic_streamgraph.png")
plt.close(fig)
print("  Saved fig_topic_streamgraph.png")


# --- Fig 6: Firm × topic distribution (stacked bar) ---
top_n_firm = min(15, n_topics)
top_topics_firm = (
    topic_info[topic_info["Topic"] != -1]
    .nlargest(top_n_firm, "Count")["Topic"]
    .tolist()
)

firm_data = company_topic[company_topic["topic"].isin(top_topics_firm)].copy()
firm_pivot = firm_data.pivot_table(
    index="company", columns="topic", values="count", fill_value=0
)
firm_pivot = firm_pivot[
    firm_pivot.sum().sort_values(ascending=False).index
]

fig, ax = plt.subplots(figsize=(14, 6), dpi=DPI)
bottom = np.zeros(len(firm_pivot))

for i, tid in enumerate(firm_pivot.columns):
    color = TOPIC_CMAP(tid % 20)
    label_str = short_label(topic_label_map.get(tid, f"T{tid}"), 25)
    ax.barh(
        firm_pivot.index, firm_pivot[tid].values, left=bottom,
        color=color, alpha=0.85, label=label_str, height=0.6,
    )
    bottom += firm_pivot[tid].values

ax.set_xlabel("Number of patents", fontsize=11)
ax.set_title("Climate Patent Topics by Company", fontsize=13, fontweight="bold")
ax.legend(fontsize=7, loc="lower right", framealpha=0.9, ncol=2)
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)
fig.tight_layout()
fig.savefig(OUTPUT_DIR / "fig_firm_topic_distribution.png")
plt.close(fig)
print("  Saved fig_firm_topic_distribution.png")

# ---------------------------------------------------------------------------
# 7. SUMMARY PRINTOUT
# ---------------------------------------------------------------------------
print("\n[7/8] Topic overview")
print("=" * 60)
print(f"{'Topic':>6}  {'Count':>6}  {'Label'}")
print("-" * 60)
for _, row in topic_info.iterrows():
    label = "OUTLIERS" if row["Topic"] == -1 else row["Name"]
    print(f"{row['Topic']:>6}  {row['Count']:>6}  {label}")

# ---------------------------------------------------------------------------
# 8. SAVE MODEL
# ---------------------------------------------------------------------------
print("\n[8/8] Saving model...")
try:
    topic_model.save(
        str(OUTPUT_DIR / "bertopic_model"),
        serialization="safetensors",
        save_ctfidf=True,
        save_embedding_model=EMBEDDING_MODEL,
    )
    print(f"  Model saved to {OUTPUT_DIR / 'bertopic_model'}")
except Exception as e:
    try:
        topic_model.save(
            str(OUTPUT_DIR / "bertopic_model"), serialization="pickle"
        )
        print(f"  Model saved (pickle) to {OUTPUT_DIR / 'bertopic_model'}")
    except Exception as e2:
        print(f"  Note: model save skipped ({e2}). CSV outputs are complete.")

# ---------------------------------------------------------------------------
# DONE
# ---------------------------------------------------------------------------
print("\n" + "=" * 60)
print("All outputs in:", OUTPUT_DIR.resolve())
print()
print("  CSVs:")
print("    topic_info.csv, document_topics.csv, topic_terms.csv")
print("    topics_over_time.csv, company_topic_distribution.csv")
print("    year_topic_counts.csv")
print()
print("  Figures:")
print("    fig_intensity_evolution.png       RQ1 — Intensity line chart")
print("    fig_climate_counts_stacked.png    RQ1 — Absolute patent counts")
print("    fig_topic_overview_table.png      RQ2 — Topic Overview Table")
print("    fig_landscape_map.png             RQ2 — Technological Landscape Map")
print("    fig_topic_streamgraph.png         RQ2 — Topic Relevance Over Time")
print("    fig_firm_topic_distribution.png   RQ2 — Firm × topic breakdown")
print("=" * 60)
print("\nDone.")