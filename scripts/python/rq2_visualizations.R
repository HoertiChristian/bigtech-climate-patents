# =============================================================================
#  RQ2 visualisations  —  Big Tech Climate Patents
#  Figures 3 – 7.  All figures use ggplot2 / tidyverse.
#
#  Expected inputs (from the BERTopic pipeline, in ../bertopic_output/ by default):
#    - document_topics.csv              # one row per patent
#    - topic_info.csv                   # topic id / name / size
#    - topic_terms.csv                  # topic × rank × word × c-TF-IDF score
#    - topics_over_time.csv             # company × year × topic counts
#    - company_topic_distribution.csv   # company × topic counts
#    - year_topic_counts.csv            # year × topic counts
#    - topic_cpc_composition.csv        # topic × Y-subclass percentages (for Fig 7)
#
#  Outputs: figures/fig3_keyword_prevalence.png ... fig7_cpc_heatmap.png
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(stringr)
  library(tidytext)       # reorder_within / scale_x_reordered (Fig 3 facets)
  library(RColorBrewer)   # Fig 8 palette
})

# ---- Paths -------------------------------------------------------------------
# Adjust these two paths to your local layout and run from the project root.
PROJECT_ROOT <- getwd()
INPUT_DIR    <- file.path(PROJECT_ROOT, "bertopic_output")
OUTPUT_DIR   <- file.path(PROJECT_ROOT, "figures")

if (!dir.exists(INPUT_DIR)) {
  stop("Input directory not found: ", INPUT_DIR,
       "\nCurrent working directory is: ", PROJECT_ROOT)
}
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- Shared theme ------------------------------------------------------------
theme_report <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(color = "grey30", size = 10),
      axis.title       = element_text(size = 10),
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold", size = 10),
      legend.position  = "bottom"
    )
}

COMPANY_COLORS <- c(
  Alphabet  = "#4285F4",
  Amazon    = "#FF9900",
  Apple     = "#A2AAAD",
  Meta      = "#1877F2",
  Microsoft = "#00A4EF"
)

POLICY_EVENTS <- tibble(
  year  = c(2015, 2017, 2022),
  label = c("Paris Agreement", "US withdrawal", "IRA")
)

# ---- Load data ---------------------------------------------------------------
docs          <- read_csv(file.path(INPUT_DIR, "document_topics.csv"),            show_col_types = FALSE)
topic_info    <- read_csv(file.path(INPUT_DIR, "topic_info.csv"),                 show_col_types = FALSE)
topic_terms   <- read_csv(file.path(INPUT_DIR, "topic_terms.csv"),                show_col_types = FALSE)
year_topic    <- read_csv(file.path(INPUT_DIR, "year_topic_counts.csv"),          show_col_types = FALSE)
company_topic <- read_csv(file.path(INPUT_DIR, "company_topic_distribution.csv"), show_col_types = FALSE)
tot           <- read_csv(file.path(INPUT_DIR, "topics_over_time.csv"),           show_col_types = FALSE)
cpc_comp      <- read_csv(file.path(INPUT_DIR, "topic_cpc_composition.csv"),      show_col_types = FALSE)

# topic_info from BERTopic has columns Topic / Count / Name by default — normalise
topic_info <- topic_info %>%
  rename_with(tolower) %>%
  select(topic = topic, count = count, name = name)

# helper: pick the FIRST keyword after the topic id as the short label.
# BERTopic auto-names look like "12_battery_thermal_management_pack" — this
# returns "battery". You can hard-code nicer names later by building a named
# vector and rebinding topic_info$short_label before each figure block.
shorten_label <- function(name, max_words = 1) {
  parts <- str_split(name, "_")[[1]]
  parts <- parts[-1]  # drop leading topic id
  paste(head(parts, max_words), collapse = " ")
}
topic_info <- topic_info %>%
  mutate(short_label = map_chr(name, shorten_label))

# exclude the HDBSCAN outlier bin globally
topic_info_sub <- topic_info %>% filter(topic != -1)
docs_sub       <- docs       %>% filter(topic != -1)


# =============================================================================
# FIGURE 3 — Keyword prevalence for top-5 topics (faceted bar chart)
# =============================================================================
# For each of the 5 largest topics, compute the % of patents in that topic
# whose abstract literally contains each of that topic's top-10 c-TF-IDF words.
# This validates topic labels against the underlying corpus: high prevalence =
# the keyword actually characterises the topic, not just a distinctive artefact.
# -----------------------------------------------------------------------------

TOP_N_TOPICS   <- 5
TOP_N_KEYWORDS <- 10

# pick top 5 topics by patent count (excluding -1)
top5_topics <- topic_info_sub %>%
  slice_max(count, n = TOP_N_TOPICS) %>%
  pull(topic)

# top 10 c-TF-IDF keywords for each of those topics
top5_keywords <- topic_terms %>%
  filter(topic %in% top5_topics) %>%
  group_by(topic) %>%
  slice_min(rank, n = TOP_N_KEYWORDS) %>%
  ungroup() %>%
  select(topic, word)

# compute prevalence (keyword appears at least once in the abstract)
# vectorised, case-insensitive, whole-word match
compute_prevalence <- function(topic_id, words, docs_df) {
  abstracts <- docs_df %>%
    filter(topic == topic_id) %>%
    pull(abstract) %>%
    str_to_lower()
  n_docs <- length(abstracts)
  if (n_docs == 0) return(tibble(word = words, prevalence = 0, n_docs = 0))
  
  prev <- map_dbl(words, function(w) {
    # unigrams and bigrams both need word-boundary-safe matching
    pattern <- paste0("\\b", str_replace_all(str_to_lower(w), " ", "\\\\s+"), "\\b")
    mean(str_detect(abstracts, pattern))
  })
  tibble(word = words, prevalence = prev, n_docs = n_docs)
}

prev_tbl <- top5_keywords %>%
  group_by(topic) %>%
  summarise(words = list(word), .groups = "drop") %>%
  mutate(res = map2(topic, words, ~ compute_prevalence(.x, .y, docs_sub))) %>%
  select(topic, res) %>%
  unnest(res) %>%
  filter(prevalence > 0) %>%     # drop keywords absent from every abstract in the topic
  left_join(topic_info_sub %>% select(topic, short_label, count), by = "topic") %>%
  mutate(facet_label = sprintf("T%d · %s  (N=%d)", topic, short_label, count))

# order topics by size (descending) in the facets, order words within each
# facet by prevalence
prev_tbl <- prev_tbl %>%
  mutate(facet_label = fct_reorder(facet_label, -count),
         word        = reorder_within(word, prevalence, facet_label))

# cap x-axis at ~10% above the largest bar so short bars are readable
x_upper <- max(prev_tbl$prevalence, na.rm = TRUE) * 1.15

fig3 <- ggplot(prev_tbl, aes(x = word, y = prevalence, fill = prevalence)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = percent(prevalence, accuracy = 1)),
            hjust = -0.15, size = 3.2) +
  scale_x_reordered() +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, x_upper),
                     expand = c(0, 0)) +
  scale_fill_gradient(low = "#cfe2f3", high = "#1f4e79", guide = "none") +
  coord_flip() +
  facet_wrap(~ facet_label, scales = "free_y", ncol = 2) +
  labs(
    title    = "Keyword prevalence within the five largest topics",
    subtitle = "Share of patents in each topic whose abstract contains the keyword",
    x = NULL, y = "Share of patents in topic"
  ) +
  theme_report() +
  theme(
    axis.text.y = element_text(size = 9),
    strip.text  = element_text(face = "bold", size = 10,
                               margin = margin(t = 2, b = 2))
  )

ggsave(file.path(OUTPUT_DIR, "fig3_keyword_prevalence.png"),
       fig3, width = 11, height = 8, dpi = 300, bg = "white")
message("Saved fig3_keyword_prevalence.png")


# =============================================================================
# FIGURE 4 — Topics over time (stacked area, top topics + other)
# =============================================================================
# Shows whether the composition of climate-patent topics has shifted over time,
# with policy-event reference lines to connect shifts to external milestones.
# -----------------------------------------------------------------------------

TOP_N_STREAM <- 8

top_stream_topics <- topic_info_sub %>%
  slice_max(count, n = TOP_N_STREAM) %>%
  pull(topic)

stream_df <- year_topic %>%
  filter(topic != -1, year >= 2010, year <= 2024) %>%
  mutate(
    topic_group = if_else(topic %in% top_stream_topics, as.character(topic), "other")
  ) %>%
  group_by(year, topic_group) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  left_join(
    topic_info_sub %>%
      mutate(topic = as.character(topic)) %>%
      select(topic, short_label),
    by = c("topic_group" = "topic")
  ) %>%
  mutate(
    display_label = if_else(topic_group == "other", "Other topics", short_label)
  )

# order legend by total size (descending), 'Other' last
legend_order <- stream_df %>%
  group_by(display_label) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(display_label)
legend_order <- c(setdiff(legend_order, "Other topics"), "Other topics")

stream_df <- stream_df %>%
  mutate(display_label = factor(display_label, levels = legend_order))

fig4 <- ggplot(stream_df, aes(x = year, y = count, fill = display_label)) +
  geom_area(alpha = 0.85, colour = "white", linewidth = 0.2) +
  geom_vline(data = POLICY_EVENTS, aes(xintercept = year),
             linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_text(data = POLICY_EVENTS,
            aes(x = year, y = Inf, label = label),
            inherit.aes = FALSE, vjust = 1.5, hjust = -0.05,
            size = 3, colour = "grey30") +
  scale_x_continuous(breaks = seq(2010, 2024, 2), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set2", name = NULL) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title    = "Topic composition of Big Tech climate patents, 2010–2024",
    subtitle = "Stacked count of patents per topic; top 8 topics shown, remainder grouped as 'Other'",
    x = NULL, y = "Number of patents"
  ) +
  theme_report() +
  theme(
    legend.text   = element_text(size = 9),
    legend.key.size = unit(0.5, "cm"),
    plot.margin   = margin(10, 15, 10, 10)
  )

ggsave(file.path(OUTPUT_DIR, "fig4_topics_over_time.png"),
       fig4, width = 11, height = 6.5, dpi = 300, bg = "white")
message("Saved fig4_topics_over_time.png")


# =============================================================================
# FIGURE 5 — Firm × topic heatmap (row-normalised)
# =============================================================================
# Each row is a firm; each column is one of the top topics. Cell value = share
# of that firm's climate patents in that topic. Highlights firm specialisation.
# -----------------------------------------------------------------------------

TOP_N_HEAT <- 15

top_heat_topics <- topic_info_sub %>%
  slice_max(count, n = TOP_N_HEAT) %>%
  pull(topic)

firm_totals <- company_topic %>%
  filter(topic != -1) %>%
  group_by(company) %>%
  summarise(firm_total = sum(count), .groups = "drop")

heat_df <- company_topic %>%
  filter(topic %in% top_heat_topics) %>%
  left_join(firm_totals, by = "company") %>%
  left_join(topic_info_sub %>% select(topic, short_label, count), by = "topic") %>%
  mutate(share = count.x / firm_total)  # count.x is firm-topic count after join

# order topics by overall size, firms alphabetical (or keep original order)
topic_order <- topic_info_sub %>%
  filter(topic %in% top_heat_topics) %>%
  arrange(desc(count)) %>%
  mutate(topic_label = sprintf("T%d · %s", topic, short_label)) %>%
  pull(topic_label)

heat_df <- heat_df %>%
  mutate(topic_label = sprintf("T%d · %s", topic, short_label),
         topic_label = factor(topic_label, levels = rev(topic_order)),
         company     = factor(company, levels = names(COMPANY_COLORS)))

fig5 <- ggplot(heat_df, aes(x = company, y = topic_label, fill = share)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = percent(share, accuracy = 1),
                colour = share > 0.15),
            size = 3) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b",
                      labels = percent_format(accuracy = 1),
                      name = "Share of firm's\nclimate patents") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey15"),
                      guide = "none") +
  labs(
    title    = "Firm specialisation across climate topics",
    subtitle = sprintf("Row-normalised share of each firm's climate patents in the top %d topics", TOP_N_HEAT),
    x = NULL, y = NULL
  ) +
  theme_report() +
  theme(
    panel.grid  = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 10, face = "bold")
  )

ggsave(file.path(OUTPUT_DIR, "fig5_firm_topic_heatmap.png"),
       fig5, width = 10, height = 8, dpi = 300, bg = "white")
message("Saved fig5_firm_topic_heatmap.png")


# =============================================================================
# FIGURE 6 — Firm portfolio composition over time (faceted stacked area)
# =============================================================================
# One panel per firm; inside each panel, stacked area of topic share over time.
# Shows how each firm's climate-patent portfolio has reorganised itself.
#
# Design choices:
#
# (1) Smoothing. Firm-year patent counts are small for Meta and Amazon (often
#     10–30 per year), which means raw per-year shares are dominated by
#     sampling noise — a single patent moving between topics can shift a share
#     by 10 percentage points. We apply a trailing 3-year rolling SUM to both
#     the numerator (topic count) and the denominator (firm total), then
#     compute the share from those smoothed counts. This stabilises the
#     visual while preserving the time axis; it is the standard approach in
#     patent-landscaping papers for small-firm cohorts.
#
# (2) No "Other" bucket. Including "Other" compresses the top-N topics into a
#     narrow top strip because "Other" averages 40–55% of each portfolio. The
#     y-axis here caps at whatever the top-N topics collectively reach.
#
# (3) Firm-years with fewer than MIN_PATENTS_PER_YEAR patents in the rolling
#     window are dropped; with a 3-year window the threshold is lower than
#     for the raw version.
# -----------------------------------------------------------------------------

TOP_N_FIRM_STREAM    <- 6   # number of topics shown per panel
ROLL_WINDOW          <- 3   # trailing-years window for the rolling sum
MIN_PATENTS_PER_WIN  <- 10   # drop firm-years where the rolling total is below this

top_firm_stream_topics <- topic_info_sub %>%
  slice_max(count, n = TOP_N_FIRM_STREAM) %>%
  pull(topic)

# helper: trailing rolling sum of length `w` over a numeric vector ordered by year.
# Early years (where fewer than `w` years are available) use the partial window.
roll_sum_trailing <- function(x, w) {
  n <- length(x)
  vapply(seq_len(n), function(i) sum(x[max(1, i - w + 1):i]), numeric(1))
}

# Step 1: build a COMPLETE (company × year × topic) grid with zeros, so the
# rolling sum sees every year even when a topic had no filings that year.
all_years <- seq(2010, 2024)
grid <- expand_grid(
  company = unique(tot$company),
  year    = all_years,
  topic   = top_firm_stream_topics
)

firm_topic_counts <- tot %>%
  filter(topic != -1, topic %in% top_firm_stream_topics,
         year >= min(all_years), year <= max(all_years)) %>%
  group_by(company, year, topic) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  right_join(grid, by = c("company", "year", "topic")) %>%
  mutate(count = coalesce(count, 0))

# Step 2: firm-year TOTAL counts (across all topics, not just top N) for denom.
firm_year_totals <- tot %>%
  filter(topic != -1, year >= min(all_years), year <= max(all_years)) %>%
  group_by(company, year) %>%
  summarise(firm_year_total = sum(count), .groups = "drop") %>%
  right_join(
    expand_grid(company = unique(tot$company), year = all_years),
    by = c("company", "year")
  ) %>%
  mutate(firm_year_total = coalesce(firm_year_total, 0))

# Step 3: apply rolling sums per (company, topic) for the numerator,
# and per company for the denominator.
firm_topic_rolled <- firm_topic_counts %>%
  arrange(company, topic, year) %>%
  group_by(company, topic) %>%
  mutate(count_rolled = roll_sum_trailing(count, ROLL_WINDOW)) %>%
  ungroup()

firm_year_totals_rolled <- firm_year_totals %>%
  arrange(company, year) %>%
  group_by(company) %>%
  mutate(firm_year_total_rolled = roll_sum_trailing(firm_year_total, ROLL_WINDOW)) %>%
  ungroup()

# Step 4: join, compute smoothed share, drop sparse firm-years
firm_stream <- firm_topic_rolled %>%
  left_join(firm_year_totals_rolled %>% select(company, year, firm_year_total_rolled),
            by = c("company", "year")) %>%
  filter(firm_year_total_rolled >= MIN_PATENTS_PER_WIN) %>%
  mutate(share = count_rolled / firm_year_total_rolled) %>%
  left_join(topic_info_sub %>% select(topic, short_label), by = "topic")

# legend ordering: topics by overall size (descending)
legend_order6 <- topic_info_sub %>%
  filter(topic %in% top_firm_stream_topics) %>%
  arrange(desc(count)) %>%
  pull(short_label)

firm_stream <- firm_stream %>%
  mutate(short_label = factor(short_label, levels = legend_order6),
         company     = factor(company, levels = names(COMPANY_COLORS)))

# y-axis upper limit: max of any firm-year's top-N cumulative smoothed share
y_upper_fig6 <- firm_stream %>%
  group_by(company, year) %>%
  summarise(total = sum(share), .groups = "drop") %>%
  pull(total) %>%
  max(na.rm = TRUE)
y_upper_fig6 <- min(1, ceiling(y_upper_fig6 * 10) / 10)

fig6 <- ggplot(firm_stream, aes(x = year, y = share, fill = short_label)) +
  geom_area(alpha = 0.9, colour = "white", linewidth = 0.2) +
  facet_wrap(~ company, ncol = 3) +
  scale_x_continuous(breaks = seq(2010, 2024, 4), expand = c(0, 0)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, y_upper_fig6), expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set2", name = NULL) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    title    = "Evolution of firm-level climate patent portfolios, 2010–2024",
    subtitle = sprintf("Cumulative share in top %d topics, trailing %d-year rolling sum (firm-windows with <%d patents omitted)",
                       TOP_N_FIRM_STREAM, ROLL_WINDOW, MIN_PATENTS_PER_WIN),
    x = NULL, y = "Share of firm's climate patents"
  ) +
  theme_report() +
  theme(
    legend.text     = element_text(size = 9),
    legend.key.size = unit(0.5, "cm"),
    plot.margin     = margin(10, 15, 10, 10)
  )

ggsave(file.path(OUTPUT_DIR, "fig6_firm_portfolio_over_time.png"),
       fig6, width = 12, height = 7, dpi = 300, bg = "white")
message("Saved fig6_firm_portfolio_over_time.png")


# =============================================================================
# FIGURE 7 — CPC subclass composition per topic (heatmap, row-normalised)
# =============================================================================
# Replaces the Table 2 view: shows that BERTopic's 36 clusters cross-cut the
# coarse Y02 subclass hierarchy. A topic drawing from multiple Y02 subclasses
# is evidence that the semantic grouping is more granular than CPC.
# -----------------------------------------------------------------------------

# The topic_cpc_composition.csv has columns: topic, Y02A, Y02B, Y02C, Y02D,
# Y02E, Y02P, Y02T, Y02W, Y04S, TOTAL_PATENTS. Subclass columns contain the
# percentage of the topic's patents carrying a CPC code under that subclass.
# Rows may sum slightly above 100% because a patent can be assigned to multiple
# CPC subclasses; we convert to a 0–1 fraction for plotting.
cpc_long <- cpc_comp %>%
  rename_with(~ str_trim(.x)) %>%
  filter(topic != -1) %>%
  pivot_longer(
    cols = matches("^Y0[24][A-Z]$"),   # exact subclass codes: Y02A, Y04S, etc.
    names_to = "cpc_subclass",
    values_to = "pct"
  ) %>%
  mutate(share = pct / 100) %>%
  left_join(topic_info_sub %>% select(topic, short_label, total_count = count),
            by = "topic")

# keep top 20 topics for a readable heatmap
TOP_N_CPC <- 20
top_cpc_topics <- topic_info_sub %>%
  slice_max(count, n = TOP_N_CPC) %>%
  pull(topic)

cpc_long <- cpc_long %>%
  filter(topic %in% top_cpc_topics) %>%
  mutate(topic_label = sprintf("T%d · %s", topic, short_label))

# topic ordering: by total count (largest at top)
topic_order7 <- topic_info_sub %>%
  filter(topic %in% top_cpc_topics) %>%
  arrange(desc(count)) %>%
  mutate(topic_label = sprintf("T%d · %s", topic, short_label)) %>%
  pull(topic_label)

# CPC ordering: meaningful domain order, not alphabetical
cpc_order <- c("Y02A", "Y02B", "Y02C", "Y02D", "Y02E", "Y02P", "Y02T", "Y02W", "Y04S")
cpc_order <- intersect(cpc_order, unique(cpc_long$cpc_subclass))

cpc_long <- cpc_long %>%
  mutate(topic_label  = factor(topic_label, levels = rev(topic_order7)),
         cpc_subclass = factor(cpc_subclass, levels = cpc_order))

fig7 <- ggplot(cpc_long, aes(x = cpc_subclass, y = topic_label, fill = share)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(
    data = cpc_long %>% filter(share >= 0.05),
    aes(label = percent(share, accuracy = 1),
        colour = share > 0.5),
    size = 2.9
  ) +
  scale_fill_gradient(low = "#f7fcf5", high = "#00441b",
                      labels = percent_format(accuracy = 1),
                      name = "Share of topic's\npatents") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey10"),
                      guide = "none") +
  labs(
    title    = "BERTopic clusters vs. Y02 CPC subclass composition",
    subtitle = sprintf("Row-normalised distribution of CPC subclasses for the top %d topics", TOP_N_CPC),
    x = "CPC subclass", y = NULL,
    caption = "Cells with <5% share are shown in colour only."
  ) +
  theme_report() +
  theme(
    panel.grid  = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 10, face = "bold")
  )

ggsave(file.path(OUTPUT_DIR, "fig7_cpc_heatmap.png"),
       fig7, width = 10, height = 9, dpi = 300, bg = "white")
message("Saved fig7_cpc_heatmap.png")


# =============================================================================
# FIGURE 8 — Aggregate firm topic focus (horizontal stacked bar chart)
# =============================================================================
# Lifetime 2010–2024 portfolio composition per firm. One row per firm, stacked
# horizontally to 100% across the top N topics + an "Other" bucket. Counterpart
# to Fig 6 without the time dimension — same story, compressed into a single
# reference view.
# -----------------------------------------------------------------------------

TOP_N_FIG8   <- 10   # number of named topics to show; rest go into "Other"
LABEL_MIN    <- 0.03  # label only segments whose share is at least this

top_fig8_topics <- topic_info_sub %>%
  slice_max(count, n = TOP_N_FIG8) %>%
  pull(topic)

# build per-firm totals and per-firm-topic shares
firm_topic_shares <- company_topic %>%
  filter(topic != -1) %>%
  mutate(topic_group = if_else(topic %in% top_fig8_topics,
                               as.character(topic), "other")) %>%
  group_by(company, topic_group) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  group_by(company) %>%
  mutate(share = count / sum(count)) %>%
  ungroup() %>%
  left_join(
    topic_info_sub %>%
      mutate(topic_int = topic, topic_chr = as.character(topic)) %>%
      select(topic_chr, short_label, topic_int),
    by = c("topic_group" = "topic_chr")
  ) %>%
  mutate(
    display_label = if_else(topic_group == "other",
                            "Other topics",
                            sprintf("T%d · %s", topic_int, short_label))
  )

# legend order: top topics first (by overall size), Other last
legend_order8 <- topic_info_sub %>%
  filter(topic %in% top_fig8_topics) %>%
  arrange(desc(count)) %>%
  mutate(display_label = sprintf("T%d · %s", topic, short_label)) %>%
  pull(display_label)
legend_order8 <- c(legend_order8, "Other topics")

# firm order: preserve COMPANY_COLORS order (alphabetical by design here)
firm_topic_shares <- firm_topic_shares %>%
  mutate(display_label = factor(display_label, levels = legend_order8),
         company       = factor(company, levels = rev(names(COMPANY_COLORS))))

# Explicitly sort by (company, display_label) matching the factor levels, then
# compute the midpoint of each segment manually. This guarantees that label
# x-positions line up with the rendered bar segments regardless of how ggplot
# orders the stack internally.
firm_topic_shares <- firm_topic_shares %>%
  arrange(company, display_label) %>%
  group_by(company) %>%
  mutate(
    x_end   = cumsum(share),
    x_start = x_end - share,
    x_mid   = (x_start + x_end) / 2
  ) %>%
  ungroup()

# build a distinct palette. 15 + 1 "Other" = 16 fills. Set3 has 12; extend with
# Dark2 for the remainder, and grey for Other.
n_named <- length(legend_order8) - 1
named_colours <- c(
  RColorBrewer::brewer.pal(12, "Set3"),
  RColorBrewer::brewer.pal(8,  "Dark2")
)[seq_len(n_named)]
fill_palette <- setNames(c(named_colours, "grey70"), legend_order8)

fig8 <- ggplot(firm_topic_shares,
               aes(x = share, y = company, fill = display_label)) +
  geom_col(width = 0.75, colour = "white", linewidth = 0.3,
           position = position_stack(reverse = TRUE)) +
  geom_text(
    data = firm_topic_shares %>% filter(share >= LABEL_MIN),
    aes(x = x_mid, y = company, label = percent(share, accuracy = 1)),
    inherit.aes = FALSE,
    size = 3.1, colour = "grey15"
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     expand = c(0, 0), limits = c(0, 1.001)) +
  scale_fill_manual(values = fill_palette, name = NULL) +
  guides(fill = guide_legend(ncol = 1)) +
  labs(
    title    = "Aggregate topic focus by firm, 2010–2024",
    subtitle = sprintf("Share of each firm's climate patent portfolio across the top %d topics (remainder as 'Other')",
                       TOP_N_FIG8),
    x = "Share of firm's climate patent portfolio", y = NULL
  ) +
  theme_report() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey90"),
    axis.text.y        = element_text(size = 11, face = "bold"),
    legend.position    = "right",
    legend.text        = element_text(size = 8),
    legend.key.size    = unit(0.4, "cm"),
    plot.margin        = margin(10, 15, 10, 10)
  )

ggsave(file.path(OUTPUT_DIR, "fig8_firm_topic_focus.png"),
       fig8, width = 13, height = 5.5, dpi = 300, bg = "white")
message("Saved fig8_firm_topic_focus.png")

message("\nAll RQ2 figures written to ", OUTPUT_DIR)