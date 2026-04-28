# =============================================================================
#  RQ2 visualisations  —  Big Tech Climate Patents
#  Figures 3 – 13.  All figures use ggplot2 / tidyverse.
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
#  Outputs: figures/fig3_keyword_prevalence.png ... fig13_topics_over_time.png
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(stringr)
  library(tidytext)       # reorder_within / scale_x_reordered (Fig 3 facets)
  library(RColorBrewer)   # palette for the global topic colour map
})

# ---- Paths -------------------------------------------------------------------
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
  year  = c(2015, 2017, 2020),
  label = c("Paris Agreement", "US withdrawal", "COVID")
)

# =============================================================================
# GLOBAL CONFIGURATION — single source of truth for "which topics" and "what
# colour does each topic get". Every figure that shows topics derives its set
# from TOP_N_GLOBAL and its colour scale from TOPIC_COLORS / TOPIC_FILL_SCALE.
# =============================================================================

TOP_N_GLOBAL <- 6


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

# ---- Topic short labels: most-prevalent keyword ------------------------------
LABEL_CANDIDATE_K <- 5

most_prevalent_keyword <- function(topic_id, candidate_words, docs_df) {
  abstracts <- docs_df %>%
    filter(topic == topic_id) %>%
    pull(abstract) %>%
    str_to_lower()
  if (length(abstracts) == 0 || length(candidate_words) == 0) {
    return(NA_character_)
  }
  prev <- vapply(candidate_words, function(w) {
    pattern <- paste0("\\b", str_replace_all(str_to_lower(w), " ", "\\\\s+"), "\\b")
    mean(str_detect(abstracts, pattern))
  }, numeric(1))
  candidate_words[which.max(prev)]
}

short_label_lookup <- topic_terms %>%
  filter(topic != -1) %>%
  group_by(topic) %>%
  slice_min(rank, n = LABEL_CANDIDATE_K) %>%
  arrange(topic, rank) %>%
  summarise(words = list(word), .groups = "drop") %>%
  mutate(short_label = map2_chr(topic, words,
                                ~ most_prevalent_keyword(.x, .y, docs))) %>%
  select(topic, short_label)

fallback_first_keyword <- function(name) {
  parts <- str_split(name, "_")[[1]]
  parts <- parts[-1]
  if (length(parts) == 0) "" else parts[1]
}

topic_info <- topic_info %>%
  left_join(short_label_lookup, by = "topic") %>%
  mutate(short_label = if_else(is.na(short_label) | short_label == "",
                               map_chr(name, fallback_first_keyword),
                               short_label))

# exclude the HDBSCAN outlier bin globally
topic_info_sub <- topic_info %>% filter(topic != -1)
docs_sub       <- docs       %>% filter(topic != -1)


# =============================================================================
# GLOBAL TOPIC SET + COLOUR MAP
#
# The top-N topics by overall patent count are the canonical "named" topics.
# Each gets a fixed colour drawn from a qualitative palette, and a fixed
# display label of the form "T<id> · <short_label>". Every downstream figure
# pulls from these two objects (TOP_TOPICS_GLOBAL, TOPIC_COLORS) so a topic
# that is, say, blue in Fig 4 stays blue in Figs 5, 6, 7, 8, 9, 11, 13.
#
# "Other topics" gets a dedicated grey, kept out of the qualitative palette so
# it never collides with a named topic's colour.
# =============================================================================

TOP_TOPICS_GLOBAL <- topic_info_sub %>%
  slice_max(count, n = TOP_N_GLOBAL) %>%
  arrange(desc(count)) %>%
  pull(topic)

# Display label per topic: same format used everywhere a topic appears with
# its id, e.g. legends and heatmap rows.
topic_display <- topic_info_sub %>%
  filter(topic %in% TOP_TOPICS_GLOBAL) %>%
  arrange(desc(count)) %>%
  mutate(topic_label = sprintf("T%d · %s", topic, short_label)) %>%
  select(topic, short_label, topic_label, count)

# Qualitative palette: Set2 has 8 distinct hues, plenty for 6 topics. Order is
# locked to descending topic size so the largest topic always gets the first
# colour, regardless of which figure renders first.
qual_palette <- RColorBrewer::brewer.pal(max(3, TOP_N_GLOBAL), "Set2")[seq_len(TOP_N_GLOBAL)]

# Three lookup tables for convenience — same colour, three different keys, so
# each figure can use whichever identifier is most natural for its data shape.
TOPIC_COLORS <- setNames(qual_palette, TOP_TOPICS_GLOBAL)                   # by topic id (numeric)
TOPIC_COLORS_BY_LABEL <- setNames(qual_palette, topic_display$short_label)  # by short label
TOPIC_COLORS_BY_TLABEL <- setNames(qual_palette, topic_display$topic_label) # by "T<id> · <label>"

OTHER_COLOR <- "grey70"

# Convenience: scale_fill / scale_colour builders that include "Other topics".
# Pass `with_other = TRUE` for figures that have an Other bucket.
topic_fill_scale <- function(by = c("short_label", "topic_label", "topic"),
                             with_other = FALSE,
                             name = NULL) {
  by <- match.arg(by)
  values <- switch(by,
                   short_label = TOPIC_COLORS_BY_LABEL,
                   topic_label = TOPIC_COLORS_BY_TLABEL,
                   topic       = TOPIC_COLORS)
  if (with_other) values <- c(values, `Other topics` = OTHER_COLOR)
  scale_fill_manual(values = values, name = name)
}

topic_colour_scale <- function(by = c("short_label", "topic_label", "topic"),
                               with_other = FALSE,
                               name = NULL) {
  by <- match.arg(by)
  values <- switch(by,
                   short_label = TOPIC_COLORS_BY_LABEL,
                   topic_label = TOPIC_COLORS_BY_TLABEL,
                   topic       = TOPIC_COLORS)
  if (with_other) values <- c(values, `Other topics` = OTHER_COLOR)
  scale_colour_manual(values = values, name = name)
}

# =============================================================================
# FIGURE 3 — Keyword prevalence for top-6 topics (faceted bar chart)
#
# Selection logic: for each topic, take BERTopic's top c-TF-IDF candidates,
# compute corpus prevalence (share of topic abstracts containing the exact
# phrase), drop zero-prevalence keywords, and keep the top TOP_N_KEYWORDS by
# prevalence. This avoids 0% bars from class-distinctive but rare bigrams.
# =============================================================================
TOP_N_KEYWORDS <- 4
KEYWORD_CANDIDATE_K <- 10   # pull more candidates so we have room to filter

candidate_keywords <- topic_terms %>%
  filter(topic %in% TOP_TOPICS_GLOBAL) %>%
  group_by(topic) %>%
  slice_min(rank, n = KEYWORD_CANDIDATE_K) %>%
  ungroup() %>%
  select(topic, word)

compute_prevalence <- function(topic_id, words, docs_df) {
  abstracts <- docs_df %>%
    filter(topic == topic_id) %>%
    pull(abstract) %>%
    str_to_lower()
  n_docs <- length(abstracts)
  if (n_docs == 0) return(tibble(word = words, prevalence = 0, n_docs = 0))
  
  prev <- map_dbl(words, function(w) {
    pattern <- paste0("\\b", str_replace_all(str_to_lower(w), " ", "\\\\s+"), "\\b")
    mean(str_detect(abstracts, pattern))
  })
  tibble(word = words, prevalence = prev, n_docs = n_docs)
}

prev_tbl <- candidate_keywords %>%
  group_by(topic) %>%
  summarise(words = list(word), .groups = "drop") %>%
  mutate(res = map2(topic, words, ~ compute_prevalence(.x, .y, docs_sub))) %>%
  select(topic, res) %>%
  unnest(res) %>%
  filter(prevalence > 0) %>%                       # drop undetectable keywords
  group_by(topic) %>%
  slice_max(prevalence, n = TOP_N_KEYWORDS) %>%    # keep top-N by prevalence
  ungroup() %>%
  left_join(topic_display, by = "topic") %>%
  mutate(facet_label = sprintf("T%d · %s  (N=%d)", topic, short_label, count))

# Order facets by topic size; order words inside each facet by prevalence.
prev_tbl <- prev_tbl %>%
  mutate(facet_label = fct_reorder(facet_label, -count),
         word        = reorder_within(word, prevalence, facet_label),
         topic_label = sprintf("T%d · %s", topic, short_label))

x_upper <- max(prev_tbl$prevalence, na.rm = TRUE) * 1.15

fig3 <- ggplot(prev_tbl, aes(x = word, y = prevalence, fill = topic_label)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = percent(prevalence, accuracy = 1)),
            hjust = -0.15, size = 3.2) +
  scale_x_reordered() +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, x_upper),
                     expand = c(0, 0)) +
  topic_fill_scale(by = "topic_label", name = NULL) +
  guides(fill = "none") +
  coord_flip() +
  facet_wrap(~ facet_label, scales = "free_y", ncol = 2) +
  labs(
    title    = "Keyword prevalence within the six largest topics",
    subtitle = "Share of patents in each topic whose abstract contains the keyword (top-ranked by BERTopic, filtered to those actually present)",
    x = NULL, y = "Share of patents in topic"
  ) +
  theme_report() +
  theme(
    axis.text.y = element_text(size = 9),
    strip.text  = element_text(face = "bold", size = 10,
                               margin = margin(t = 2, b = 2))
  )

# =============================================================================
# FIGURE 4 — Topics over time (stacked area, top topics + other)
# =============================================================================
stream_df <- year_topic %>%
  filter(topic != -1, year >= 2010, year <= 2024) %>%
  mutate(
    topic_group = if_else(topic %in% TOP_TOPICS_GLOBAL, as.character(topic), "other")
  ) %>%
  group_by(year, topic_group) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  left_join(
    topic_display %>%
      mutate(topic_chr = as.character(topic)) %>%
      select(topic_chr, short_label),
    by = c("topic_group" = "topic_chr")
  ) %>%
  mutate(
    display_label = if_else(topic_group == "other", "Other topics", short_label)
  )

# legend ordered by global topic size, "Other" last
legend_order <- c(topic_display$short_label, "Other topics")

stream_df <- stream_df %>%
  mutate(display_label = factor(display_label, levels = legend_order))

fig4 <- ggplot(stream_df, aes(x = year, y = count, fill = display_label)) +
  geom_area(alpha = 0.85, colour = "white", linewidth = 0.2) +
  scale_x_continuous(breaks = seq(2010, 2024, 2), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  topic_fill_scale(by = "short_label", with_other = TRUE, name = NULL) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title    = "Topic composition of Big Tech climate patents, 2010–2024",
    subtitle = sprintf("Stacked count of patents per topic; top %d topics shown, remainder grouped as 'Other'",
                       TOP_N_GLOBAL),
    x = NULL, y = "Number of patents"
  ) +
  theme_report() +
  theme(
    legend.text   = element_text(size = 9),
    legend.key.size = unit(0.5, "cm"),
    plot.margin   = margin(10, 15, 10, 10)
  )


# =============================================================================
# FIGURE 5 — Firm × topic heatmap (row-normalised)
# =============================================================================

firm_totals <- company_topic %>%
  filter(topic != -1) %>%
  group_by(company) %>%
  summarise(firm_total = sum(count), .groups = "drop")

heat_df <- company_topic %>%
  filter(topic %in% TOP_TOPICS_GLOBAL) %>%
  rename(firm_topic_count = count) %>%
  left_join(firm_totals, by = "company") %>%
  left_join(
    topic_display %>% dplyr::select(topic, topic_label),
    by = "topic",
    suffix = c("_old", "")            # keep the new topic_label unsuffixed
  ) %>%
  mutate(share = firm_topic_count / firm_total) %>%
  mutate(
    topic_label = factor(topic_label, levels = rev(topic_display$topic_label)),
    company     = factor(company, levels = names(COMPANY_COLORS))
  )

heat_df <- heat_df %>%
  mutate(
    topic_label = factor(topic_label, levels = rev(topic_display$topic_label)),
    company     = factor(company, levels = names(COMPANY_COLORS))
  )

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
    subtitle = sprintf("Row-normalised share of each firm's climate patents in the top %d topics",
                       TOP_N_GLOBAL),
    x = NULL, y = NULL
  ) +
  theme_report() +
  theme(
    panel.grid  = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 10, face = "bold")
  )


# =============================================================================
# FIGURE 6 — Firm portfolio composition over time (faceted stacked area)
# =============================================================================
ROLL_WINDOW          <- 3
MIN_PATENTS_PER_WIN  <- 10

roll_sum_trailing <- function(x, w) {
  n <- length(x)
  vapply(seq_len(n), function(i) sum(x[max(1, i - w + 1):i]), numeric(1))
}

all_years <- seq(2010, 2024)
grid <- expand_grid(
  company = unique(tot$company),
  year    = all_years,
  topic   = TOP_TOPICS_GLOBAL
)

firm_topic_counts <- tot %>%
  filter(topic != -1, topic %in% TOP_TOPICS_GLOBAL,
         year >= min(all_years), year <= max(all_years)) %>%
  group_by(company, year, topic) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  right_join(grid, by = c("company", "year", "topic")) %>%
  mutate(count = coalesce(count, 0))

firm_year_totals <- tot %>%
  filter(topic != -1, year >= min(all_years), year <= max(all_years)) %>%
  group_by(company, year) %>%
  summarise(firm_year_total = sum(count), .groups = "drop") %>%
  right_join(
    expand_grid(company = unique(tot$company), year = all_years),
    by = c("company", "year")
  ) %>%
  mutate(firm_year_total = coalesce(firm_year_total, 0))

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

firm_stream <- firm_topic_rolled %>%
  left_join(firm_year_totals_rolled %>% select(company, year, firm_year_total_rolled),
            by = c("company", "year")) %>%
  filter(firm_year_total_rolled >= MIN_PATENTS_PER_WIN) %>%
  mutate(share = count_rolled / firm_year_total_rolled) %>%
  left_join(topic_display %>% select(topic, short_label), by = "topic") %>%
  mutate(short_label = factor(short_label, levels = topic_display$short_label),
         company     = factor(company, levels = names(COMPANY_COLORS)))

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
  topic_fill_scale(by = "short_label", name = NULL) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    title    = "Evolution of firm-level climate patent portfolios, 2010–2024",
    subtitle = sprintf("Cumulative share in top %d topics, trailing %d-year rolling sum (firm-windows with <%d patents omitted)",
                       TOP_N_GLOBAL, ROLL_WINDOW, MIN_PATENTS_PER_WIN),
    x = NULL, y = "Share of firm's climate patents"
  ) +
  theme_report() +
  theme(
    legend.text     = element_text(size = 9),
    legend.key.size = unit(0.5, "cm"),
    plot.margin     = margin(10, 15, 10, 10)
  )


# =============================================================================
# FIGURE 7 — CPC subclass composition per topic (heatmap, row-normalised)
# =============================================================================
# Same reasoning as Fig 5: cell fill encodes a magnitude on a sequential scale,
# so the per-topic colour map does not apply. Topic SET and ORDER match the
# global canonical order to keep cross-figure reading consistent.
cpc_long <- cpc_comp %>%
  rename_with(~ str_trim(.x)) %>%
  filter(topic != -1) %>%
  pivot_longer(
    cols = matches("^Y0[24][A-Z]$"),
    names_to = "cpc_subclass",
    values_to = "pct"
  ) %>%
  mutate(share = pct / 100) %>%
  left_join(topic_info_sub %>% select(topic, short_label, total_count = count),
            by = "topic")

cpc_long <- cpc_long %>%
  filter(topic %in% TOP_TOPICS_GLOBAL) %>%
  mutate(topic_label = sprintf("T%d · %s", topic, short_label),
         topic_label = factor(topic_label, levels = rev(topic_display$topic_label)))

cpc_order <- c("Y02A", "Y02B", "Y02C", "Y02D", "Y02E", "Y02P", "Y02T", "Y02W", "Y04S")
cpc_order <- intersect(cpc_order, unique(cpc_long$cpc_subclass))

cpc_long <- cpc_long %>%
  mutate(cpc_subclass = factor(cpc_subclass, levels = cpc_order))

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
    subtitle = sprintf("Row-normalised distribution of CPC subclasses for the top %d topics",
                       TOP_N_GLOBAL),
    x = "CPC subclass", y = NULL,
    caption = "Cells with <5% share are shown in colour only."
  ) +
  theme_report() +
  theme(
    panel.grid  = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 10, face = "bold")
  )


# =============================================================================
# FIGURE 8 — Aggregate firm topic focus (horizontal stacked bar chart)
# =============================================================================
LABEL_MIN <- 0.03

firm_topic_shares <- company_topic %>%
  filter(topic != -1) %>%
  mutate(topic_group = if_else(topic %in% TOP_TOPICS_GLOBAL,
                               as.character(topic), "other")) %>%
  group_by(company, topic_group) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  group_by(company) %>%
  mutate(share = count / sum(count)) %>%
  ungroup() %>%
  left_join(
    topic_display %>%
      mutate(topic_chr = as.character(topic)) %>%
      select(topic_chr, short_label, topic_label),
    by = c("topic_group" = "topic_chr")
  ) %>%
  mutate(
    display_label = if_else(topic_group == "other",
                            "Other topics",
                            topic_label)
  )

# legend: top topics in canonical order, then Other
legend_order8 <- c(topic_display$topic_label, "Other topics")

firm_topic_shares <- firm_topic_shares %>%
  mutate(display_label = factor(display_label, levels = legend_order8),
         company       = factor(company, levels = rev(names(COMPANY_COLORS))))

firm_topic_shares <- firm_topic_shares %>%
  arrange(company, display_label) %>%
  group_by(company) %>%
  mutate(
    x_end   = cumsum(share),
    x_start = x_end - share,
    x_mid   = (x_start + x_end) / 2
  ) %>%
  ungroup()

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
  topic_fill_scale(by = "topic_label", with_other = TRUE, name = NULL) +
  guides(fill = guide_legend(ncol = 1)) +
  labs(
    title    = "Aggregate topic focus by firm, 2010–2024",
    subtitle = sprintf("Share of each firm's climate patent portfolio across the top %d topics (remainder as 'Other')",
                       TOP_N_GLOBAL),
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



library(devEMF)

save_fig <- function(plot, name, width, height) {
  ggsave(file.path(OUTPUT_DIR, paste0(name, ".png")),
         plot, width = width, height = height, dpi = 300, bg = "white")
  ggsave(file.path(OUTPUT_DIR, paste0(name, ".emf")),
         plot, width = width, height = height,
         device = emf, bg = "white")
}

save_fig(fig3, "fig3_keyword_prevalence",       14, 10)
save_fig(fig4, "fig4_topics_over_time",         14,  7)
save_fig(fig5, "fig5_firm_topic_heatmap",       12, 10)
save_fig(fig6, "fig6_firm_portfolio_over_time", 14,  8)
save_fig(fig7, "fig7_cpc_heatmap",              12, 11)
save_fig(fig8, "fig8_firm_topic_focus",         15,  6)


# =============================================================================
# RQ 2.2 — additional visualisations of topic shifts around policy events
# =============================================================================
year_totals <- year_topic %>%
  filter(topic != -1, year >= 2010, year <= 2024) %>%
  group_by(year) %>%
  summarise(year_total = sum(count), .groups = "drop")


# =============================================================================
# FIGURE 9 — Small-multiples line chart, one panel per top topic
# =============================================================================
line_df <- year_topic %>%
  filter(topic %in% TOP_TOPICS_GLOBAL, year >= 2010, year <= 2024) %>%
  group_by(year, topic) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  left_join(year_totals, by = "year") %>%
  mutate(share = count / year_total) %>%
  left_join(topic_display, by = "topic") %>%
  mutate(facet_label = sprintf("T%d · %s  (N=%d)", topic, short_label, count.y),
         facet_label = fct_reorder(facet_label, -count.y),
         topic_label = sprintf("T%d · %s", topic, short_label))

fig9 <- ggplot(line_df, aes(x = year, y = share, colour = topic_label)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  facet_wrap(~ facet_label, ncol = 3, scales = "free_y") +
  scale_x_continuous(breaks = seq(2010, 2024, 4)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.02, 0.15))) +
  topic_colour_scale(by = "topic_label", name = NULL) +
  guides(colour = "none") +   # facet titles already identify each topic
  labs(
    title    = "Topic-level trajectories",
    subtitle = sprintf("Yearly share of all climate patents per topic, top %d topics", TOP_N_GLOBAL),
    x = NULL, y = "Share of climate patents",
    caption = "Patent publication lags filing by ~12–24 months; expected response window begins 1–2 years after each event."
  ) +
  theme_report() +
  theme(
    strip.text = element_text(size = 9, face = "bold"),
    panel.spacing = unit(0.6, "lines")
  )


# =============================================================================
# FIGURE 11 — Bump chart of topic rankings over time
# =============================================================================
bump_df <- year_topic %>%
  filter(topic %in% TOP_TOPICS_GLOBAL, year >= 2010, year <= 2024) %>%
  group_by(year, topic) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  group_by(year) %>%
  mutate(rank = rank(-count, ties.method = "first")) %>%
  ungroup() %>%
  left_join(topic_display %>% select(topic, short_label), by = "topic")

endpoint_left  <- bump_df %>% filter(year == min(year))
endpoint_right <- bump_df %>% filter(year == max(year))

fig11 <- ggplot(bump_df, aes(x = year, y = rank,
                             colour = short_label, group = short_label)) +
  geom_line(linewidth = 1.1, alpha = 0.85) +
  geom_point(size = 2.4) +
  geom_text(data = endpoint_left,
            aes(label = short_label),
            hjust = 1.05, size = 3, show.legend = FALSE) +
  geom_text(data = endpoint_right,
            aes(label = short_label),
            hjust = -0.05, size = 3, show.legend = FALSE) +
  scale_y_reverse(breaks = seq_len(TOP_N_GLOBAL)) +
  scale_x_continuous(breaks = seq(2010, 2024, 2),
                     expand = expansion(add = c(2.0, 2.0))) +
  topic_colour_scale(by = "short_label", name = NULL) +
  guides(colour = "none") +   # endpoint labels identify each line
  labs(
    title    = "Topic rank dynamics, 2010–2024",
    subtitle = sprintf("Yearly rank of the top %d topics by patent share; rank 1 = largest",
                       TOP_N_GLOBAL),
    x = NULL, y = "Rank within climate portfolio"
  ) +
  theme_report() +
  theme(
    panel.grid.major.y = element_line(colour = "grey92"),
    panel.grid.minor   = element_blank()
  )


# =============================================================================
# FIGURE 13 — BERTopic-style multi-line view (topics_over_time, re-styled)
# =============================================================================
tot_df <- tot %>%
  filter(topic %in% TOP_TOPICS_GLOBAL, year >= 2010, year <= 2024) %>%
  group_by(year, topic) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  left_join(year_totals, by = "year") %>%
  mutate(share = count / year_total) %>%
  left_join(topic_display %>% select(topic, short_label), by = "topic") %>%
  mutate(short_label = factor(short_label, levels = topic_display$short_label))

fig13 <- ggplot(tot_df, aes(x = year, y = share,
                            colour = short_label, group = short_label)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 1.6) +
  scale_x_continuous(breaks = seq(2010, 2024, 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.02, 0.18))) +
  topic_colour_scale(by = "short_label", name = NULL) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(
    title    = "Topic prevalence over time",
    subtitle = sprintf("Yearly share of climate patents, top %d topics on a shared axis",
                       TOP_N_GLOBAL),
    x = NULL, y = "Share of climate patents"
  ) +
  theme_report()


# ---- Save RQ 2.2 figures -----------------------------------------------------
save_fig(fig9,  "fig9_topic_lines_facet",  14, 8)
save_fig(fig11, "fig11_topic_bump",        14, 8)
save_fig(fig13, "fig13_topics_over_time",  14, 7)


message("\nAll RQ2 figures written to ", OUTPUT_DIR)