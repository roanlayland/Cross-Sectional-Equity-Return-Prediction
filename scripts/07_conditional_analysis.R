# =====================================================================
# Cross-Sectional-Equity-Return-Prediction — Conditional factor analysis
#
# Does each factor work everywhere, or only in certain kinds of stocks?
# Conditions: size, sector, volatility, and time period.
#
# MULTIPLE TESTING. 26 factors x 5 size buckets = 130 tests. At
# alpha = 0.05, an expected ~6 significant results by chance alone.
# Trust MONOTONIC PATTERNS across buckets, not isolated hits.
# =====================================================================

library(tidyverse)
library(lubridate)
library(scales)

PANEL_PATH <- "data/panel_ranked.rds"
panel      <- read_rds(PANEL_PATH)
FEATURES   <- read_rds("data/feature_list.rds")
RK         <- paste0("rk_", FEATURES)

panel <- panel |>
  filter(!is.na(fwd_12m), form_date >= as.Date("1994-03-31"))

theme_set(theme_minimal(base_size = 11) +
          theme(panel.grid.minor = element_blank(),
                plot.title = element_text(face = "bold")))


# =====================================================================
# 1. Generic conditional sort
# =====================================================================
# For each (factor, group): decile sort WITHIN the group each month,
# then t-test the time series of top-minus-bottom spreads.
#
# Sorting within group is essential. Sorting globally then splitting
# would confound the factor with the grouping variable.

conditional_sort <- function(dat, group_var, min_n = 100) {
  map_dfr(FEATURES, function(f) {
    rk <- paste0("rk_", f)
    dat |>
      filter(!is.na(.data[[rk]]), !is.na(.data[[group_var]])) |>
      group_by(form_date, grp = .data[[group_var]]) |>
      filter(n() >= min_n) |>
      mutate(d = ntile(.data[[rk]], 10)) |>
      filter(d %in% c(1, 10)) |>
      group_by(form_date, grp, d) |>
      summarise(ret = mean(fwd_12m), .groups = "drop") |>
      pivot_wider(names_from = d, values_from = ret, names_prefix = "d") |>
      mutate(spread = d10 - d1) |>
      group_by(grp) |>
      summarise(
        avg_spread = mean(spread, na.rm = TRUE),
        t_stat     = mean(spread, na.rm = TRUE) /
                     (sd(spread, na.rm = TRUE) / sqrt(n())),
        hit_rate   = mean(spread > 0, na.rm = TRUE),
        n_periods  = n(),
        .groups = "drop"
      ) |>
      mutate(feature = f)
  })
}


# =====================================================================
# 2. BY SIZE
# =====================================================================
panel_size <- panel |>
  group_by(form_date) |>
  mutate(size_q = ntile(mktcap, 5)) |>
  ungroup() |>
  mutate(size_lab = factor(size_q,
                           labels = c("1 Smallest","2","3","4","5 Largest")))

size_res <- conditional_sort(panel_size, "size_lab", min_n = 100)

cat("\n=========== FACTOR SPREADS BY SIZE QUINTILE ===========\n")
size_res |>
  select(feature, grp, avg_spread, t_stat) |>
  pivot_wider(names_from = grp, values_from = c(avg_spread, t_stat)) |>
  arrange(desc(abs(`t_stat_1 Smallest`))) |>
  print(n = 30, width = Inf)

# Monotonicity: does the effect strengthen steadily toward small caps?
size_trend <- size_res |>
  mutate(q = as.integer(grp)) |>
  group_by(feature) |>
  summarise(
    small_spread = avg_spread[q == 1],
    large_spread = avg_spread[q == 5],
    small_minus_large = avg_spread[q == 1] - avg_spread[q == 5],
    # correlation between bucket number and spread: -1 = perfectly
    # decreasing with size, +1 = perfectly increasing
    monotone = cor(q, avg_spread),
    n_sig = sum(abs(t_stat) > 2)
  ) |>
  arrange(desc(abs(small_minus_large)))

cat("\n=========== SIZE GRADIENT ===========\n")
cat("monotone near -1: factor strongest in SMALL caps (expected)\n")
cat("monotone near +1: factor strongest in LARGE caps (unusual)\n")
cat("n_sig: how many of 5 buckets reach |t| > 2\n\n")
print(size_trend, n = 30)


# =====================================================================
# 3. BY SECTOR
# =====================================================================
# 26 factors x 11 sectors = 286 tests. Expect ~14 false positives.

sector_names <- c("10"="Energy","15"="Materials","20"="Industrials",
                  "25"="Cons Disc","30"="Cons Staples","35"="Health Care",
                  "40"="Financials","45"="Tech","50"="Telecom",
                  "55"="Utilities","60"="Real Estate")

panel_sec <- panel |>
  filter(!is.na(gsector)) |>
  mutate(sector = recode(as.character(gsector), !!!sector_names))

sector_res <- conditional_sort(panel_sec, "sector", min_n = 50)

cat("\n=========== TOP FACTOR-SECTOR COMBINATIONS ===========\n")
cat("~14 of these are false positives. Treat as hypothesis-generating.\n\n")
sector_res |>
  filter(n_periods >= 200) |>
  arrange(desc(abs(t_stat))) |>
  select(feature, grp, avg_spread, t_stat, n_periods) |>
  print(n = 25)

sector_consistency <- sector_res |>
  filter(n_periods >= 200) |>
  group_by(feature) |>
  summarise(
    mean_spread = mean(avg_spread),
    n_sectors   = n(),
    n_positive  = sum(avg_spread > 0),
    n_sig       = sum(abs(t_stat) > 2),
    consistency = max(n_positive, n_sectors - n_positive) / n_sectors
  ) |>
  arrange(desc(n_sig))

cat("\n=========== CROSS-SECTOR CONSISTENCY ===========\n")
cat("consistency = share of sectors sharing the same sign.\n")
cat("Above 0.85 with several significant = robust effect.\n\n")
print(sector_consistency, n = 30)


# =====================================================================
# 4. BY VOLATILITY
# =====================================================================
# Limits-to-arbitrage prediction: mispricing persists where arbitrage
# is risky. Factors should be stronger in high-volatility stocks.

panel_vol <- panel |>
  filter(!is.na(vol_12m)) |>
  group_by(form_date) |>
  mutate(vol_q = ntile(vol_12m, 3)) |>
  ungroup() |>
  mutate(vol_lab = factor(vol_q, labels = c("1 Low vol","2 Mid","3 High vol")))

vol_res <- conditional_sort(panel_vol, "vol_lab", min_n = 100)

cat("\n=========== FACTOR SPREADS BY VOLATILITY TERCILE ===========\n")
vol_res |>
  select(feature, grp, avg_spread, t_stat) |>
  pivot_wider(names_from = grp, values_from = c(avg_spread, t_stat)) |>
  arrange(desc(abs(`t_stat_3 High vol`))) |>
  print(n = 30, width = Inf)


# =====================================================================
# 5. BY TIME PERIOD
# =====================================================================
# The single most important robustness check. A factor that works in
# one decade and not the others is a period-specific artifact.

panel_era <- panel |>
  mutate(era = case_when(
    form_date < as.Date("2002-01-01") ~ "1994-2001",
    form_date < as.Date("2010-01-01") ~ "2002-2009",
    form_date < as.Date("2018-01-01") ~ "2010-2017",
    TRUE                              ~ "2018-2024"
  ))

era_res <- conditional_sort(panel_era, "era", min_n = 100)

cat("\n=========== FACTOR SPREADS BY ERA ===========\n")
era_res |>
  select(feature, grp, t_stat) |>
  pivot_wider(names_from = grp, values_from = t_stat) |>
  mutate(n_sig = rowSums(across(where(is.numeric), ~ abs(.x) > 2), na.rm = TRUE),
         all_same_sign = abs(rowSums(across(-c(feature, n_sig),
                                            ~ sign(.x)), na.rm = TRUE)) == 4) |>
  arrange(desc(n_sig)) |>
  print(n = 30, width = Inf)

cat("\nFactors significant in 3+ of 4 eras with consistent sign are the\n")
cat("ones worth defending. Anything in 1 era only is likely noise.\n")


# =====================================================================
# 6. FIGURES
# =====================================================================
top8 <- size_res |> group_by(feature) |>
  summarise(m = max(abs(t_stat))) |> slice_max(m, n = 8) |> pull(feature)

fig_size <- size_res |>
  filter(feature %in% top8) |>
  ggplot(aes(grp, avg_spread, fill = abs(t_stat) > 2)) +
  geom_col() +
  geom_hline(yintercept = 0, colour = "grey40") +
  facet_wrap(~ feature, scales = "free_y") +
  scale_fill_manual(values = c("TRUE" = "steelblue4", "FALSE" = "grey75"),
                    name = "|t| > 2") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Factor decile spreads by size quintile",
       subtitle = "Sorted within each size bucket. Blue = significant.",
       x = NULL, y = "Top-minus-bottom spread") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

ggsave("figures/conditional_size.png", fig_size, width = 11, height = 7, dpi = 300)

fig_era <- era_res |>
  filter(feature %in% top8) |>
  ggplot(aes(grp, avg_spread, fill = abs(t_stat) > 2)) +
  geom_col() +
  geom_hline(yintercept = 0, colour = "grey40") +
  facet_wrap(~ feature, scales = "free_y") +
  scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "grey75"),
                    name = "|t| > 2") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Factor decile spreads by era",
       subtitle = "Consistency across periods is the strongest evidence a factor is real",
       x = NULL, y = "Top-minus-bottom spread") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

ggsave("figures/conditional_era.png", fig_era, width = 11, height = 7, dpi = 300)


# =====================================================================
# 7. EXPORT
# =====================================================================
write_csv(size_res,            "output/conditional_size.csv")
write_csv(size_trend,          "output/size_gradient.csv")
write_csv(sector_res,          "output/conditional_sector.csv")
write_csv(sector_consistency,  "output/sector_consistency.csv")
write_csv(vol_res,             "output/conditional_volatility.csv")
write_csv(era_res,             "output/conditional_era.csv")

cat("\nDone. Six CSVs in output/, two figures in figures/.\n")
