# =====================================================================
# Cross-Sectional-Equity-Return-Prediction — Session 5/6: Factor sorts and linear models
#
# Works on synthetic or real data.
# Replaces the earlier 02_analysis.R (which assumed annual/quintile).
# =====================================================================

library(tidyverse)
library(lubridate)
library(broom)
library(sandwich)
library(lmtest)
library(glmnet)
library(scales)

# ---- SWITCH BETWEEN SYNTHETIC AND REAL HERE ------------------------
#PANEL_PATH <- "data/panel_ranked_SYNTH.rds"
PANEL_PATH <- "data/panel_ranked.rds"
# --------------------------------------------------------------------

panel    <- read_rds(PANEL_PATH)
FEATURES <- read_rds("data/feature_list.rds")
RK       <- paste0("rk_", FEATURES)
IS_SYNTH <- str_detect(PANEL_PATH, "SYNTH")
# ---- IMPUTATION TOGGLE ---------------------------------------------
# Complete-case analysis drops any row with an NA in ANY predictor.
# With several features at 20% missing, that compounds to ~70% loss.
# Median-impute within date instead, and flag what was missing —
# whether a firm reports a given item is itself informative.
IMPUTE <- TRUE

if (IMPUTE) {
  miss_rate <- panel |> summarise(across(all_of(RK), ~ mean(is.na(.x)))) |>
    pivot_longer(everything(), names_to = "term", values_to = "rate")
  needs_flag <- miss_rate |> filter(rate > 0.05) |> pull(term)
  
  panel <- panel |>
    mutate(across(all_of(needs_flag),
                  ~ as.integer(is.na(.x)), .names = "miss_{.col}")) |>
    group_by(form_date) |>
    mutate(across(all_of(RK), ~ {
      m <- is.na(.x)
      if (any(m) && !all(m)) .x[m] <- median(.x, na.rm = TRUE)
      .x
    })) |>
    ungroup()
  
  RK <- c(RK, paste0("miss_", needs_flag))
  cat("Imputed. Flagged:", length(needs_flag), "features\n")
}
# ---- DROP DEGENERATE PERIODS ---------------------------------------
# The first ~14 months have features that are 100% missing: lagged
# fundamentals and 12-month momentum don't exist yet. Median imputation
# can't fill a column with no observations, so lm() sees zero complete
# cases and errors. Drop these periods entirely.
bad_periods <- panel |>
  group_by(form_date) |>
  summarise(across(all_of(RK), ~ mean(is.na(.x))), .groups = "drop") |>
  pivot_longer(-form_date) |>
  filter(value == 1) |>
  distinct(form_date) |>
  pull(form_date)

cat("Dropping", length(bad_periods), "periods with fully-missing features\n")
if (length(bad_periods)) cat("  range:", format(range(bad_periods)), "\n")

panel <- panel |> filter(!form_date %in% bad_periods)

cat("Remaining:", n_distinct(panel$form_date), "periods,",
    nrow(panel), "rows\n")
# --------------------------------------------------------------------
cat("Median complete cases per period:",
    panel |> filter(!is.na(fwd_12m)) |> group_by(form_date) |>
      summarise(n = sum(complete.cases(pick(all_of(RK))))) |>
      pull(n) |> median(), "\n")
# --------------------------------------------------------------------
dir.create("output", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
theme_set(theme_minimal(base_size = 12) +
          theme(panel.grid.minor = element_blank(),
                plot.title = element_text(face = "bold")))

cat("Rows:", nrow(panel), "| Periods:", n_distinct(panel$form_date), "\n")


# =====================================================================
# 1. DECILE SORTS
# =====================================================================
# t-stats come from the TIME SERIES of monthly spreads (n ~ 360), never
# from pooled stocks. Pooling treats every stock in a month as an
# independent observation and inflates t-stats by roughly sqrt(N_stocks).

sort_one <- function(fac) {
  rk <- paste0("rk_", fac)
  panel |>
    filter(!is.na(.data[[rk]]), !is.na(fwd_12m)) |>
    group_by(form_date) |>
    filter(n() >= 100) |>
    mutate(d = ntile(.data[[rk]], 10)) |>
    group_by(form_date, d) |>
    summarise(ret = mean(fwd_12m), .groups = "drop") |>
    mutate(feature = fac)
}

sorts <- map_dfr(FEATURES, sort_one)

spreads <- sorts |>
  filter(d %in% c(1, 10)) |>
  pivot_wider(names_from = d, values_from = ret, names_prefix = "d") |>
  mutate(spread = d10 - d1) |>
  group_by(feature) |>
  summarise(
    avg_spread = mean(spread, na.rm = TRUE),
    t_stat     = mean(spread, na.rm = TRUE) /
                 (sd(spread, na.rm = TRUE) / sqrt(n())),
    hit_rate   = mean(spread > 0, na.rm = TRUE),
    n_periods  = n()
  ) |>
  arrange(desc(abs(t_stat)))

print(spreads, n = 30)
write_csv(spreads, "output/decile_spreads.csv")


# =====================================================================
# 2. CORRELATION STRUCTURE
# =====================================================================
cor_mat <- panel |> select(all_of(RK)) |>
  rename_with(~ str_remove(.x, "rk_")) |>
  cor(use = "pairwise.complete.obs")

fig_cor <- as_tibble(cor_mat, rownames = "v1") |>
  pivot_longer(-v1, names_to = "v2", values_to = "r") |>
  ggplot(aes(v1, v2, fill = r)) +
  geom_tile() +
  scale_fill_gradient2(low = "firebrick", mid = "white",
                       high = "steelblue4", midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Cross-sectional feature correlations",
       subtitle = "Clustering within value and profitability blocks motivates multivariate estimation",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7))

ggsave("figures/correlations.png", fig_cor, width = 10, height = 9, dpi = 300)


# =====================================================================
# 3. FAMA-MACBETH
# =====================================================================
# Stage 1: one cross-sectional regression per month.
# Stage 2: average the coefficients, t-test with Newey-West.
# NW lag 12 because 12-month overlapping targets induce autocorrelation
# in the coefficient series. Plain SEs would overstate significance.

fm_formula <- as.formula(paste("fwd_12m ~", paste(RK, collapse = " + ")))

stage1 <- panel |>
  filter(!is.na(fwd_12m)) |>
  group_by(form_date) |>
  filter(n() >= 100) |>
  group_modify(~ tidy(lm(fm_formula, data = .x))) |>
  ungroup()

stage2 <- function(s1) {
  s1 |>
    filter(term != "(Intercept)", !is.na(estimate)) |>
    group_by(term) |>
    filter(n() >= 24) |>          # need enough periods to average
    group_modify(function(d, k) {
      m  <- lm(estimate ~ 1, data = d)
      L  <- min(12, max(1, floor(nrow(d) / 4)))   # NW lag can't exceed n
      nw <- coeftest(m, vcov = NeweyWest(m, lag = L, prewhite = FALSE))
      tibble(coef = nw[1,1], se = nw[1,2], t_nw = nw[1,3], n = nrow(d))
    }) |> ungroup()
}

fm <- stage2(stage1) |> arrange(desc(abs(t_nw)))
print(fm, n = 30)

r2 <- panel |>
  filter(!is.na(fwd_12m)) |>
  group_by(form_date) |>
  filter(n() >= 100) |>
  group_modify(~ glance(lm(fm_formula, data = .x))) |>
  ungroup()

cat("Median per-period R2:", round(median(r2$r.squared, na.rm=TRUE), 4),
    "| adj:", round(median(r2$adj.r.squared, na.rm=TRUE), 4), "\n")


# =====================================================================
# 4. SECTOR CONTROLS
# =====================================================================
# Tech is high-ROE and high-valuation; energy is the reverse. Factors
# that die here were sector bets, not stock selection. Reporting that is
# a stronger finding than reporting the raw coefficient.

fm_sec_formula <- as.formula(
  paste("fwd_12m ~", paste(RK, collapse = " + "), "+ factor(gsector)"))

sec_data <- panel |>
  filter(!is.na(fwd_12m), !is.na(gsector)) |>
  group_by(form_date) |>
  filter(n() >= 200,
         n_distinct(gsector) >= 5,
         sum(complete.cases(pick(all_of(RK)))) >= 200) |>
  ungroup()

cat("Sector regression:", n_distinct(sec_data$form_date), "usable periods\n")

stage1_sec <- sec_data |>
  group_by(form_date) |>
  group_modify(~ tidy(lm(fm_sec_formula, data = .x))) |>
  ungroup()

fm_sec <- stage2(stage1_sec |> filter(term %in% RK))

comparison <- fm |> select(term, coef_raw = coef, t_raw = t_nw) |>
  left_join(fm_sec |> select(term, coef_sec = coef, t_sec = t_nw), by = "term") |>
  arrange(desc(abs(t_raw)))

print(comparison, n = 30)
write_csv(comparison, "output/famamacbeth.csv")


# =====================================================================
# 5. ELASTIC NET — which features survive penalization
# =====================================================================
cv_data <- panel |> filter(!is.na(fwd_12m)) |> drop_na(all_of(RK))
x <- as.matrix(cv_data[, RK]); y <- cv_data$fwd_12m

enet <- glmnet(x, y, alpha = 0.5)
coef_path <- as.matrix(coef(enet)) |> as_tibble(rownames = "term") |>
  pivot_longer(-term, names_to = "step", values_to = "coef") |>
  filter(term != "(Intercept)") |>
  mutate(lambda = enet$lambda[as.integer(str_remove(step, "s"))+1])

surviving <- coef_path |>
  filter(coef != 0) |>
  group_by(term) |>
  summarise(max_lambda_alive = max(lambda)) |>
  arrange(desc(max_lambda_alive))

print(surviving, n = 30)


# =====================================================================
# 6. FIGURES
# =====================================================================
top6 <- spreads |> slice_max(abs(t_stat), n = 6) |> pull(feature)

fig_dec <- sorts |>
  filter(feature %in% top6) |>
  group_by(feature, d) |>
  summarise(ret = mean(ret), se = sd(ret)/sqrt(n()), .groups = "drop") |>
  ggplot(aes(factor(d), ret)) +
  geom_col(fill = "steelblue4") +
  geom_errorbar(aes(ymin = ret-1.96*se, ymax = ret+1.96*se),
                width = .2, colour = "grey30") +
  facet_wrap(~ feature, scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Forward 12-month return by feature decile",
       x = "Decile (10 = highest)", y = NULL)

ggsave("figures/decile_sorts.png", fig_dec, width = 11, height = 7, dpi = 300)

fig_coef <- comparison |>
  pivot_longer(c(coef_raw, coef_sec), names_to = "model", values_to = "coef") |>
  mutate(model = if_else(model == "coef_raw", "No controls", "Sector controls"),
         label = str_remove(term, "rk_")) |>
  ggplot(aes(coef, fct_reorder(label, coef), colour = model)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(size = 2.5, position = position_dodge(width = .5)) +
  scale_colour_manual(values = c("No controls" = "grey55",
                                 "Sector controls" = "steelblue4")) +
  labs(title = "Fama-MacBeth coefficients", x = "Coefficient",
       y = NULL, colour = NULL)

ggsave("figures/coefficients.png", fig_coef, width = 9, height = 8, dpi = 300)


# =====================================================================
# 7. GROUND TRUTH CHECK — synthetic data only
# =====================================================================
# This is the entire point of the synthetic run. Does the pipeline
# recover what was planted?

if (IS_SYNTH && file.exists("data/TRUE_BETAS.rds")) {
  TRUE_BETAS <- read_rds("data/TRUE_BETAS.rds")

  check <- fm |>
    mutate(true_beta = as.numeric(TRUE_BETAS[term]),
           true_beta = replace_na(true_beta, 0),
           error = coef - true_beta,
           flagged = abs(t_nw) > 2) |>
    select(term, true_beta, estimated = coef, error, t_nw, flagged) |>
    arrange(desc(abs(true_beta)), desc(abs(t_nw)))

  cat("\n=============== GROUND TRUTH COMPARISON ===============\n")
  print(check, n = 30)

  planted <- check |> filter(true_beta != 0)
  zeros   <- check |> filter(true_beta == 0)

  cat("\nPlanted features recovered with |t|>2: ",
      sum(planted$flagged), "/", nrow(planted), "\n")
  cat("Zero-beta features falsely flagged:    ",
      sum(zeros$flagged), "/", nrow(zeros), "\n")
  cat("Mean absolute error on planted betas:  ",
      round(mean(abs(planted$error)), 4), "\n")
  cat("Correlation, true vs estimated:        ",
      round(cor(check$true_beta, check$estimated), 3), "\n")

  cat("\nPASS looks like:\n",
      " - most planted features flagged\n",
      " - false positives at roughly 5% of zero-beta features\n",
      " - mean absolute error well under 0.01\n",
      " - correlation above 0.9\n",
      "\nIf false positives are high, the ranking or grouping is wrong.\n",
      "If nothing is recovered, features and target are misaligned.\n")

  write_csv(check, "output/ground_truth_check.csv")
}
# 1. Do univariate sorts find what the multivariate regression missed?
spreads |> filter(feature %in% c("ep","fcfp","leverage","roe","mom_12_1","asset_growth"))

# 2. Is the BLOCK recovered even if individuals aren't?
fm |> filter(term %in% paste0("rk_", c("ep","bp","sp","fcfp","ebitda_ev"))) |>
  summarise(block_total = sum(coef))
# true total planted in the value block = 0.022 + 0.018 = 0.040

# 3. How much data does each regression actually use?
panel |> filter(!is.na(fwd_12m)) |> group_by(form_date) |>
  summarise(n_all = n(),
            n_complete = sum(complete.cases(pick(all_of(RK))))) |>
  summarise(across(c(n_all, n_complete), median))

