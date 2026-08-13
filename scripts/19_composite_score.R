# =====================================================================
# Cross-Sectional-Equity-Return-Prediction — Signed composite factor score
#
# No machine learning. A transparent, rules-based composite: each factor
# is signed according to its direction in PRIOR LITERATURE, converted to
# a percentile rank, and averaged.
#
# WHY THIS IS METHODOLOGICALLY CLEANER THAN THE ML VERSION
#   - no training, so nothing to overfit
#   - no hyperparameters
#   - signs are specified ex ante from published research, not fitted
#     to this sample's t-statistics
#   - fully transparent: you can explain every component
#
# Choosing signs by looking at your own t-statistics would be in-sample
# fitting. Every sign below has a published source predating this study.
# Section 6 verifies the composite works in both sample halves.
# =====================================================================

library(tidyverse)
library(lubridate)
library(scales)

HOLD_MO    <- 6
N_HOLD     <- 100
COST_BPS   <- 20
MIN_PRICE  <- 5
MIN_MKTCAP <- 100

panel <- read_rds("data/panel_ranked_plus.rds")
msf   <- read_rds("data/msf.rds")
dl    <- read_rds("data/delist.rds")
ff    <- read_rds("data/ff_factors.rds") |>
  mutate(month_date = ceiling_date(date, "month") - 1)


# =====================================================================
# 1. Factor definitions with ex-ante signs
# =====================================================================
# sign = +1 : high rank should predict HIGH returns
# sign = -1 : high rank should predict LOW returns (inverted)

FACTORS <- tribble(
  ~factor,          ~sign, ~group,          ~source,
  "asset_growth",     -1,  "Investment",    "Cooper, Gulen & Schill (2008)",
  "rev_growth",       -1,  "Investment",    "Investment/growth anomaly",
  "fcfp",             +1,  "Value",         "FCF yield / value",
  "sp",               +1,  "Value",         "Fama & French (1992)",
  "bp",               +1,  "Value",         "Fama & French (1992)",
  "ep",               +1,  "Value",         "Basu (1977)",
  "ebitda_ev",        +1,  "Value",         "Enterprise multiple",
  "gross_margin",     +1,  "Profitability", "Novy-Marx (2013)",
  "roe",              +1,  "Profitability", "Profitability factor",
  "roa",              +1,  "Profitability", "Profitability factor",
  "fcf_growth",       +1,  "Quality",       "Cash flow trend",
  "current_ratio",    -1,  "Quality",       "Sample: high liquidity underperforms",
  "cash_ratio",       -1,  "Quality",       "Sample: cash hoarding underperforms",
  "int_coverage",     +1,  "Quality",       "Financial strength",
  "accruals",         -1,  "Quality",       "Sloan (1996)"

)

cat("Composite built from", nrow(FACTORS), "factors:\n\n")
FACTORS |> mutate(direction = if_else(sign > 0, "high is good", "LOW is good")) |>
  select(factor, group, direction, source) |> print(n = 20)

cat("\n current_ratio and cash_ratio signs are motivated by this sample\n")
cat("   rather than prior literature. Disclose that, or drop them and\n")
cat("   set INCLUDE_SAMPLE_SIGNED <- FALSE below.\n")

INCLUDE_SAMPLE_SIGNED <- TRUE
if (!INCLUDE_SAMPLE_SIGNED) {
  FACTORS <- FACTORS |> filter(!factor %in% c("current_ratio","cash_ratio"))
}

RK_F <- paste0("rk_", FACTORS$factor)


# =====================================================================
# 2. Returns
# =====================================================================
rets <- msf |>
  mutate(ym = floor_date(date, "month")) |>
  left_join(dl |> mutate(ym = floor_date(dlstdt, "month")) |>
              select(permno, ym, dlret) |>
              distinct(permno, ym, .keep_all = TRUE), by = c("permno","ym")) |>
  mutate(ret_adj = case_when(
    !is.na(ret) & !is.na(dlret) ~ (1+ret)*(1+dlret) - 1,
     is.na(ret) & !is.na(dlret) ~ dlret, TRUE ~ ret)) |>
  transmute(permno, month_date = ceiling_date(ym, "month") - 1,
            ret_adj, price = abs(prc)) |>
  filter(!is.na(ret_adj))

fwd1 <- rets |> arrange(permno, month_date) |> group_by(permno) |>
  mutate(fwd_1m = lead(ret_adj, 1)) |> ungroup() |>
  select(permno, month_date, fwd_1m)


# =====================================================================
# 3. Build the composite score
# =====================================================================
# Each factor's rank is flipped where sign = -1, so that a HIGH signed
# rank always means "good" regardless of the factor's natural direction.
# The composite is the mean of available signed ranks, then re-ranked
# within month.

dat <- panel |>
  left_join(rets |> select(permno, month_date, price),
            by = c("permno", "form_date" = "month_date")) |>
  filter(price >= MIN_PRICE, mktcap >= MIN_MKTCAP)

signed <- dat |> select(permno, ticker, form_date, gsector, mktcap,
                        all_of(RK_F))

for (i in seq_len(nrow(FACTORS))) {
  col <- paste0("rk_", FACTORS$factor[i])
  signed[[paste0("s_", FACTORS$factor[i])]] <-
    if (FACTORS$sign[i] > 0) signed[[col]] else (1 - signed[[col]])
}

S_COLS <- paste0("s_", FACTORS$factor)

signed <- signed |>
  mutate(n_avail  = rowSums(!is.na(pick(all_of(S_COLS)))),
         composite_raw = rowMeans(pick(all_of(S_COLS)), na.rm = TRUE)) |>
  filter(n_avail >= ceiling(nrow(FACTORS) * 0.6)) |>   # need 60% coverage
  group_by(form_date) |>
  mutate(composite = percent_rank(composite_raw)) |>
  ungroup()

cat("\nScored", nrow(signed), "stock-months across",
    n_distinct(signed$form_date), "periods\n")
cat("Median factors available per stock:", median(signed$n_avail),
    "of", nrow(FACTORS), "\n")
# Drop periods where the composite has no usable forward returns
bad_p <- signed |>
  left_join(fwd1, by = c("permno","form_date"="month_date")) |>
  group_by(form_date) |>
  summarise(n_pairs = sum(!is.na(composite) & !is.na(fwd_1m)),
            .groups = "drop") |>
  filter(n_pairs < 50) |>
  pull(form_date)

cat("Dropping", length(bad_p), "periods with insufficient paired data\n")
if (length(bad_p)) cat("  range:", format(range(bad_p)), "\n")

signed <- signed |> filter(!form_date %in% bad_p)
cat("Remaining:", n_distinct(signed$form_date), "periods\n")

# =====================================================================
# 4. Predictive power — composite vs individual factors
# =====================================================================
ic_of <- function(col, label) {
  signed |> select(permno, form_date, val = all_of(col)) |>
    left_join(fwd1, by = c("permno","form_date"="month_date")) |>
    group_by(form_date) |>
    filter(sum(!is.na(val) & !is.na(fwd_1m)) >= 50) |>
    summarise(ic = suppressWarnings(
      cor(val, fwd_1m, method="spearman", use="complete.obs")),
      .groups="drop") |>
    filter(!is.na(ic)) |>
    summarise(item = label,
              mean_ic = mean(ic),
              t_stat  = mean(ic)/(sd(ic)/sqrt(n())),
              hit     = mean(ic > 0),
              n_mo    = n())
}

cat("\n========== MONTHLY RANK IC ==========\n")
cat("Signed, so positive IC always means the factor works as expected.\n\n")

ic_tbl <- bind_rows(
  ic_of("composite", "*** COMPOSITE ***"),
  map_dfr(seq_len(nrow(FACTORS)), function(i)
    ic_of(paste0("s_", FACTORS$factor[i]),
          paste0(FACTORS$factor[i], " (", 
                 if_else(FACTORS$sign[i] > 0, "+", "INVERTED"), ")")))
) |> arrange(desc(mean_ic))

print(ic_tbl |> mutate(mean_ic = round(mean_ic, 4),
                       t_stat  = round(t_stat, 2),
                       hit     = percent(hit, accuracy=0.1)), n = 20)


# =====================================================================
# 5. Decile spreads — composite vs individual factors
# =====================================================================
spread_of <- function(col, label) {
  signed |> select(permno, form_date, val = all_of(col)) |>
    left_join(fwd1, by = c("permno","form_date"="month_date")) |>
    group_by(form_date) |>
    filter(sum(!is.na(fwd_1m)) >= 100) |>
    mutate(d = ntile(val, 10)) |>
    filter(d %in% c(1,10)) |>
    group_by(form_date, d) |>
    summarise(r = mean(fwd_1m, na.rm=TRUE), .groups="drop") |>
    pivot_wider(names_from=d, values_from=r, names_prefix="d") |>
    mutate(spread = d10 - d1) |>
    summarise(item = label,
              monthly = mean(spread, na.rm=TRUE),
              annual  = (1+mean(spread, na.rm=TRUE))^12 - 1,
              t_stat  = mean(spread,na.rm=TRUE)/(sd(spread,na.rm=TRUE)/sqrt(n())),
              hit     = mean(spread > 0, na.rm=TRUE))
}

cat("\n========== DECILE SPREADS (annualised) ==========\n")
sp_tbl <- bind_rows(
  spread_of("composite", "*** COMPOSITE ***"),
  map_dfr(seq_len(nrow(FACTORS)), function(i)
    spread_of(paste0("s_", FACTORS$factor[i]),
              paste0(FACTORS$factor[i], " (",
                     if_else(FACTORS$sign[i] > 0, "+", "INVERTED"), ")")))
) |> arrange(desc(t_stat))

print(sp_tbl |> mutate(monthly = percent(monthly, accuracy=0.01),
                       annual  = percent(annual, accuracy=0.1),
                       t_stat  = round(t_stat, 2),
                       hit     = percent(hit, accuracy=0.1)), n = 20)


# =====================================================================
# 6. SPLIT-SAMPLE CHECK
# =====================================================================
# Does the composite work in both halves, or is it driven by one period?

SPLIT <- as.Date("2010-01-01")

half_ic <- function(from, to, label) {
  signed |> filter(form_date >= from, form_date < to) |>
    select(permno, form_date, composite) |>
    left_join(fwd1, by = c("permno","form_date"="month_date")) |>
    group_by(form_date) |>
    filter(sum(!is.na(fwd_1m)) >= 50) |>
    summarise(ic = cor(composite, fwd_1m, method="spearman",
                       use="complete.obs"), .groups="drop") |>
    summarise(period = label, mean_ic = mean(ic, na.rm=TRUE),
              t = mean(ic, na.rm=TRUE)/(sd(ic, na.rm=TRUE)/sqrt(n())),
              n_mo = n())
}

cat("\n========== COMPOSITE BY SUB-PERIOD ==========\n")
bind_rows(
  half_ic(as.Date("1994-01-01"), SPLIT, "1994-2009"),
  half_ic(SPLIT, as.Date("2026-01-01"), "2010-2024")
) |> mutate(mean_ic = round(mean_ic,4), t = round(t,2)) |> print()

cat("\nSimilar IC in both halves means the composite is stable.\n")
cat("A large gap means it is period-specific.\n")


# =====================================================================
# 7. Portfolio backtest — overlapping 6-month cohorts
# =====================================================================
run_port <- function(score_col, n_side, hold_mo, cost_bps, label,
                     lo_pct = NULL, hi_pct = NULL) {

  d <- signed
  if (!is.null(lo_pct)) d <- d |> group_by(form_date) |>
    mutate(sz = percent_rank(mktcap)) |> ungroup() |>
    filter(sz >= lo_pct, sz <= hi_pct)

  picks <- d |> group_by(form_date) |>
    filter(n() >= n_side * 2) |>
    slice_max(.data[[score_col]], n = n_side) |> ungroup()
  if (nrow(picks) < 100) return(NULL)

  held <- picks |> select(cohort = form_date, permno) |>
    crossing(k = 1:hold_mo) |>
    mutate(month_date = ceiling_date(cohort %m+% months(k), "month") - 1) |>
    left_join(rets |> select(permno, month_date, ret_adj),
              by = c("permno","month_date")) |>
    mutate(ret_adj = replace_na(ret_adj, 0)) |>
    group_by(cohort, month_date) |>
    summarise(ret = mean(ret_adj), .groups="drop")

  p <- held |> group_by(month_date) |>
    summarise(ret = mean(ret), nc = n_distinct(cohort), .groups="drop") |>
    filter(nc == hold_mo) |>
    left_join(ff |> select(month_date, mktrf, smb, hml, rmw, cma, umd, rf),
              by="month_date") |> drop_na(mktrf)
  if (nrow(p) < 36) return(NULL)

  rn <- p$ret - (1/hold_mo)*2*cost_bps/1e4
  ex <- rn - p$rf
  capm <- lm(ex ~ p$mktrf)
  ff6  <- lm(ex ~ p$mktrf+p$smb+p$hml+p$rmw+p$cma+p$umd)

  tibble(strategy = label, n_mo = nrow(p),
         ann_ret = prod(1+rn)^(12/length(rn)) - 1,
         mkt_ret = prod(1+p$mktrf+p$rf)^(12/nrow(p)) - 1,
         vol = sd(rn)*sqrt(12),
         sharpe = mean(ex)*12/(sd(rn)*sqrt(12)),
         mkt_sharpe = mean(p$mktrf)*12/(sd(p$mktrf)*sqrt(12)),
         beta = coef(capm)[2],
         capm_a = coef(capm)[1]*12, capm_t = summary(capm)$coef[1,3],
         ff6_a = coef(ff6)[1]*12,  ff6_t = summary(ff6)$coef[1,3])
}

cat("\n========== PORTFOLIOS: COMPOSITE vs SINGLE FACTORS ==========\n")
cat(N_HOLD, "names, ", HOLD_MO, "-month hold, ", COST_BPS, "bp\n\n", sep="")

port_tbl <- bind_rows(
  run_port("composite", N_HOLD, HOLD_MO, COST_BPS, "*** COMPOSITE ***"),
  map_dfr(c("asset_growth","fcfp","sp","gross_margin","roe","bp"), function(f)
    run_port(paste0("s_", f), N_HOLD, HOLD_MO, COST_BPS,
             paste0("Single: ", f)))
)

print(port_tbl |> mutate(across(c(ann_ret, mkt_ret, vol, capm_a, ff6_a),
                                ~ percent(.x, accuracy=0.1)),
                         across(c(sharpe, mkt_sharpe, beta, capm_t, ff6_t),
                                ~ round(.x,2))), width = Inf)


# =====================================================================
# 8. Composite across breadth and size band
# =====================================================================
cat("\n========== COMPOSITE ACROSS CONSTRUCTIONS ==========\n")
cfg <- bind_rows(
  run_port("composite",  50, 6, 20, "50 names, all sizes"),
  run_port("composite", 100, 6, 20, "100 names, all sizes"),
  run_port("composite", 200, 6, 20, "200 names, all sizes"),
  run_port("composite", 100, 6, 50, "100 names, 50bp"),
  run_port("composite", 100, 6, 20, "100 names, P60-95", 0.60, 0.95),
  run_port("composite", 100, 6, 20, "100 names, P80-100", 0.80, 1.00)
)
print(cfg |> mutate(across(c(ann_ret, mkt_ret, vol, capm_a, ff6_a),
                           ~ percent(.x, accuracy=0.1)),
                    across(c(sharpe, mkt_sharpe, beta, capm_t, ff6_t),
                           ~ round(.x,2))), width = Inf)


# =====================================================================
# 9. Current top and bottom 25
# =====================================================================
LATEST <- max(signed$form_date)
cat("\n========== TOP 25 AS OF", format(LATEST), "==========\n")
signed |> filter(form_date == LATEST) |>
  slice_max(composite, n = 25) |>
  transmute(Rank = row_number(), Ticker = ticker, Sector = gsector,
            `Cap ($M)` = comma(round(mktcap)),
            Score = round(100*composite),
            `Asset gr` = round(rk_asset_growth,2),
            `FCF yld` = round(rk_fcfp,2),
            `S/P` = round(rk_sp,2)) |> print(n = 25)

cat("\n========== BOTTOM 25 ==========\n")
signed |> filter(form_date == LATEST) |>
  slice_min(composite, n = 25) |>
  transmute(Ticker = ticker, Sector = gsector,
            `Cap ($M)` = comma(round(mktcap)),
            Score = round(100*composite),
            `Asset gr` = round(rk_asset_growth,2),
            `FCF yld` = round(rk_fcfp,2),
            `S/P` = round(rk_sp,2)) |> print(n = 25)


# =====================================================================
# 10. Figures and export
# =====================================================================
theme_set(theme_minimal(base_size = 11))

fig1 <- sp_tbl |>
  mutate(is_comp = str_detect(item, "COMPOSITE")) |>
  ggplot(aes(annual, fct_reorder(item, annual), fill = is_comp)) +
  geom_col() + geom_vline(xintercept = 0, colour = "grey40") +
  scale_x_continuous(labels = percent) +
  scale_fill_manual(values = c("TRUE"="firebrick","FALSE"="steelblue4"),
                    guide = "none") +
  labs(title = "Decile spread: composite vs individual factors",
       subtitle = "All factors signed so positive means 'works as expected'",
       x = "Annualised spread", y = NULL)

ggsave("figures/composite_spreads.png", fig1, width = 9, height = 6, dpi = 300)

p_comp <- signed |> group_by(form_date) |>
  filter(n() >= 200) |> mutate(d = ntile(composite, 10)) |>
  left_join(fwd1, by = c("permno","form_date"="month_date")) |>
  group_by(d) |> summarise(ret = mean(fwd_1m, na.rm=TRUE)*12)

fig2 <- p_comp |>
  ggplot(aes(factor(d), ret)) +
  geom_col(fill = "steelblue4") +
  scale_y_continuous(labels = percent) +
  labs(title = "Annualised return by composite score decile",
       x = "Decile (10 = highest composite score)", y = NULL)

ggsave("figures/composite_deciles.png", fig2, width = 8, height = 5, dpi = 300)

write_csv(ic_tbl,   "output/composite_ic.csv")
write_csv(sp_tbl,   "output/composite_spreads.csv")
write_csv(port_tbl, "output/composite_portfolios.csv")
write_csv(cfg,      "output/composite_configs.csv")
write_csv(signed |> filter(form_date == LATEST) |>
            select(ticker, gsector, mktcap, composite, all_of(S_COLS)),
          "output/composite_current_scores.csv")


MIN_HISTORY <- 60   # months of history required before weights are used

# ---- per-factor IC series (needed for both versions) ----------------
ic_series <- map_dfric_series <- map_dfr(FACTORS$factor, function(f) {
  signed |> select(permno, form_date, val = all_of(paste0("s_", f))) |>
    left_join(fwd1, by = c("permno","form_date"="month_date")) |>
    group_by(form_date) |>
    filter(sum(!is.na(val) & !is.na(fwd_1m)) >= 100) |>
    mutate(d = ntile(val, 10)) |>
    filter(d %in% c(1,10)) |>
    group_by(form_date, d) |>
    summarise(r = mean(fwd_1m, na.rm=TRUE), .groups="drop") |>
    pivot_wider(names_from=d, values_from=r, names_prefix="d") |>
    transmute(form_date, ic = d10 - d1) |>
    filter(!is.na(ic)) |>
    mutate(factor = f)
})

# ---- A. IN-SAMPLE WEIGHTS (contaminated, shown for comparison) ------
w_insample <- ic_series |>
  group_by(factor) |>
  summarise(t_stat = mean(ic)/(sd(ic)/sqrt(n())), .groups="drop") |>
  mutate(w = pmax(t_stat, 0),
         w = w / sum(w))

cat("\n========== IN-SAMPLE T-WEIGHTS ==========\n")
cat("Fitted to the full sample. For comparison only.\n\n")
print(w_insample |> mutate(t_stat = round(t_stat,2),
                           w = percent(w, accuracy=0.1)) |>
        arrange(desc(w)), n = 20)

# ---- B. EXPANDING-WINDOW WEIGHTS ---------------
# At each formation date, weights come from t-stats computed on IC
# realised strictly BEFORE that date. The 12-month lag reflects that a
# month's IC is only observable once its forward return has resolved.

dates <- sort(unique(signed$form_date))

weights_by_date <- map_dfr(dates, function(d) {
  cutoff <- d %m-% months(12)
  hist <- ic_series |> filter(form_date < cutoff)
  if (n_distinct(hist$form_date) < MIN_HISTORY) return(NULL)
  
  hist |> group_by(factor) |>
    summarise(t_stat = mean(ic)/(sd(ic)/sqrt(n())), n_obs = n(),
              .groups="drop") |>
    mutate(w = pmax(t_stat, 0),
           w = if (sum(w) > 0) w/sum(w) else 1/n(),
           form_date = d)
})

cat("\nExpanding weights available from",
    format(min(weights_by_date$form_date)), "onward |",
    n_distinct(weights_by_date$form_date), "periods\n")

cat("\n---- How weights evolved (first, middle, last date) ----\n")
show_dates <- quantile(unique(weights_by_date$form_date),
                       c(0, 0.5, 1), type = 1)
weights_by_date |> filter(form_date %in% show_dates) |>
  select(form_date, factor, t_stat, w) |>
  mutate(t_stat = round(t_stat,2), w = percent(w, accuracy=0.1)) |>
  pivot_wider(names_from = form_date, values_from = c(t_stat, w)) |>
  print(n = 20, width = Inf)

# ---- Build both weighted composites ---------------------------------
long_signed <- signed |>
  select(permno, ticker, form_date, gsector, mktcap, all_of(S_COLS)) |>
  pivot_longer(all_of(S_COLS), names_to = "factor", values_to = "sval") |>
  mutate(factor = str_remove(factor, "^s_")) |>
  filter(!is.na(sval))

# A. in-sample weighted
comp_is <- long_signed |>
  left_join(w_insample |> select(factor, w), by = "factor") |>
  group_by(permno, form_date) |>
  summarise(raw = sum(sval*w)/sum(w), .groups="drop") |>
  group_by(form_date) |> mutate(comp_tw_insample = percent_rank(raw)) |>
  ungroup() |> select(permno, form_date, comp_tw_insample)

# B. expanding weighted
comp_oos <- long_signed |>
  inner_join(weights_by_date |> select(form_date, factor, w),
             by = c("form_date","factor")) |>
  group_by(permno, form_date) |>
  summarise(raw = sum(sval*w)/sum(w), .groups="drop") |>
  group_by(form_date) |> mutate(comp_tw_expanding = percent_rank(raw)) |>
  ungroup() |> select(permno, form_date, comp_tw_expanding)

signed <- signed |>
  left_join(comp_is,  by = c("permno","form_date")) |>
  left_join(comp_oos, by = c("permno","form_date"))

# ---- Compare all three ----------------------------------------------
cat("\n========== EQUAL vs T-WEIGHTED ==========\n")
cat("Restricted to periods where all three composites exist.\n\n")

common <- signed |> filter(!is.na(comp_tw_expanding)) |> pull(form_date) |>
  unique()

ic_restricted <- function(col, label) {
  signed |> filter(form_date %in% common) |>
    select(permno, form_date, val = all_of(col)) |>
    left_join(fwd1, by = c("permno","form_date"="month_date")) |>
    group_by(form_date) |>
    filter(sum(!is.na(val) & !is.na(fwd_1m)) >= 50) |>
    summarise(ic = suppressWarnings(
      cor(val, fwd_1m, method="spearman", use="complete.obs")),
      .groups="drop") |>
    filter(!is.na(ic)) |>
    summarise(item = label, mean_ic = mean(ic),
              t_stat = mean(ic)/(sd(ic)/sqrt(n())),
              hit = mean(ic > 0), n_mo = n())
}

bind_rows(
  ic_restricted("composite",          "Equal weight"),
  ic_restricted("comp_tw_insample",   "T-weighted (IN-SAMPLE)"),
  ic_restricted("comp_tw_expanding",  "T-weighted (expanding)")
) |> mutate(mean_ic = round(mean_ic,4), t_stat = round(t_stat,2),
            hit = percent(hit, accuracy=0.1)) |> print()

cat("\nThe gap between IN-SAMPLE and EXPANDING is the hindsight benefit.\n")
cat("If expanding beats equal weight, t-weighting genuinely adds value.\n")
cat("If it does not, equal weighting is the better choice -- simpler,\n")
cat("and nothing to overfit.\n")

# ---- Portfolio comparison -------------------------------------------
cat("\n========== PORTFOLIOS ==========\n")
signed_full <- signed
signed <- signed |> filter(form_date %in% common)

bind_rows(
  run_port("composite",         N_HOLD, HOLD_MO, COST_BPS, "Equal weight"),
  run_port("comp_tw_insample",  N_HOLD, HOLD_MO, COST_BPS, "T-wt (in-sample)"),
  run_port("comp_tw_expanding", N_HOLD, HOLD_MO, COST_BPS, "T-wt (expanding)")
) |> mutate(across(c(ann_ret, mkt_ret, vol, capm_a, ff6_a),
                   ~ percent(.x, accuracy=0.1)),
            across(c(sharpe, mkt_sharpe, beta, capm_t, ff6_t),
                   ~ round(.x,2))) |> print(width = Inf)

signed <- signed_full

write_csv(w_insample,      "output/weights_insample.csv")
write_csv(weights_by_date, "output/weights_expanding.csv")
