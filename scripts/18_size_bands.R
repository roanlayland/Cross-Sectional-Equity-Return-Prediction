# =====================================================================
# AlphaQuant — Where in the size distribution does the signal work?
#
# Tests market cap BANDS (min and max), using RELATIVE size rather than
# fixed dollar thresholds. $2B in 1994 was a large company; today it is
# small. Fixed cutoffs make the definition of "large cap" drift across
# a 30-year sample.
#
# Measures three things per band:
#   1. Rank IC        — pure predictive power, no portfolio construction
#   2. Decile spread  — the tradeable version
#   3. Portfolio alpha — after costs and beta
#
# ⚠️ Also validates: does the best band in 2010-2017 stay best in
# 2018-2024? Sector selection already failed this test (rank correlation
# -0.055). Size may or may not.
# =====================================================================

library(tidyverse)
library(lubridate)
library(scales)

scored <- read_rds("output/all_scores.rds")
msf    <- read_rds("data/msf.rds")
dl     <- read_rds("data/delist.rds")
ff     <- read_rds("data/ff_factors.rds") |>
  mutate(month_date = ceiling_date(date, "month") - 1)

rets <- msf |>
  mutate(ym = floor_date(date, "month")) |>
  left_join(dl |> mutate(ym = floor_date(dlstdt, "month")) |>
              select(permno, ym, dlret) |>
              distinct(permno, ym, .keep_all = TRUE), by = c("permno","ym")) |>
  mutate(ret_adj = case_when(
    !is.na(ret) & !is.na(dlret) ~ (1+ret)*(1+dlret) - 1,
     is.na(ret) & !is.na(dlret) ~ dlret, TRUE ~ ret)) |>
  transmute(permno, month_date = ceiling_date(ym, "month") - 1, ret_adj) |>
  filter(!is.na(ret_adj))

fwd1 <- msf |>
  mutate(month_date = ceiling_date(floor_date(date,"month"),"month")-1) |>
  arrange(permno, month_date) |> group_by(permno) |>
  mutate(fwd_1m = lead(ret, 1)) |> ungroup() |>
  select(permno, month_date, fwd_1m)


# =====================================================================
# 1. Assign relative size bands each month
# =====================================================================
# Deciles of market cap WITHIN each month, so band definitions track
# the market rather than drifting with inflation and growth.

banded <- scored |>
  group_by(form_date) |>
  mutate(size_dec = ntile(mktcap, 10),
         size_pct = percent_rank(mktcap)) |>
  ungroup()

cat("Median market cap by size decile ($M):\n")
banded |> group_by(size_dec) |>
  summarise(median_cap = median(mktcap),
            p10 = quantile(mktcap, .1), p90 = quantile(mktcap, .9),
            n_per_month = n()/n_distinct(banded$form_date)) |>
  mutate(across(c(median_cap, p10, p90), ~ comma(round(.x))),
         n_per_month = round(n_per_month)) |> print(n = 10)


# =====================================================================
# 2. Rank IC by size decile — the cleanest measure
# =====================================================================
# No portfolio construction, no costs, no beta. Just: does the score
# order returns correctly within this slice of the market?

ic_by_size <- banded |>
  left_join(fwd1, by = c("permno","form_date"="month_date")) |>
  group_by(form_date, size_dec) |>
  filter(sum(!is.na(fwd_1m)) >= 30) |>
  summarise(ic = cor(score, fwd_1m, method="spearman", use="complete.obs"),
            .groups = "drop") |>
  group_by(size_dec) |>
  summarise(mean_ic = mean(ic, na.rm=TRUE),
            t_stat  = mean(ic, na.rm=TRUE)/(sd(ic, na.rm=TRUE)/sqrt(n())),
            hit     = mean(ic > 0, na.rm=TRUE),
            n_mo    = n())

cat("\n========== RANK IC BY SIZE DECILE ==========\n")
cat("Decile 1 = smallest, 10 = largest\n\n")
print(ic_by_size |> mutate(mean_ic = round(mean_ic,4),
                           t_stat = round(t_stat,2),
                           hit = percent(hit, accuracy=0.1)), n = 10)


# =====================================================================
# 3. Decile spread by size band
# =====================================================================
spread_by_size <- banded |>
  left_join(fwd1, by = c("permno","form_date"="month_date")) |>
  group_by(form_date, size_dec) |>
  filter(sum(!is.na(fwd_1m)) >= 30) |>
  mutate(q = ntile(score, 5)) |>
  filter(q %in% c(1,5)) |>
  group_by(form_date, size_dec, q) |>
  summarise(r = mean(fwd_1m, na.rm=TRUE), .groups="drop") |>
  pivot_wider(names_from=q, values_from=r, names_prefix="q") |>
  mutate(spread = q5 - q1) |>
  group_by(size_dec) |>
  summarise(monthly = mean(spread, na.rm=TRUE),
            annual  = (1+mean(spread, na.rm=TRUE))^12 - 1,
            t_stat  = mean(spread,na.rm=TRUE)/(sd(spread,na.rm=TRUE)/sqrt(n())),
            n_mo    = n())

cat("\n========== QUINTILE SPREAD BY SIZE DECILE ==========\n")
print(spread_by_size |> mutate(monthly = percent(monthly, accuracy=0.01),
                               annual = percent(annual, accuracy=0.1),
                               t_stat = round(t_stat,2)), n = 10)


# =====================================================================
# 4. Portfolios within size BANDS (min and max)
# =====================================================================
band_backtest <- function(lo_pct, hi_pct, n_side, hold_mo, cost_bps, label,
                          date_from = NULL) {
  d <- banded |> filter(size_pct >= lo_pct, size_pct <= hi_pct)
  if (!is.null(date_from)) d <- d |> filter(form_date >= date_from)

  picks <- d |> group_by(form_date) |>
    filter(n() >= n_side * 2) |>
    slice_max(score, n = n_side) |> ungroup()
  if (nrow(picks) < 100) return(NULL)

  held <- picks |> select(cohort = form_date, permno) |>
    crossing(k = 1:hold_mo) |>
    mutate(month_date = ceiling_date(cohort %m+% months(k), "month") - 1) |>
    left_join(rets, by = c("permno","month_date")) |>
    mutate(ret_adj = replace_na(ret_adj, 0)) |>
    group_by(cohort, month_date) |>
    summarise(ret = mean(ret_adj), .groups="drop")

  p <- held |> group_by(month_date) |>
    summarise(ret = mean(ret), nc = n_distinct(cohort), .groups="drop") |>
    filter(nc == hold_mo) |>
    left_join(ff |> select(month_date, mktrf, smb, hml, rmw, cma, umd, rf),
              by="month_date") |> drop_na(mktrf)
  if (nrow(p) < 24) return(NULL)

  rn <- p$ret - (1/hold_mo)*2*cost_bps/1e4
  ex <- rn - p$rf
  capm <- lm(ex ~ p$mktrf)
  ff6  <- lm(ex ~ p$mktrf+p$smb+p$hml+p$rmw+p$cma+p$umd)

  tibble(band = label, n_mo = nrow(p),
         ann_ret = prod(1+rn)^(12/length(rn)) - 1,
         mkt_ret = prod(1+p$mktrf+p$rf)^(12/nrow(p)) - 1,
         vol = sd(rn)*sqrt(12),
         sharpe = mean(ex)*12/(sd(rn)*sqrt(12)),
         mkt_sharpe = mean(p$mktrf)*12/(sd(p$mktrf)*sqrt(12)),
         beta = coef(capm)[2],
         capm_a = coef(capm)[1]*12, capm_t = summary(capm)$coef[1,3],
         ff6_a = coef(ff6)[1]*12,  ff6_t = summary(ff6)$coef[1,3])
}

bands <- tribble(
  ~lo,  ~hi,  ~label,
  0.00, 0.20, "P0-20 (micro)",
  0.20, 0.40, "P20-40 (small)",
  0.40, 0.60, "P40-60 (mid)",
  0.60, 0.80, "P60-80 (large)",
  0.80, 1.00, "P80-100 (mega)",
  0.50, 1.00, "P50-100 (top half)",
  0.60, 0.95, "P60-95 (large, ex-mega)",
  0.70, 1.00, "P70-100",
  0.00, 1.00, "P0-100 (all)"
)

cat("\n========== PORTFOLIOS BY SIZE BAND ==========\n")
cat("50 names, 6-month hold, 20bp\n\n")

band_res <- pmap_dfr(bands, function(lo, hi, label)
  band_backtest(lo, hi, 50, 6, 20, label))

print(band_res |> mutate(across(c(ann_ret, mkt_ret, vol, capm_a, ff6_a),
                                ~ percent(.x, accuracy=0.1)),
                         across(c(sharpe, mkt_sharpe, beta, capm_t, ff6_t),
                                ~ round(.x,2))), width = Inf)


# =====================================================================
# 5. ⚠️ VALIDATION — does the best band persist?
# =====================================================================
SPLIT <- as.Date("2018-01-01")

ic_half <- function(from, to) {
  banded |> filter(form_date >= from, form_date < to) |>
    left_join(fwd1, by = c("permno","form_date"="month_date")) |>
    group_by(form_date, size_dec) |>
    filter(sum(!is.na(fwd_1m)) >= 30) |>
    summarise(ic = cor(score, fwd_1m, method="spearman",
                       use="complete.obs"), .groups="drop") |>
    group_by(size_dec) |>
    summarise(ic = mean(ic, na.rm=TRUE),
              t = mean(ic, na.rm=TRUE)/(sd(ic, na.rm=TRUE)/sqrt(n())))
}

h1 <- ic_half(as.Date("2010-01-01"), SPLIT)
h2 <- ic_half(SPLIT, as.Date("2026-01-01"))

cat("\n========== DOES THE SIZE PATTERN PERSIST? ==========\n")
per <- h1 |> select(size_dec, ic1 = ic, t1 = t) |>
  left_join(h2 |> select(size_dec, ic2 = ic, t2 = t), by="size_dec")
print(per |> mutate(across(c(ic1, ic2), ~ round(.x,4)),
                    across(c(t1, t2), ~ round(.x,2))), n = 10)

cat("\nRank correlation across halves:",
    round(cor(rank(-per$ic1), rank(-per$ic2), method="spearman"), 3), "\n")
cat("(Sector selection scored -0.055 on this test, i.e. pure noise.)\n")

best_h1 <- per$size_dec[which.max(per$ic1)]
cat("\nBest decile in 2010-2017: ", best_h1,
    " -> its rank in 2018-2024: ",
    which(order(-per$ic2) == best_h1), " of 10\n", sep="")

cat("\n========== OUT-OF-SAMPLE BAND TEST (2018-2024) ==========\n")
oos <- bind_rows(
  band_backtest((best_h1-1)/10, best_h1/10, 30, 6, 20,
                paste0("Best band from H1 (dec ", best_h1, ")"), SPLIT),
  band_backtest(0, 1, 50, 6, 20, "All sizes", SPLIT)
)
print(oos |> mutate(across(c(ann_ret, mkt_ret, vol, capm_a, ff6_a),
                           ~ percent(.x, accuracy=0.1)),
                    across(c(sharpe, mkt_sharpe, beta, capm_t, ff6_t),
                           ~ round(.x,2))), width = Inf)


# =====================================================================
# 6. Figures
# =====================================================================
theme_set(theme_minimal(base_size = 11))

fig1 <- ic_by_size |>
  ggplot(aes(factor(size_dec), mean_ic, fill = abs(t_stat) > 2)) +
  geom_col() + geom_hline(yintercept = 0, colour = "grey40") +
  scale_fill_manual(values = c("TRUE"="steelblue4","FALSE"="grey75"),
                    name = "|t| > 2") +
  labs(title = "Predictive power by company size",
       subtitle = "Monthly rank IC within each size decile. 1 = smallest.",
       x = "Size decile", y = "Mean rank IC")

fig2 <- spread_by_size |>
  ggplot(aes(factor(size_dec), annual, fill = abs(t_stat) > 2)) +
  geom_col() + geom_hline(yintercept = 0, colour = "grey40") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("TRUE"="steelblue4","FALSE"="grey75"),
                    name = "|t| > 2") +
  labs(title = "Top-minus-bottom quintile spread by size decile",
       x = "Size decile", y = "Annualised spread")

ggsave("figures/ic_by_size.png", fig1, width = 8, height = 5, dpi = 300)
ggsave("figures/spread_by_size.png", fig2, width = 8, height = 5, dpi = 300)

write_csv(ic_by_size,     "output/ic_by_size.csv")
write_csv(spread_by_size, "output/spread_by_size.csv")
write_csv(band_res,       "output/size_bands.csv")

cat("\nDone.\n")


# =====================================================================
# HOW TO READ THIS
# =====================================================================
# Section 2 (rank IC by decile) is the cleanest evidence. It has no
# portfolio construction, no costs, no beta -- just whether the score
# orders returns correctly within that slice.
#
# A MONOTONIC gradient is believable: literature consistently finds
# anomalies stronger in smaller stocks, where arbitrage is harder.
#
# A single decile spiking with neighbours flat is noise. Nine bands
# means roughly one false positive at the 5% level.
#
# Section 5 is the test that matters. Sector selection scored -0.055
# on the equivalent test, meaning it was pure noise. If size scores
# above ~0.5, the size effect is real and worth reporting as a finding
# rather than a parameter choice.
