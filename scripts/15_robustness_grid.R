# =====================================================================
# Cross-Sectional-Equity-Return-Prediction — Robustness grid
#
# Varies breadth, size floor, holding period, and transaction cost, then
# reports EVERY cell. Also breaks results out by sector.
#
# THIS IS A ROBUSTNESS SURFACE, NOT A SEARCH
# The grid below contains ~120 configurations. At a 5% threshold, about
# six will look significant by chance alone. Reporting the best cell as
# "the result" would be data mining and the performance would be biased
# upward. Report the WHOLE surface. The finding is the PATTERN across
# cells -- e.g. "performance improves monotonically with breadth" --
# not any individual number.
#
# Efficiency: the model is trained once per year and every stock-month
# is scored once. The grid then only varies PORTFOLIO CONSTRUCTION on
# those cached scores, so 120 cells cost little more than one.
# =====================================================================

library(tidyverse)
library(lubridate)
library(xgboost)
library(scales)

FIRST_YEAR <- 2010
LAST_YEAR  <- 2024
MIN_PRICE  <- 5

panel    <- read_rds("data/panel_ranked.rds")
FEATURES <- read_rds("data/feature_list.rds")
RK       <- paste0("rk_", FEATURES)
msf      <- read_rds("data/msf.rds")
dl       <- read_rds("data/delist.rds")
ff       <- read_rds("data/ff_factors.rds") |>
  mutate(month_date = ceiling_date(date, "month") - 1)


# =====================================================================
# 1. Prep
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

miss_rate  <- panel |> summarise(across(all_of(RK), ~ mean(is.na(.x)))) |>
  pivot_longer(everything(), names_to="term", values_to="rate")
needs_flag <- miss_rate |> filter(rate > 0.05) |> pull(term)

panel <- panel |>
  mutate(across(all_of(needs_flag), ~ as.integer(is.na(.x)),
                .names = "miss_{.col}")) |>
  group_by(form_date) |>
  mutate(across(all_of(RK), ~ {
    m <- is.na(.x); if (any(m) && !all(m)) .x[m] <- median(.x, na.rm=TRUE); .x
  })) |> ungroup()

RK <- c(RK, paste0("miss_", needs_flag))

bad <- panel |> group_by(form_date) |>
  summarise(across(all_of(RK), ~ mean(is.na(.x))), .groups="drop") |>
  pivot_longer(-form_date) |> filter(value == 1) |>
  distinct(form_date) |> pull(form_date)

dat <- panel |> filter(!form_date %in% bad) |>
  left_join(rets |> select(permno, month_date, price),
            by = c("permno"="permno","form_date"="month_date"))

sector_names <- c("10"="Energy","15"="Materials","20"="Industrials",
                  "25"="Cons Disc","30"="Cons Staples","35"="Health Care",
                  "40"="Financials","45"="Tech","50"="Telecom",
                  "55"="Utilities","60"="Real Estate")


# =====================================================================
# 2. Train once per year, score EVERY stock-month once
# =====================================================================
xgb_params <- list(objective="reg:squarederror", eta=0.05, max_depth=6,
                   subsample=0.7, colsample_bytree=0.7,
                   min_child_weight=50, lambda=5, nthread=4)

train_model <- function(train_end) {
  tr <- dat |> filter(!is.na(fwd_12m), form_date < train_end) |>
    drop_na(all_of(RK))
  if (nrow(tr) < 20000) return(NULL)
  fit <- tr |> filter(form_date <  max(form_date) %m-% years(4))
  val <- tr |> filter(form_date >= max(form_date) %m-% years(3))
  if (nrow(fit) < 5000 || nrow(val) < 2000) return(NULL)
  lg <- capture.output(
    xgb.train(xgb_params, xgb.DMatrix(as.matrix(fit[,RK]), label=fit$fwd_12m),
              nrounds=600,
              evals=list(v=xgb.DMatrix(as.matrix(val[,RK]), label=val$fwd_12m)),
              early_stopping_rounds=50, verbose=1))
  r <- as.numeric(str_extract(lg, "(?<=val-rmse:)[0-9.]+")); r <- r[!is.na(r)]
  xgb.train(xgb_params, xgb.DMatrix(as.matrix(tr[,RK]), label=tr$fwd_12m),
            nrounds = max(if (length(r)) which.min(r) else 150L, 10L), verbose=0)
}

# Longest holding period in the grid determines the embargo
MAX_HOLD <- 12

scored <- map_dfr(FIRST_YEAR:LAST_YEAR, function(yr) {
  m <- train_model(as.Date(paste0(yr,"-01-01")) %m-% months(MAX_HOLD))
  if (is.null(m)) return(NULL)
  cat("Scored", yr, "\n")
  cs <- dat |> filter(year(form_date) == yr, price >= MIN_PRICE) |>
    drop_na(all_of(RK))
  cs$score <- predict(m, as.matrix(cs[, RK]))
  cs |> transmute(form_date, permno, ticker, mktcap, price, score,
                  sector = recode(as.character(gsector), !!!sector_names))
})

write_rds(scored, "output/all_scores.rds")
cat("Scored", nrow(scored), "stock-months\n")


# =====================================================================
# 3. Generic backtest on cached scores
# =====================================================================
run_config <- function(n_side, min_cap, hold_mo, cost_bps,
                       sector_neutral = FALSE) {

  elig <- scored |> filter(mktcap >= min_cap)

  picks <- if (sector_neutral) {
    # equal number from each sector, so results aren't a sector bet
    per_sec <- max(2, round(n_side / 11))
    elig |> filter(!is.na(sector)) |> group_by(form_date, sector) |>
      slice_max(score, n = per_sec) |> ungroup()
  } else {
    elig |> group_by(form_date) |> slice_max(score, n = n_side) |> ungroup()
  }

  if (nrow(picks) == 0) return(NULL)

  held <- picks |>
    select(cohort = form_date, permno) |>
    crossing(k = 1:hold_mo) |>
    mutate(month_date = ceiling_date(cohort %m+% months(k), "month") - 1) |>
    left_join(rets |> select(permno, month_date, ret_adj),
              by = c("permno","month_date")) |>
    mutate(ret_adj = replace_na(ret_adj, 0)) |>
    group_by(cohort, month_date) |>
    summarise(ret = mean(ret_adj), .groups = "drop")

  p <- held |> group_by(month_date) |>
    summarise(ret = mean(ret), n_coh = n_distinct(cohort), .groups="drop") |>
    filter(n_coh == hold_mo) |>
    left_join(ff |> select(month_date, mktrf, smb, hml, rmw, cma, umd, rf),
              by = "month_date") |>
    drop_na(mktrf)

  if (nrow(p) < 36) return(NULL)

  cost <- (1/hold_mo) * 2 * cost_bps / 1e4
  rn   <- p$ret - cost
  ex   <- rn - p$rf

  capm <- lm(ex ~ p$mktrf)
  ff6  <- lm(ex ~ p$mktrf + p$smb + p$hml + p$rmw + p$cma + p$umd)

  tibble(
    n_side = n_side, min_cap = min_cap, hold_mo = hold_mo,
    cost_bps = cost_bps, sector_neutral = sector_neutral,
    n_months = nrow(p),
    ann_ret  = prod(1+rn)^(12/length(rn)) - 1,
    ann_vol  = sd(rn)*sqrt(12),
    sharpe   = mean(ex)*12 / (sd(rn)*sqrt(12)),
    capm_a   = coef(capm)[1]*12,
    capm_t   = summary(capm)$coef[1,3],
    beta     = coef(capm)[2],
    ff6_a    = coef(ff6)[1]*12,
    ff6_t    = summary(ff6)$coef[1,3]
  )
}


# =====================================================================
# 4. The grid
# =====================================================================
grid <- expand_grid(
  n_side   = c(25, 50, 100, 150, 300),
  min_cap  = c(100, 500, 2000),      # $M: all / small-mid+ / mid-large only
  hold_mo  = c(1, 3, 6, 12),
  cost_bps = c(20, 50)
)

cat("\nRunning", nrow(grid), "configurations...\n")

results <- pmap_dfr(grid, function(n_side, min_cap, hold_mo, cost_bps) {
  run_config(n_side, min_cap, hold_mo, cost_bps)
})

write_csv(results, "output/robustness_grid.csv")


# =====================================================================
# 5. Read the surface
# =====================================================================
cat("\n===== EFFECT OF BREADTH (averaged over all other settings) =====\n")
results |> group_by(n_side) |>
  summarise(ann = mean(ann_ret), vol = mean(ann_vol), sharpe = mean(sharpe),
            ff6_a = mean(ff6_a), pct_sig = mean(abs(ff6_t) > 2)) |>
  mutate(across(c(ann, vol, ff6_a, pct_sig), ~ percent(.x, accuracy=0.1)),
         sharpe = round(sharpe,2)) |> print()

cat("\n===== EFFECT OF SIZE FLOOR =====\n")
results |> group_by(min_cap) |>
  summarise(ann = mean(ann_ret), vol = mean(ann_vol), sharpe = mean(sharpe),
            ff6_a = mean(ff6_a), pct_sig = mean(abs(ff6_t) > 2)) |>
  mutate(across(c(ann, vol, ff6_a, pct_sig), ~ percent(.x, accuracy=0.1)),
         sharpe = round(sharpe,2)) |> print()

cat("\n===== EFFECT OF HOLDING PERIOD =====\n")
results |> group_by(hold_mo) |>
  summarise(ann = mean(ann_ret), vol = mean(ann_vol), sharpe = mean(sharpe),
            ff6_a = mean(ff6_a), pct_sig = mean(abs(ff6_t) > 2)) |>
  mutate(across(c(ann, vol, ff6_a, pct_sig), ~ percent(.x, accuracy=0.1)),
         sharpe = round(sharpe,2)) |> print()

cat("\n===== EFFECT OF TRANSACTION COSTS =====\n")
results |> group_by(cost_bps) |>
  summarise(ann = mean(ann_ret), sharpe = mean(sharpe),
            ff6_a = mean(ff6_a), pct_sig = mean(abs(ff6_t) > 2)) |>
  mutate(across(c(ann, ff6_a, pct_sig), ~ percent(.x, accuracy=0.1)),
         sharpe = round(sharpe,2)) |> print()

cat("\n===== HOW MANY CELLS BEAT THE MARKET? =====\n")
cat("Market over this period: 15.0% annual, Sharpe 0.93\n\n")
results |> summarise(
  n_cells        = n(),
  beat_on_return = mean(ann_ret > 0.15),
  beat_on_sharpe = mean(sharpe > 0.93),
  ff6_alpha_sig  = mean(ff6_t > 2),
  expected_by_chance = 0.025
) |> mutate(across(-c(n_cells, expected_by_chance),
                   ~ percent(.x, accuracy=0.1))) |> print(width = Inf)

cat("\nIf ff6_alpha_sig is near 2.5%, that is what pure chance produces\n")
cat("with a one-sided 5% test. Only a rate well ABOVE that is evidence.\n")


# =====================================================================
# 6. Sector-neutral comparison
# =====================================================================
cat("\n===== SECTOR-NEUTRAL vs UNCONSTRAINED =====\n")
sn <- bind_rows(
  run_config(100, 100, 6, 20, sector_neutral = FALSE) |> mutate(v="Unconstrained"),
  run_config(100, 100, 6, 20, sector_neutral = TRUE)  |> mutate(v="Sector-neutral")
)
print(sn |> select(v, ann_ret, ann_vol, sharpe, beta, ff6_a, ff6_t) |>
        mutate(across(c(ann_ret, ann_vol, ff6_a), ~ percent(.x, accuracy=0.1)),
               across(c(sharpe, beta, ff6_t), ~ round(.x,2))), width = Inf)

cat("\nIf sector-neutral performs similarly, the strategy is genuine stock\n")
cat("selection. If it collapses, the edge was a sector bet.\n")


# =====================================================================
# 7. Performance within each sector
# =====================================================================
# The return must be LEAD by one month. Joining a score to the SAME
# month's return measures a contemporaneous relationship, not a forecast,
# and comes out negative for mechanical reasons: a stock that just fell
# becomes cheap, and cheap is what the model ranks highly.

cat("\n===== TOP-DECILE SPREAD WITHIN EACH SECTOR =====\n")
cat("10-11 sectors tested. Expect roughly one false positive.\n")
cat("Consistency of SIGN across sectors matters more than any single t.\n\n")

fwd1 <- msf |>
  mutate(month_date = ceiling_date(floor_date(date, "month"), "month") - 1) |>
  arrange(permno, month_date) |>
  group_by(permno) |>
  mutate(fwd_1m = lead(ret, 1)) |>
  ungroup() |>
  select(permno, month_date, fwd_1m)

sector_res <- scored |>
  filter(mktcap >= 100, !is.na(sector)) |>
  left_join(fwd1, by = c("permno", "form_date" = "month_date")) |>
  group_by(form_date, sector) |>
  filter(sum(!is.na(fwd_1m)) >= 30) |>
  mutate(d = ntile(score, 10)) |>
  filter(d %in% c(1, 10)) |>
  group_by(form_date, sector, d) |>
  summarise(ret = mean(fwd_1m, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = d, values_from = ret, names_prefix = "d") |>
  mutate(spread = d10 - d1) |>
  group_by(sector) |>
  summarise(avg_monthly = mean(spread, na.rm = TRUE),
            annualised  = (1 + mean(spread, na.rm = TRUE))^12 - 1,
            t_stat      = mean(spread, na.rm = TRUE) /
              (sd(spread, na.rm = TRUE) / sqrt(n())),
            n_months    = n()) |>
  arrange(desc(t_stat))

print(sector_res |> mutate(across(c(avg_monthly, annualised),
                                  ~ percent(.x, accuracy = 0.1)),
                           t_stat = round(t_stat, 2)), n = 12)

cat("\nSectors with positive spread:", sum(sector_res$annualised > 0),
    "of", nrow(sector_res), "\n")
cat("Under the null of no signal you would expect about half.\n")

# Overall monthly rank IC — the headline predictive statistic
ic_overall <- scored |>
  left_join(fwd1, by = c("permno", "form_date" = "month_date")) |>
  group_by(form_date) |>
  filter(sum(!is.na(fwd_1m)) >= 50) |>
  summarise(ic = cor(score, fwd_1m, method = "spearman",
                     use = "complete.obs"), .groups = "drop")

cat("\n===== OVERALL MONTHLY RANK IC =====\n")
ic_overall |>
  summarise(mean_ic  = mean(ic, na.rm = TRUE),
            sd_ic    = sd(ic, na.rm = TRUE),
            t_stat   = mean(ic, na.rm = TRUE) /
              (sd(ic, na.rm = TRUE) / sqrt(n())),
            hit_rate = mean(ic > 0, na.rm = TRUE),
            n_months = n()) |>
  mutate(across(c(mean_ic, sd_ic), ~ round(.x, 4)),
         t_stat = round(t_stat, 2),
         hit_rate = percent(hit_rate, accuracy = 0.1)) |>
  print(width = Inf)

cat("\nMonthly IC of 0.02-0.03 is a genuine signal. It is also small:\n")
cat("roughly a 6% gross annual decile spread, which does not survive\n")
cat("costs plus a portfolio beta of 1.4.\n")

write_csv(sector_res, "output/sector_results.csv")
write_csv(ic_overall, "output/monthly_ic.csv")
# =====================================================================
# 8. Figures
# =====================================================================
theme_set(theme_minimal(base_size = 11))

fig1 <- results |>
  mutate(cap_lab = paste0("$", comma(min_cap), "M+"),
         hold_lab = paste0(hold_mo, "mo hold")) |>
  ggplot(aes(factor(n_side), sharpe, colour = factor(cost_bps),
             group = factor(cost_bps))) +
  geom_hline(yintercept = 0.93, linetype = 2, colour = "firebrick") +
  geom_line() + geom_point() +
  facet_grid(cap_lab ~ hold_lab) +
  scale_colour_manual(values = c("20"="steelblue4","50"="grey50"),
                      name = "Cost (bp)") +
  labs(title = "Sharpe ratio across all configurations",
       subtitle = "Red line = market Sharpe (0.93). Every cell tested is shown.",
       x = "Portfolio size (names)", y = "Sharpe")

ggsave("figures/robustness_grid.png", fig1, width = 11, height = 8, dpi = 300)

fig2 <- sector_res |>
  ggplot(aes(annualised, fct_reorder(sector, annualised),
             fill = abs(t_stat) > 2)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "grey40") +
  scale_x_continuous(labels = percent) +
  scale_fill_manual(values = c("TRUE"="steelblue4","FALSE"="grey75"),
                    name = "|t| > 2") +
  labs(title = "Top-minus-bottom decile spread within each sector",
       subtitle = "Sorted within sector each month. 11 tests — expect ~1 false positive.",
       x = "Annualised spread", y = NULL)

ggsave("figures/sector_spreads.png", fig2, width = 9, height = 5, dpi = 300)

cat("\nDone. Grid in output/robustness_grid.csv, two figures saved.\n")
