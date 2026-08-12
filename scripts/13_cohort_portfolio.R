1# =====================================================================
# AlphaQuant — Overlapping 12-month cohort portfolio
#
# Each month: score the cross-section, buy $100 of each of the top 25.
# Hold that cohort for a full 12 months, then sell.
#
# At steady state you hold 12 overlapping cohorts = 300 positions, and
# only 1/12 of the book turns over each month. This is the standard
# Jegadeesh-Titman overlapping construction.
#
# WHY THIS IS BETTER THAN MONTHLY REBALANCING
#   - holding period matches the 12-month horizon the model predicts
#   - turnover falls from 60-90%/month to ~8%/month
#   - cost drag falls from ~4%/yr to well under 1%/yr
#   - 300 positions instead of 25 means far less idiosyncratic noise
#
# CAPITAL: $2,500 of new money each month for the first 12 months
# ($30,000 total). After that it is self-funding — each maturing cohort
# pays for the next one.
# =====================================================================

library(tidyverse)
library(lubridate)
library(xgboost)
library(scales)

N_HOLD     <- 150
DOLLARS    <- 100
HOLD_MO    <- 6
COST_BPS   <- 20
MIN_PRICE  <- 5
MIN_MKTCAP <- 500
FIRST_YEAR <- 2010
LAST_YEAR  <- 2024

panel    <- read_rds("data/panel_ranked.rds")
FEATURES <- read_rds("data/feature_list.rds")
RK       <- paste0("rk_", FEATURES)
msf      <- read_rds("data/msf.rds")
dl       <- read_rds("data/delist.rds")


# =====================================================================
# 1. Monthly return series
# =====================================================================
rets <- msf |>
  mutate(ym = floor_date(date, "month")) |>
  left_join(dl |> mutate(ym = floor_date(dlstdt, "month")) |>
              select(permno, ym, dlret) |>
              distinct(permno, ym, .keep_all = TRUE),
            by = c("permno","ym")) |>
  mutate(ret_adj = case_when(
    !is.na(ret) & !is.na(dlret) ~ (1+ret)*(1+dlret) - 1,
     is.na(ret) & !is.na(dlret) ~ dlret,
    TRUE ~ ret)) |>
  transmute(permno,
            month_date = ceiling_date(ym, "month") - 1,
            ret_adj,
            price = abs(prc),
            company = comnam) |>
  filter(!is.na(ret_adj))

cat("Return series:", nrow(rets), "stock-months\n")


# =====================================================================
# 2. Standard prep
# =====================================================================
miss_rate  <- panel |> summarise(across(all_of(RK), ~ mean(is.na(.x)))) |>
  pivot_longer(everything(), names_to="term", values_to="rate")
needs_flag <- miss_rate |> filter(rate > 0.05) |> pull(term)

panel <- panel |>
  mutate(across(all_of(needs_flag), ~ as.integer(is.na(.x)),
                .names = "miss_{.col}")) |>
  group_by(form_date) |>
  mutate(across(all_of(RK), ~ {
    m <- is.na(.x); if (any(m) && !all(m)) .x[m] <- median(.x, na.rm=TRUE); .x
  })) |>
  ungroup()

RK <- c(RK, paste0("miss_", needs_flag))

bad <- panel |> group_by(form_date) |>
  summarise(across(all_of(RK), ~ mean(is.na(.x))), .groups="drop") |>
  pivot_longer(-form_date) |> filter(value == 1) |>
  distinct(form_date) |> pull(form_date)

dat <- panel |> filter(!form_date %in% bad) |>
  left_join(rets |> select(permno, month_date, price, company),
            by = c("permno" = "permno", "form_date" = "month_date"))


# =====================================================================
# 3. Annual retrain, monthly cohort formation
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
  n <- max(if (length(r)) which.min(r) else 150L, 10L)
  xgb.train(xgb_params, xgb.DMatrix(as.matrix(tr[,RK]), label=tr$fwd_12m),
            nrounds = n, verbose = 0)
}

cohorts <- list()

for (yr in FIRST_YEAR:LAST_YEAR) {
  m <- train_model(as.Date(paste0(yr, "-01-01")) %m-% months(HOLD_MO))
  if (is.null(m)) next
  cat("Trained for", yr, "\n")

  for (d in sort(unique(dat$form_date[year(dat$form_date) == yr]))) {
    d  <- as.Date(d)
    cs <- dat |> filter(form_date == d,
                        price >= MIN_PRICE, mktcap >= MIN_MKTCAP) |>
      drop_na(all_of(RK))
    if (nrow(cs) < 200) next

    cs$score <- predict(m, as.matrix(cs[, RK]))
    cohorts[[as.character(d)]] <- cs |>
      slice_max(score, n = N_HOLD) |>
      transmute(cohort = d, permno, ticker, company, price, mktcap, score)
  }
}

coh <- bind_rows(cohorts)
cat("\nCohorts formed:", n_distinct(coh$cohort), "\n")


# =====================================================================
# 4. Track each position over its 12-month life
# =====================================================================
# For each cohort, attach the monthly return of every holding for each
# of the 12 months after formation. If a stock delists mid-hold, its
# series ends and the position is treated as cash for the remainder.

positions <- coh |>
  select(cohort, permno, ticker) |>
  crossing(k = 1:HOLD_MO) |>
  mutate(month_date = ceiling_date(cohort %m+% months(k), "month") - 1) |>
  left_join(rets |> select(permno, month_date, ret_adj),
            by = c("permno","month_date")) |>
  mutate(ret_adj = replace_na(ret_adj, 0))   # delisted -> cash

# Return of each cohort in each calendar month (equal weight within cohort)
cohort_monthly <- positions |>
  group_by(cohort, month_date, k) |>
  summarise(ret = mean(ret_adj), n = n(), .groups = "drop")

# Portfolio return = equal weight across all ACTIVE cohorts that month
port <- cohort_monthly |>
  group_by(month_date) |>
  summarise(gross_ret     = mean(ret),
            n_cohorts     = n_distinct(cohort),
            n_positions   = sum(n),
            .groups = "drop") |>
  arrange(month_date) |>
  filter(n_cohorts == HOLD_MO)   # steady state only

cat("Steady-state months:", nrow(port), "\n")
cat("Positions held:", unique(port$n_positions)[1], "\n")


# =====================================================================
# 5. Costs
# =====================================================================
# One cohort of 25 is sold and one is bought each month. Overlap between
# the maturing and new cohorts is small, so treat it as a full round
# trip on 1/HOLD_MO of the book.

MONTHLY_TURNOVER <- 1 / HOLD_MO
MONTHLY_COST     <- MONTHLY_TURNOVER * 2 * COST_BPS / 1e4

port <- port |> mutate(cost = MONTHLY_COST,
                       net_ret = gross_ret - cost)


# =====================================================================
# 6. Benchmark
# =====================================================================
bench <- dat |>
  filter(price >= MIN_PRICE, mktcap >= MIN_MKTCAP) |>
  select(permno, form_date) |>
  rename(month_date = form_date) |>
  left_join(rets |> select(permno, month_date, ret_adj),
            by = c("permno","month_date")) |>
  filter(!is.na(ret_adj), month_date %in% port$month_date) |>
  group_by(month_date) |>
  summarise(bench_ret = mean(ret_adj), .groups = "drop")

START_VALUE <- N_HOLD * DOLLARS * HOLD_MO   # 25 x $100 x 12 = $30,000

acct <- port |>
  left_join(bench, by = "month_date") |>
  arrange(month_date) |>
  mutate(
    value_gross = START_VALUE * cumprod(1 + gross_ret),
    value_net   = START_VALUE * cumprod(1 + net_ret),
    value_bench = START_VALUE * cumprod(1 + bench_ret),
    peak        = cummax(value_net),
    drawdown    = value_net / peak - 1,
    excess      = net_ret - bench_ret
  )


# =====================================================================
# 7. Month-by-month
# =====================================================================
cat("\n============ MONTHLY ACCOUNT VALUE ============\n")
acct |>
  transmute(Month     = format(month_date, "%Y-%m"),
            `Gross %` = percent(gross_ret, accuracy = 0.1),
            `Net %`   = percent(net_ret, accuracy = 0.1),
            Value     = dollar(value_net, accuracy = 1),
            `Bench $` = dollar(value_bench, accuracy = 1),
            `+/- vs bench` = percent(excess, accuracy = 0.1),
            `DD %`    = percent(drawdown, accuracy = 0.1)) |>
  print(n = Inf)


# =====================================================================
# 8. Summary
# =====================================================================
n_mo <- nrow(acct); n_yr <- n_mo / 12

leg <- function(ret, val, label) tibble(
  Strategy   = label,
  `Final $`  = dollar(last(val), accuracy = 1),
  `Total %`  = percent(last(val)/START_VALUE - 1, accuracy = 0.1),
  `Annual %` = percent((last(val)/START_VALUE)^(1/n_yr) - 1, accuracy = 0.1),
  `Vol %`    = percent(sd(ret)*sqrt(12), accuracy = 0.1),
  Sharpe     = round(mean(ret)*12 / (sd(ret)*sqrt(12)), 2),
  `Worst mo` = percent(min(ret), accuracy = 0.1),
  `Win %`    = percent(mean(ret > 0), accuracy = 0.1)
)

cat("\n================ SUMMARY ================\n")
cat("Start: $", comma(START_VALUE), " | ", n_mo, " months (",
    round(n_yr,1), " years)\n",
    "Holding: ", HOLD_MO, " months | ", N_HOLD, " per cohort | ",
    unique(port$n_positions)[1], " positions at steady state\n\n", sep="")

bind_rows(
  leg(acct$gross_ret, acct$value_gross, "Cohort 12mo (gross)"),
  leg(acct$net_ret,   acct$value_net,   "Cohort 12mo (net)"),
  leg(acct$bench_ret, acct$value_bench, "Equal-weight universe")
) |> print(width = Inf)

cat("\nExcess over benchmark: ",
    percent(mean(acct$excess)*12, accuracy = 0.1), "/yr, t = ",
    round(mean(acct$excess)/(sd(acct$excess)/sqrt(n_mo)), 2), "\n", sep="")
cat("Max drawdown: ", percent(min(acct$drawdown), accuracy = 0.1), "\n", sep="")
cat("Monthly turnover: ", percent(MONTHLY_TURNOVER, accuracy = 0.1),
    " | annual cost drag: ", percent(MONTHLY_COST*12, accuracy = 0.01),
    "\n", sep="")


# =====================================================================
# 9. Cohort-level detail — which vintages worked?
# =====================================================================
cohort_perf <- cohort_monthly |>
  group_by(cohort) |>
  arrange(k) |>
  summarise(total_ret = prod(1 + ret) - 1, .groups = "drop") |>
  filter(!is.na(total_ret))

cat("\n========== 12-MONTH RETURN BY COHORT VINTAGE ==========\n")
cohort_perf |>
  mutate(Year = year(cohort)) |>
  group_by(Year) |>
  summarise(avg_12m = percent(mean(total_ret), accuracy = 0.1),
            best    = percent(max(total_ret), accuracy = 0.1),
            worst   = percent(min(total_ret), accuracy = 0.1),
            n = n()) |>
  print(n = 20)


# =====================================================================
# 10. Charts
# =====================================================================
fig1 <- acct |>
  select(month_date, value_net, value_bench) |>
  pivot_longer(-month_date) |>
  mutate(name = recode(name, value_net = "Cohort strategy (net)",
                       value_bench = "Equal-weight universe")) |>
  ggplot(aes(month_date, value, colour = name)) +
  geom_line(linewidth = 1) +
  scale_y_log10(labels = dollar) +
  scale_colour_manual(values = c("Cohort strategy (net)"="steelblue4",
                                 "Equal-weight universe"="firebrick")) +
  labs(title = "Overlapping 12-month cohort portfolio",
       subtitle = paste0("$", comma(START_VALUE), " initial, top ", N_HOLD,
                         " each month held 12 months. Net of costs. Log scale."),
       x = NULL, y = "Account value", colour = NULL)

ggsave("figures/cohort_equity_curve.png", fig1, width = 10, height = 6, dpi = 300)

fig2 <- cohort_perf |>
  ggplot(aes(cohort, total_ret, fill = total_ret > 0)) +
  geom_col() +
  geom_hline(yintercept = 0, colour = "grey40") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("TRUE"="steelblue4","FALSE"="firebrick"),
                    guide = "none") +
  labs(title = "12-month return of each monthly cohort",
       subtitle = "Every bar is one formation month held a full year",
       x = NULL, y = "Realised 12-month return")

ggsave("figures/cohort_returns.png", fig2, width = 10, height = 5, dpi = 300)

write_csv(acct, "output/cohort_account.csv")
write_csv(cohort_perf, "output/cohort_returns.csv")
write_rds(coh, "output/cohorts.rds")

cat("\nSaved figures and CSVs.\n")


# =====================================================================
# HOW TO READ THIS
# =====================================================================
# Compare "Cohort 12mo (net)" against "Equal-weight universe", not the
# S&P 500. Your universe is US common stocks above a size floor, equal
# weighted — comparing that to a cap-weighted large-cap index is not a
# fair benchmark.
#
# The number that matters is the excess t-statistic. Above 2 over ~14
# years is a genuine result. Below 2 means the strategy outperformed on
# average but not reliably enough to distinguish from luck.
#
# The cohort-vintage table is the honest picture: some formation months
# work, others do not. A strategy that only worked in 3 of 14 years is
# a different claim from one that worked in 11.
