# =====================================================================
# ACross-Sectional-Equity-Return-Prediction — Monthly rebalanced Top-25 portfolio
#
# Each month: score the cross-section, buy $100 of each of the top 25,
# hold one month, sell everything, repeat.
#
# Models are retrained once per year with a 12-month embargo, then used
# for the following twelve monthly rebalances.
# =====================================================================

library(tidyverse)
library(lubridate)
library(xgboost)
library(scales)

N_HOLD     <- 25       # stocks in the portfolio
DOLLARS    <- 100      # per position
COST_BPS   <- 20       # per side
MIN_PRICE  <- 5        # exclude sub-$5 stocks (illiquid, wide spreads)
MIN_MKTCAP <- 100      # $M floor
FIRST_YEAR <- 2010
LAST_YEAR  <- 2024

panel    <- read_rds("data/panel_ranked.rds")
FEATURES <- read_rds("data/feature_list.rds")
RK       <- paste0("rk_", FEATURES)
msf      <- read_rds("data/msf.rds")
dl       <- read_rds("data/delist.rds")


# =====================================================================
# 1. One-month forward returns (with delisting returns)
# =====================================================================

fwd1 <- msf |>
  mutate(ym = floor_date(date, "month")) |>
  left_join(dl |> mutate(ym = floor_date(dlstdt, "month")) |>
              select(permno, ym, dlret) |>
              distinct(permno, ym, .keep_all = TRUE),
            by = c("permno","ym")) |>
  mutate(ret_adj = case_when(
    !is.na(ret) & !is.na(dlret) ~ (1+ret)*(1+dlret) - 1,
     is.na(ret) & !is.na(dlret) ~ dlret,
    TRUE ~ ret)) |>
  arrange(permno, ym) |>
  group_by(permno) |>
  mutate(fwd_1m = lead(ret_adj, 1),
         price  = abs(prc)) |>
  ungroup() |>
  transmute(permno, form_date = ceiling_date(ym, "month") - 1,
            fwd_1m, price, company = comnam)

cat("fwd_1m built for", nrow(fwd1), "stock-months\n")


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
  left_join(fwd1, by = c("permno","form_date"))


# =====================================================================
# 3. Annual retrain, monthly scoring
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

  xgb.train(xgb_params,
            xgb.DMatrix(as.matrix(tr[,RK]), label=tr$fwd_12m),
            nrounds = n, verbose = 0)
}

holdings <- list()

for (yr in FIRST_YEAR:LAST_YEAR) {
  # 12-month embargo before the first month of this year
  m <- train_model(as.date <- as.Date(paste0(yr, "-01-01")) %m-% months(12))
  if (is.null(m)) next
  cat("Trained for", yr, "\n")

  months_in_yr <- dat |> filter(year(form_date) == yr) |>
    distinct(form_date) |> arrange(form_date) |> pull(form_date)

  for (d in months_in_yr) {
    d <- as.Date(d)
    cs <- dat |> filter(form_date == d, !is.na(fwd_1m),
                        price >= MIN_PRICE, mktcap >= MIN_MKTCAP) |>
      drop_na(all_of(RK))
    if (nrow(cs) < 200) next

    cs$score <- predict(m, as.matrix(cs[, RK]))
    holdings[[as.character(d)]] <- cs |>
      slice_max(score, n = N_HOLD) |>
      select(form_date, permno, ticker, company, price, mktcap,
             score, fwd_1m)
  }
}

hold <- bind_rows(holdings) |> arrange(form_date, desc(score))
write_rds(hold, "output/monthly_holdings.rds")

cat("\nMonths simulated:", n_distinct(hold$form_date), "\n")


# =====================================================================
# 4. Turnover and costs
# =====================================================================
dates <- sort(unique(hold$form_date))

turnover <- map_dfr(seq_along(dates), function(i) {
  cur <- hold$permno[hold$form_date == dates[i]]
  if (i == 1) return(tibble(form_date = dates[i], turnover = 1))
  prv <- hold$permno[hold$form_date == dates[i-1]]
  tibble(form_date = dates[i],
         turnover = 1 - length(intersect(cur, prv)) / length(cur))
})


# =====================================================================
# 5. The equity curve
# =====================================================================
# Gross return = equal-weighted mean of the 25 positions' 1-month return.
# Cost = turnover x 2 sides x 20bp, charged each month.

monthly <- hold |>
  group_by(form_date) |>
  summarise(gross_ret = mean(fwd_1m, na.rm = TRUE),
            n_held = n(),
            med_mktcap = median(mktcap, na.rm = TRUE),
            .groups = "drop") |>
  left_join(turnover, by = "form_date") |>
  mutate(cost = turnover * 2 * COST_BPS / 1e4,
         net_ret = gross_ret - cost)

# Benchmark: equal-weighted return of the eligible universe
bench <- dat |>
  filter(!is.na(fwd_1m), price >= MIN_PRICE, mktcap >= MIN_MKTCAP,
         form_date %in% dates) |>
  group_by(form_date) |>
  summarise(bench_ret = mean(fwd_1m, na.rm = TRUE), .groups = "drop")

START_VALUE <- N_HOLD * DOLLARS   # 25 x $100 = $2,500

acct <- monthly |>
  left_join(bench, by = "form_date") |>
  arrange(form_date) |>
  mutate(
    value_gross = START_VALUE * cumprod(1 + gross_ret),
    value_net   = START_VALUE * cumprod(1 + net_ret),
    value_bench = START_VALUE * cumprod(1 + bench_ret),
    peak        = cummax(value_net),
    drawdown    = value_net / peak - 1,
    excess      = net_ret - bench_ret
  )


# =====================================================================
# 6. Month-by-month table
# =====================================================================
cat("\n================ MONTHLY ACCOUNT VALUE ================\n")
acct |>
  transmute(Month = format(form_date, "%Y-%m"),
            `Gross %` = percent(gross_ret, accuracy = 0.1),
            `Cost %`  = percent(cost, accuracy = 0.01),
            `Net %`   = percent(net_ret, accuracy = 0.1),
            Value     = dollar(value_net, accuracy = 1),
            `Bench $` = dollar(value_bench, accuracy = 1),
            `DD %`    = percent(drawdown, accuracy = 0.1),
            Turn      = percent(turnover, accuracy = 1)) |>
  print(n = Inf)


# =====================================================================
# 7. Summary
# =====================================================================
n_mo <- nrow(acct); n_yr <- n_mo / 12

summarise_leg <- function(ret, val, label) {
  tibble(
    Strategy   = label,
    `Final $`  = dollar(last(val), accuracy = 1),
    `Total %`  = percent(last(val)/START_VALUE - 1, accuracy = 0.1),
    `Annual %` = percent((last(val)/START_VALUE)^(1/n_yr) - 1, accuracy = 0.1),
    `Vol %`    = percent(sd(ret) * sqrt(12), accuracy = 0.1),
    Sharpe     = round(mean(ret)*12 / (sd(ret)*sqrt(12)), 2),
    `Best mo`  = percent(max(ret), accuracy = 0.1),
    `Worst mo` = percent(min(ret), accuracy = 0.1),
    `Win %`    = percent(mean(ret > 0), accuracy = 0.1)
  )
}

cat("\n================ SUMMARY ================\n")
cat("Start: $", START_VALUE, " | ", n_mo, " months (", round(n_yr,1),
    " years)\n\n", sep = "")

bind_rows(
  summarise_leg(acct$gross_ret, acct$value_gross, "Top 25 (gross)"),
  summarise_leg(acct$net_ret,   acct$value_net,   "Top 25 (net of costs)"),
  summarise_leg(acct$bench_ret, acct$value_bench, "Equal-weight universe")
) |> print(width = Inf)

cat("\nExcess over benchmark: ",
    percent(mean(acct$excess) * 12, accuracy = 0.1),
    " per year, t = ",
    round(mean(acct$excess)/(sd(acct$excess)/sqrt(n_mo)), 2), "\n", sep = "")
cat("Max drawdown: ", percent(min(acct$drawdown), accuracy = 0.1), "\n", sep="")
cat("Average monthly turnover: ",
    percent(mean(acct$turnover), accuracy = 1), "\n", sep = "")
cat("Annual cost drag: ",
    percent(mean(acct$cost) * 12, accuracy = 0.1), "\n", sep = "")
cat("Median market cap held: $",
    comma(round(median(acct$med_mktcap))), "M\n", sep = "")


# =====================================================================
# 8. Charts
# =====================================================================
fig1 <- acct |>
  select(form_date, value_gross, value_net, value_bench) |>
  pivot_longer(-form_date) |>
  mutate(name = recode(name, value_gross = "Top 25 gross",
                       value_net = "Top 25 net", value_bench = "Universe EW")) |>
  ggplot(aes(form_date, value, colour = name)) +
  geom_line(linewidth = .9) +
  scale_y_log10(labels = dollar) +
  scale_colour_manual(values = c("Top 25 gross"="grey60",
                                 "Top 25 net"="steelblue4",
                                 "Universe EW"="firebrick")) +
  labs(title = "Monthly rebalanced Top-25 portfolio",
       subtitle = paste0("$", START_VALUE, " initial, $", DOLLARS,
                         " per position, rebalanced monthly. Log scale."),
       x = NULL, y = "Account value", colour = NULL)

ggsave("figures/monthly_equity_curve.png", fig1, width = 10, height = 6, dpi = 300)

fig2 <- acct |>
  ggplot(aes(form_date, drawdown)) +
  geom_area(fill = "firebrick", alpha = .6) +
  scale_y_continuous(labels = percent) +
  labs(title = "Drawdown, net of costs", x = NULL, y = NULL)

ggsave("figures/monthly_drawdown.png", fig2, width = 10, height = 4, dpi = 300)

write_csv(acct, "output/monthly_account.csv")

cat("\nSaved figures and output/monthly_account.csv\n")
