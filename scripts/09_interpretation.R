# =====================================================================
# AlphaQuant — What is the tree model actually using?
#
# Your interaction experiment showed that vol x asset_growth in
# multiplicative form does NOT close the gap between linear and tree
# models. So the nonlinearity has some other shape. This script finds it.
#
#   1. Permutation importance (rank-IC based, not RMSE)
#   2. Partial dependence — the SHAPE of each effect
#   3. Linearity test — how far is each effect from a straight line?
#   4. 2D partial dependence — the actual interaction surface
#   5. Portfolio characteristics — what does the model buy and sell?
# =====================================================================

library(tidyverse)
library(lubridate)
library(xgboost)
library(scales)

PANEL_PATH <- "data/panel_ranked.rds"
panel      <- read_rds(PANEL_PATH)
FEATURES   <- read_rds("data/feature_list.rds")
RK         <- paste0("rk_", FEATURES)

set.seed(42)
theme_set(theme_minimal(base_size = 11) +
          theme(panel.grid.minor = element_blank(),
                plot.title = element_text(face = "bold")))


# =====================================================================
# 1. Prepare data (same pipeline as 06 and 08)
# =====================================================================
miss_rate  <- panel |> summarise(across(all_of(RK), ~ mean(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "term", values_to = "rate")
needs_flag <- miss_rate |> filter(rate > 0.05) |> pull(term)

panel <- panel |>
  mutate(across(all_of(needs_flag), ~ as.integer(is.na(.x)),
                .names = "miss_{.col}")) |>
  group_by(form_date) |>
  mutate(across(all_of(RK), ~ {
    m <- is.na(.x); if (any(m) && !all(m)) .x[m] <- median(.x, na.rm = TRUE); .x
  })) |>
  ungroup()

RK <- c(RK, paste0("miss_", needs_flag))

bad <- panel |> group_by(form_date) |>
  summarise(across(all_of(RK), ~ mean(is.na(.x))), .groups = "drop") |>
  pivot_longer(-form_date) |> filter(value == 1) |>
  distinct(form_date) |> pull(form_date)

dat <- panel |> filter(!form_date %in% bad, !is.na(fwd_12m)) |>
  select(permno, ticker, form_date, gsector, mktcap, fwd_12m, all_of(RK)) |>
  drop_na(all_of(RK))

# Train on everything up to the embargo, hold out 2011-2023 for testing.
# Same embargo logic as the walk-forward: the last training observation's
# outcome must be realised before the test window opens.
TRAIN_END <- as.Date("2010-01-01")
train <- dat |> filter(form_date < TRAIN_END)
test  <- dat |> filter(form_date >= as.Date("2011-01-01"))

cat("Train:", nrow(train), "rows |  Test:", nrow(test), "rows\n")

xgb_params <- list(objective = "reg:squarederror", eta = 0.05, max_depth = 6,
                   subsample = 0.7, colsample_bytree = 0.7,
                   min_child_weight = 50, lambda = 5, nthread = 4)

fit <- train |> filter(form_date <  as.Date("2006-01-01"))
val <- train |> filter(form_date >= as.Date("2007-01-01"))

lg <- capture.output(
  xgb.train(xgb_params,
            xgb.DMatrix(as.matrix(fit[, RK]), label = fit$fwd_12m),
            nrounds = 600,
            evals = list(v = xgb.DMatrix(as.matrix(val[, RK]),
                                         label = val$fwd_12m)),
            early_stopping_rounds = 50, verbose = 1)
)
r <- as.numeric(str_extract(lg, "(?<=val-rmse:)[0-9.]+")); r <- r[!is.na(r)]
BEST_N <- max(if (length(r)) which.min(r) else 150L, 10L)
cat("Best rounds:", BEST_N, "\n")

model <- xgb.train(xgb_params,
                   xgb.DMatrix(as.matrix(train[, RK]), label = train$fwd_12m),
                   nrounds = BEST_N, verbose = 0)

test$pred <- predict(model, as.matrix(test[, RK]))

base_ic <- test |> group_by(form_date) |>
  summarise(ic = cor(pred, fwd_12m, method = "spearman")) |>
  pull(ic) |> mean(na.rm = TRUE)
cat("Baseline out-of-sample rank IC:", round(base_ic, 4), "\n")


# =====================================================================
# 2. PERMUTATION IMPORTANCE
# =====================================================================
# Shuffle one feature, see how much rank IC falls. Measured on rank IC
# rather than RMSE because ranking is what the strategy actually uses.
# Built-in xgboost gain is biased toward high-cardinality features;
# permutation is not.
#
# Shuffling happens WITHIN each month, so the cross-sectional
# distribution is preserved and only the stock-to-value mapping breaks.

perm_importance <- function(feat, n_rep = 3) {
  drops <- map_dbl(1:n_rep, function(i) {
    t2 <- test |> group_by(form_date) |>
      mutate(!!feat := sample(.data[[feat]])) |> ungroup()
    p  <- predict(model, as.matrix(t2[, RK]))
    ic <- t2 |> mutate(pred = p) |> group_by(form_date) |>
      summarise(ic = cor(pred, fwd_12m, method = "spearman")) |>
      pull(ic) |> mean(na.rm = TRUE)
    base_ic - ic
  })
  tibble(feature = feat, ic_drop = mean(drops), sd_drop = sd(drops))
}

cat("\nComputing permutation importance (a few minutes)...\n")
imp <- map_dfr(RK, perm_importance) |> arrange(desc(ic_drop))

cat("\n===== PERMUTATION IMPORTANCE =====\n")
cat("ic_drop = how much rank IC falls when this feature is shuffled\n\n")
print(imp, n = 25)

fig_imp <- imp |> slice_max(ic_drop, n = 15) |>
  ggplot(aes(ic_drop, fct_reorder(str_remove(feature, "rk_"), ic_drop))) +
  geom_col(fill = "steelblue4") +
  geom_errorbar(aes(xmin = ic_drop - sd_drop, xmax = ic_drop + sd_drop),
                width = .3, colour = "grey30") +
  labs(title = "Permutation importance (out-of-sample)",
       subtitle = "Drop in rank IC when the feature is shuffled within month",
       x = "Rank IC drop", y = NULL)
ggsave("figures/permutation_importance.png", fig_imp,
       width = 8, height = 6, dpi = 300)


# =====================================================================
# 3. PARTIAL DEPENDENCE — the shape of each effect
# =====================================================================
# Set the feature to each grid value for every stock, predict, average.
# Isolates the model's learned function for that feature.

TOP <- imp |> slice_max(ic_drop, n = 8) |> pull(feature)
samp <- test |> slice_sample(n = 20000)

pdp_1d <- function(feat, grid_n = 20) {
  grid <- seq(0.025, 0.975, length.out = grid_n)
  map_dfr(grid, function(g) {
    tmp <- samp; tmp[[feat]] <- g
    tibble(feature = feat, x = g,
           yhat = mean(predict(model, as.matrix(tmp[, RK]))))
  })
}

cat("\nComputing partial dependence...\n")
pdp <- map_dfr(TOP, pdp_1d)

fig_pdp <- pdp |>
  mutate(label = str_remove(feature, "rk_")) |>
  ggplot(aes(x, yhat)) +
  geom_line(linewidth = 1, colour = "steelblue4") +
  geom_smooth(method = "lm", se = FALSE, linetype = 2,
              colour = "firebrick", linewidth = .6) +
  facet_wrap(~ label, scales = "free_y") +
  labs(title = "Partial dependence: what the model learned",
       subtitle = "Blue = model's function. Red dashed = best linear fit. Gaps are the nonlinearity.",
       x = "Feature percentile rank", y = "Predicted 12-month return")
ggsave("figures/partial_dependence.png", fig_pdp,
       width = 11, height = 7, dpi = 300)


# =====================================================================
# 4. LINEARITY TEST — how far from a straight line?
# =====================================================================
# This is the key diagnostic. Your interaction experiment showed that
# multiplicative terms don't close the linear-tree gap, so the shape
# must be something else. R2_linear near 1.0 means that feature's effect
# IS essentially linear; low values mean it isn't.

linearity <- pdp |>
  group_by(feature) |>
  summarise(
    r2_linear   = summary(lm(yhat ~ x))$r.squared,
    range_yhat  = diff(range(yhat)),
    slope       = coef(lm(yhat ~ x))[2],
    # is it monotonic, or does it turn?
    n_sign_flips = sum(diff(sign(diff(yhat))) != 0)
  ) |>
  arrange(r2_linear)

cat("\n===== LINEARITY OF EACH EFFECT =====\n")
cat("r2_linear near 1.00 -> effect is essentially linear\n")
cat("r2_linear below 0.80 -> genuinely nonlinear\n")
cat("n_sign_flips > 0     -> non-monotonic (a linear model CANNOT fit this)\n\n")
print(linearity, n = 15)


# =====================================================================
# 5. 2D PARTIAL DEPENDENCE — the interaction surface
# =====================================================================
# Your 08 test used vol x asset_growth as a MULTIPLICATIVE term and it
# failed. This shows the surface the tree actually learned. If it isn't
# a smooth saddle, that explains why the product term didn't help.

pdp_2d <- function(f1, f2, grid_n = 12) {
  g <- seq(0.05, 0.95, length.out = grid_n)
  s <- samp |> slice_sample(n = 8000)
  expand_grid(x1 = g, x2 = g) |>
    pmap_dfr(function(x1, x2) {
      tmp <- s; tmp[[f1]] <- x1; tmp[[f2]] <- x2
      tibble(x1 = x1, x2 = x2,
             yhat = mean(predict(model, as.matrix(tmp[, RK]))))
    })
}

cat("\nComputing 2D partial dependence (slow)...\n")
surf <- pdp_2d("rk_asset_growth", "rk_vol_12m")

fig_surf <- surf |>
  ggplot(aes(x1, x2, fill = yhat)) +
  geom_tile() +
  geom_contour(aes(z = yhat), colour = "white", alpha = .4) +
  scale_fill_viridis_c(name = "Predicted\nreturn") +
  labs(title = "Interaction surface: asset growth x volatility",
       subtitle = "A smooth diagonal gradient means a product term suffices. Anything else explains why it didn't.",
       x = "Asset growth rank", y = "Volatility rank")
ggsave("figures/interaction_surface.png", fig_surf,
       width = 8, height = 6.5, dpi = 300)

# Formally: how much of the surface does a linear + product model explain?
surf_fit <- lm(yhat ~ x1 + x2 + x1:x2, data = surf)
cat("\n===== IS THE SURFACE A SIMPLE PRODUCT? =====\n")
cat("R2 of (x1 + x2 + x1*x2) fit to the tree surface:",
    round(summary(surf_fit)$r.squared, 4), "\n")
cat("Near 1.00 -> a product term captures it; your 08 result is puzzling.\n")
cat("Below 0.90 -> the tree learned a shape no product term can express,\n")
cat("             which explains why adding interactions did not help.\n")


# =====================================================================
# 6. WHAT DOES THE MODEL ACTUALLY BUY?
# =====================================================================
# The most intuitive output in the whole project: average characteristics
# of the top vs bottom predicted decile.

port <- test |>
  group_by(form_date) |>
  mutate(d = ntile(pred, 10)) |>
  filter(d %in% c(1, 10)) |>
  ungroup() |>
  group_by(d) |>
  summarise(across(all_of(TOP), mean), n = n()) |>
  mutate(d = if_else(d == 10, "BUY (top decile)", "SELL (bottom decile)"))

cat("\n===== AVERAGE CHARACTERISTICS OF EACH SIDE =====\n")
cat("Values are percentile ranks. 0.5 = median stock.\n\n")
port |> pivot_longer(-c(d, n)) |>
  pivot_wider(names_from = d, values_from = value) |>
  mutate(difference = `BUY (top decile)` - `SELL (bottom decile)`) |>
  arrange(desc(abs(difference))) |>
  print(n = 25)


# =====================================================================
# 7. EXPORT
# =====================================================================
write_csv(imp,       "output/permutation_importance.csv")
write_csv(pdp,       "output/partial_dependence.csv")
write_csv(linearity, "output/linearity_test.csv")
write_csv(surf,      "output/interaction_surface.csv")

cat("\nDone. Four CSVs, three figures.\n")


# =====================================================================
# HOW TO READ THIS
# =====================================================================
# The central question your 08 results raised: if vol x asset_growth in
# product form doesn't close the linear-tree gap, what shape IS the
# nonlinearity?
#
# Section 4 answers it for single features. Watch for:
#   - n_sign_flips > 0 : the effect turns around. Linear models cannot
#     represent this at all, regardless of interaction terms.
#   - r2_linear < 0.8 with 0 sign flips : monotonic but curved, e.g. the
#     effect concentrates in one tail. A linear model gets the direction
#     right and the magnitude wrong where it matters most.
#
# Section 5 answers it for the interaction. If the surface R2 is below
# 0.90, the tree learned something threshold-like rather than
# multiplicative — which is exactly why your hand-coded product term
# failed, and it is a genuinely interesting finding to report.
#
# Section 6 is what goes in your presentation. "The model buys low
# asset growth, high FCF yield, high sales-to-price" is a sentence
# anyone can follow.
