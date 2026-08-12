# Cross-Sectional Equity Return Prediction

**Machine learning and factor investing on 30 years of US equity data**

*Roan Layland · UC San Diego · rlayland@ucsd.edu*

Out-of-sample rank IC of **0.023 (t = 2.97)** across 763,195 firm-months.
Across **120 portfolio configurations, none beat the market** on a
risk-adjusted basis — market beta of 1.24–1.79 and transaction costs
consume the edge.

📄 **[Full paper](paper.md)** · 📊 **[Figures](figures/)** · 💻 **[Code](scripts/)**

---

## What this is

An independent replication and extension of the cross-sectional return
prediction literature, built end to end in R on CRSP and Compustat data
accessed through WRDS.

The project asks whether firm characteristics predict returns out of
sample (they do), whether nonlinear models help (they do, in a specific
way), and whether any of it survives portfolio implementation (it
doesn't). The third answer is the interesting one.

## Data

| | |
|---|---|
| Observations | 763,195 firm-months |
| Securities | 9,354 US common stocks |
| Period | 1994–2024, 370 monthly cross-sections |
| Median cross-section | 1,913 stocks |
| Sources | CRSP monthly + delisting, Compustat annual + quarterly, CCM link, Fama-French factors |

Universe: CRSP share codes 10–11, exchanges 1–3, market cap above the
monthly NYSE 20th percentile.

## Key results

### The signal is real

| Metric | Value |
|---|---|
| Monthly rank IC (XGBoost) | 0.023 (t = 2.97) |
| Monthly rank IC (rules-based composite) | 0.035 (t = 4.69) |
| Sectors with positive spread | 10 of 10 |
| Size deciles with significant IC | 7 of 10 |
| Best single factor: asset growth | −8.5% annual decile spread (t = −8.55) |

Asset growth survives conditioning on size, sector, era, and volatility.
Its magnitude scales monotonically with idiosyncratic volatility
(−0.9% → −4.8% → −11.9% across terciles), consistent with
limits-to-arbitrage explanations.

### The signal doesn't become a strategy

| Test | Result |
|---|---|
| 120 configurations beating market on return | **0%** |
| 120 configurations beating market on Sharpe | **0%** |
| Configurations with significant FF6 alpha | **0%** |
| Best construction | Sharpe 0.66 vs market 0.59, alpha 2.3% (t = 1.60) |

Three mechanisms: learned models systematically select high-beta
securities (β = 1.24–1.79 versus 0.86–1.05 for equal-weighted
composites); transaction costs consume 1–4% annually; and the model's
discriminative power concentrates on the short side, inaccessible to
long-only portfolios.

### Nonlinearity has a specific form

Partial dependence analysis shows several features are **non-monotonic**
(linear R² as low as 0.238, with 5–11 sign reversals). The asset-growth ×
volatility interaction surface is fit by a multiplicative specification
with R² of only 0.699 — which explains why seven hand-specified
interaction terms added nothing to an elastic net (ΔIC = −0.0017,
t = −1.25).

### Specification-search bias, measured

| Test | Result |
|---|---|
| Sector rankings, correlation across sample halves | **−0.055** |
| Size decile rankings, correlation across halves | **0.067** |
| Hindsight-selected sectors vs no selection (out-of-sample) | **−1.4%/year** |
| Accruals screen before/after matching comparison windows | 15.6% → **12.9%** |

Findings that replicated across sample halves all had identifiable
economic mechanisms. Findings that didn't were maxima of noisy draws.

## Methodology notes

**Point-in-time alignment.** Fundamentals become usable the month after
their earnings announcement date (`rdq`, present for 71.6% of firm-years),
with `datadate + 6 months` as a conservative fallback. Rolling backward
join with a 24-month staleness cap. Audit confirms zero negative gaps,
minimum 28 days.

**Pipeline validated before use.** The full analytical chain was tested
against synthetic panels with signals of known strength: 6 of 6 planted
coefficients recovered, 0 of 25 false positives, before any real data was
analysed.

**Walk-forward with embargo.** Expanding window, 12-month embargo (the
target is a 12-month forward return, so any closer training observation
has an overlapping outcome). Hyperparameters tuned on the training window
only. No shuffled cross-validation at any point.

**Inference.** All t-statistics computed on time series of period-level
statistics, never pooled across stocks. Newey-West correction for
overlapping returns.

## Repository structure

```
scripts/
  00_synthetic_data.R        Synthetic validation panel
  01b_extended_pull.R        WRDS extraction
  03_build_panel.R           Panel construction + look-ahead audit
  05_factor_analysis.R       Sorts, Fama-MacBeth, elastic net
  06_ml_backtest.R           Walk-forward ML with embargo
  07_conditional_analysis.R  Size / sector / volatility / era
  08_conditional_models.R    Interaction and regime models
  09_interpretation.R        Permutation importance, partial dependence
  13_cohort_portfolio.R      Overlapping cohort backtest
  14_final_analyses.R        Long/short decomposition
  15_robustness_grid.R       120-configuration grid
  16_best_combination.R      Selection-bias measurement
  17_beta_and_regimes.R      Beta control, regime analysis
  18_size_bands.R            Size band analysis
  19_composite_score.R       Rules-based factor composite
  20_spending_factors.R      Accruals and spending factors
  22_accruals_screen.R       Quality screen tests
  23_ml_extended.R           Extended-period ML backtest
  24_market_beating.R        Five-criterion formal evaluation

output/    Result tables (CSV)
figures/   Publication figures (PNG)
paper.pdf  Full writeup
```

## Reproduction

Requires a WRDS subscription with CRSP and Compustat access.

```r
install.packages(c("RPostgres","tidyverse","lubridate","broom","sandwich",
                   "lmtest","glmnet","xgboost","ranger","scales","slider",
                   "data.table","zoo"))
```

Run in order: `01b_extended_pull.R` → `03_build_panel.R` →
`05_factor_analysis.R` → `06_ml_backtest.R`, then any analysis script.

**Data is not included.** WRDS data is licensed and cannot be
redistributed. `data/` is gitignored.

## Limitations

Equal-weighted portfolios compared against cap-weighted benchmarks;
20bp transaction cost assumption is optimistic for the smallest names in
the universe; short-side borrowing costs not modelled; sample period
2010–2024 was historically unfavourable for value and quality tilts; and
approximately 400 configurations were evaluated in total, so
marginal-effect tables are reported rather than best-cell results.

Full limitations section in the paper.

## Tools

R · PostgreSQL (WRDS) · xgboost · ranger · glmnet · data.table ·
tidyverse · sandwich/lmtest

---

*Independent research project. Not investment advice.*
