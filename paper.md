Cross-Sectional Equity Return Prediction
Machine learning and factor investing on 30 years of US equity data
Independent quantitative research — Roan Layland, UC San Diego Data: CRSP and Compustat via Wharton Research Data Services Code: R · 15 scripts · ~4,000 lines

ABSTRACT
I construct a point-in-time panel of 763,195 firm-month observations covering 9,354 US common stocks from 1994 to 2024 and ask three questions: whether firm characteristics predict cross-sectional returns out of sample, whether nonlinear models add value over linear ones, and whether any resulting predictability survives translation into a portfolio.
The signal is real. A gradient-boosted tree model trained under expanding-window validation with a twelve-month embargo produces a monthly rank information coefficient of 0.023 (t = 2.97), positive in all ten GICS sectors and significant in seven of ten size deciles. A rules-based composite of fifteen published factors achieves rank IC of 0.035 (t = 4.69). Both exceed the best of thirty randomly-selected benchmark portfolios by a wide margin. Asset growth is the strongest individual predictor, with an −8.5% annual decile spread (t = −8.55) that survives conditioning on size, sector, era, and volatility, and whose magnitude scales monotonically with idiosyncratic volatility from −0.9% to −11.9% across terciles — consistent with limits-to-arbitrage explanations.
The signal does not become a strategy. Across 120 systematically varied portfolio configurations, none exceeded the CRSP value-weighted market on either raw return or Sharpe ratio. Eight further constructions incorporating volatility targeting, low-beta screening, and quality filters were evaluated against five pre-specified criteria; none passed. The best construction achieved a Sharpe ratio of 0.66 against the market's 0.59 with CAPM alpha of 2.3% — not statistically distinguishable from zero (t = 1.60).
Three mechanisms explain the gap. Learned models systematically select high-beta securities (β = 1.29–1.79 across all machine-learning configurations, versus 0.86–1.02 for equal-weighted composites), and returns fall short of what that exposure alone predicts. Transaction costs consume 1–4% annually depending on holding period. And decomposition at matched breadth shows the model's discriminative power concentrates on the short side — the bottom decile sits at the 6th percentile of dividend yield and 17th of sales-to-price, while the top decile is near the cross-sectional median on nearly every characteristic.
A methodological finding runs throughout. Fine-grained configuration choices do not persist out of sample: sector performance rankings correlate −0.055 across sample halves and size-decile rankings 0.067. A portfolio using hindsight-selected sectors underperformed an unselected portfolio by 1.4 percentage points annually out of sample. Separately, an accruals screen that appeared to generate 15.6% annual returns and a Sharpe ratio of 0.78 fell to 12.9% and 0.60 once all configurations were forced onto a common set of months — the apparent advantage came from 210 silently dropped periods, not from selection. Findings that did replicate across sample halves shared a common property: an identifiable economic mechanism.

1. RESEARCH DESIGN
1.1 Motivation
The cross-sectional predictability of equity returns is among the most studied questions in empirical finance and among the least settled. Hou, Xue and Zhang (2020) retested hundreds of published anomalies under consistent methodology and found the majority failed replication. Gu, Kelly and Xiu (2020) demonstrated that machine learning methods improve out-of-sample predictive accuracy over linear factor models. Between these results lies an unresolved question that this project addresses directly: when a model finds statistical predictability, does it survive contact with portfolio construction, risk exposure, and trading costs?
1.2 Questions
Do firm characteristics predict returns out of sample? Measured by rank information coefficient under strict walk-forward validation.
Does nonlinearity add value, and of what form? Comparing regularized linear models against tree ensembles, then characterizing the shape of any nonlinearity identified.
Does predictability survive implementation? Tested across the space of realistic construction choices rather than at a single configuration.
The third question receives the most weight and produces the least intuitive answer.
1.3 What distinguishes this study
Pipeline validated before use. The full analytical chain was tested against synthetic panels containing signals of known strength before any real data was analysed. This established that the code recovers what is present and does not manufacture what is absent.
Nonlinearity characterized, not merely detected. Prior work establishes that tree ensembles outperform linear specifications. This study identifies the mechanism in this setting: non-monotonicity within features rather than multiplicative interaction across them. Seven theoretically-motivated interaction terms, derived from the study's own conditional analysis, produced no improvement when supplied to an elastic net.
Selection bias measured, not merely acknowledged. Rather than listing data mining as a limitation, the study quantifies it three separate times on its own data, and finds in each case that hindsight-selected specifications failed to replicate.
Negative results reported in full. Approximately 400 portfolio configurations were evaluated. Marginal-effect tables are reported throughout rather than best-cell results, on the reasoning that the maximum of 400 draws is not an unbiased estimate of anything.

2. DATA
2.1 Sources
Source
Content
CRSP Monthly Stock File
Returns, prices, shares outstanding, volume
CRSP Delisting File
Delisting returns
Compustat Fundamentals Annual
Balance sheet, income statement, cash flow
Compustat Fundamentals Quarterly
Earnings announcement dates (rdq)
CRSP-Compustat Link (CCM)
gvkey ↔ permno mapping with validity dates
Fama-French Factors
MKT, SMB, HML, RMW, CMA, UMD, RF

Universe filters: CRSP share codes 10–11 (US common stock), exchange codes 1–3 (NYSE, AMEX, NASDAQ), market capitalization above the monthly NYSE 20th percentile.
Final panel: 763,195 firm-month observations · 9,354 distinct securities · 370 monthly cross-sections · median 1,913 stocks per cross-section (range 1,714–3,162).
Returns are compounded from monthly CRSP returns with delisting returns applied, so firms that fail exit at their realized loss rather than disappearing at last traded price. Omitting this step is a common source of upward bias in academic backtests.
2.2 Point-in-time integrity
The central data-construction requirement: at formation date T, only information publicly available on T may be used.
Fundamentals become usable in the month following their earnings announcement date (rdq), which is present for 71.6% of firm-years. Where rdq is missing, the Fama-French convention of datadate + 6 months applies as a conservative fallback. The match is a rolling backward join — at each formation date, the most recent already-announced fundamental, subject to a 24-month staleness cap — not an equality join on fiscal year.
Audit results:
Metric
Pre-join
Post-join
Minimum gap (days)
28
28
Median gap (days)
42
207
Negative gaps
0.0%
0.0%

Price-scaled ratios use market capitalization at the formation date, not fiscal year end. Using the stale price would embed the valuation that existed when the financials were filed, reintroducing the circularity that makes naive P/E studies uninterpretable.
Independent verification. Apple's ROE in the panel steps from 0.336 to 0.462 between October and November 2015. Apple's fiscal year ends in September; FY2015 results were announced in late October. The step appears exactly where the timing rule requires, and the FY2015 value matches Apple's reported figure. An E/P of 0.058 in January 2015 implies a P/E near 17, consistent with Apple's actual trading multiple.
2.3 Features
Twenty-six characteristics, expanded to thirty-two in later analysis. All valuation measures are expressed as yields rather than ratios (E/P rather than P/E) so that negative denominators remain rankable rather than producing non-monotonic values.
Category
Features
Valuation
E/P, B/P, S/P, FCF yield, dividend yield, EBITDA/EV
Profitability
ROE, ROA, ROIC, gross margin, operating margin, net margin
Growth
Revenue growth, EPS growth, asset growth, FCF growth
Financial strength
Debt/assets, current ratio, cash ratio, interest coverage
Market
Log market cap, 12-month volatility, turnover
Technical
Momentum 12-1, momentum 6-1, one-month reversal
Added later
Accruals, capex/OCF, capex/assets, capex/sales, opex/sales, cash conversion

All features are converted to cross-sectional percentile ranks within each formation month, which controls outliers, makes coefficients comparable across features, and removes three decades of drift in valuation levels.
Momentum uses the standard 12-1 construction, skipping the most recent month to avoid contamination from short-term reversal.
2.4 Missing data
No individual feature exceeds 9.2% missingness. However, 26 independent missingness patterns compound: complete-case analysis retains only 865 of 1,840 median stocks per cross-section (47%).
Resolution: within-date median imputation, with binary missingness indicators for features above 5% missing.
This choice was made on evidence rather than convention. On synthetic data containing six known planted signals:
Method
Signals recovered
Mean absolute error
Complete cases
Complete-case
3 of 6
0.0152
244 of 780
Median imputation
6 of 6
0.0064
780 of 780

with no increase in false positives (0 of 20 and 0 of 25 respectively).
Fourteen periods (1993-01 through 1994-02) were dropped: lagged fundamentals and 12-month momentum have zero coverage there, making imputation impossible and regression undefined.

3. METHOD
3.1 Validating the pipeline before trusting it
Real data cannot establish whether analytical code is correct, because the true answer is unknown. Synthetic data can.
Before any real analysis, the complete pipeline was run against synthetic panels containing signals of specified strength, with realistic features: AR(1)-persistent characteristics, correlated factor blocks (0.65 within value, profitability, and momentum groups), a common market component, realistic missingness, and heavy idiosyncratic noise.
Test
Result
Recover six planted linear coefficients
6 of 6 at |t| > 2 · 0 of 25 false positives · MAE 0.0064 · true-vs-estimated correlation 0.953
Detect purely non-monotonic signal (symmetric hump, all linear betas zero)
XGBoost IC 0.196 · Random forest 0.184 · Elastic net −0.0003
Complete-case versus imputation
3 of 6 versus 6 of 6 signals recovered

The second test is the most important. It confirms that the tree hyperparameters can detect nonlinearity when it exists, so any subsequent failure of trees to outperform reflects the data-generating process rather than a strangled model. This distinction matters: without it, a null result on nonlinearity would be uninterpretable.
3.2 Walk-forward validation
Expanding window with a twelve-month embargo. For test year T, training data ends at T − 12 months. The target is a 12-month forward return, so any observation formed later has an outcome overlapping the test period. Standard k-fold cross-validation violates this and leaks.
Hyperparameters are selected on the final three years of each training window and never on test data. No shuffling occurs at any stage.
The embargo is verified before every run. For test year 2015: last training formation date 2013-12-31, its outcome realized 2014-12-31, first test formation date 2015-01-31.
3.3 Models
Model
Specification
Elastic net
α ∈ {0, 0.25, 0.5, 0.75, 1}, λ by validation MSE
Random forest
500 trees, depth 8, min node 500, mtry = √p
XGBoost
η = 0.05, depth 6, min child weight 50, λ = 5, early stopping at 50 rounds
Composite
15 factors, signs fixed from literature, equal weight, no training

3.4 Metrics and inference
Rank information coefficient — Spearman correlation between predicted and realized returns within each cross-section, averaged across periods. Preferred to RMSE because the strategy uses ordering rather than point estimates, and because RMSE is dominated by a handful of extreme returns.
Directional accuracy is deliberately excluded from headline results. The base rate of positive 12-month returns is approximately 70%, so a model predicting "up" universally scores 70% while being worthless.
All t-statistics are computed on time series of period-level statistics, never pooled across stocks. Pooling would treat ~1,900 stocks in a single month as independent observations and inflate t-statistics by approximately √N. Fama-MacBeth second-stage standard errors use Newey-West correction with lag scaled to series length.

4. RESULTS I — THE SIGNAL
4.1 Single-factor decile sorts
Annualized top-minus-bottom decile spreads, 358 monthly cross-sections:
Feature
Spread
t
Hit rate
Asset growth
−8.54%
−8.55
36.0%
FCF yield
+5.59%
6.81
69.0%
FCF growth
+1.71%
5.03
59.2%
Gross margin
+2.75%
4.70
54.2%
Sales-to-price
+9.13%
4.38
62.3%
Revenue growth
−3.60%
−3.75
46.9%
ROE
+2.97%
3.39
66.2%
EBITDA/EV
+5.51%
3.31
63.1%
Book-to-price
+4.89%
3.28
52.5%
One-month reversal
+3.64%
2.98
59.8%
ROIC
+3.21%
2.72
65.1%
ROA
+3.34%
2.71
64.0%
Interest coverage
+3.18%
2.66
62.0%
Momentum 6-1
+4.08%
2.55
65.9%
Current ratio
−2.99%
−2.41
40.8%
Cash ratio
−3.60%
−2.31
39.9%
E/P
+2.78%
2.11
62.0%
Momentum 12-1
+0.45%
0.28
61.7%
Leverage
−0.21%
−0.24
52.8%
Turnover
+0.30%
0.17
42.5%

Momentum is absent in this sample. The 12-1 spread is indistinguishable from zero, and era analysis reveals sign reversal across decades (t = 2.23, −3.62, 4.54, 0.94 across four sub-periods). This is consistent with documented momentum crashes in 2009 and 2020 and with momentum's known weakness in equal-weighted portfolios, where small illiquid names carry equal influence.
4.2 Fama-MacBeth regressions
Two-stage estimation with Newey-West standard errors, 358 cross-sections, all 26 ranked features plus missingness indicators:
Feature
Coefficient
t (NW)
With sector controls
ROE
+0.0396
3.33
3.42
Dividend yield
−0.0262
−3.08
−2.98
Asset growth
−0.0473
−2.97
−2.97
Cash ratio
+0.0575
2.64
3.26
Gross margin
+0.0411
2.32
2.62
Current ratio
−0.0334
−2.19
−2.30

Median per-period R² = 0.124 (adjusted 0.107). This exceeds the 1–5% typical of monthly-horizon studies because the target is annual: predictable variation accumulates over the horizon while characteristics remain persistent.
Three observations warrant isolation.
Dividend yield reverses sign under multivariate estimation. It is insignificant univariate (t = 0.93) but significantly negative jointly (t = −3.08). Conditional on profitability and balance-sheet strength, high-payout firms underperform — a relationship entirely invisible to single sorts.
Value factors split a single premium. FCF yield falls from t = 6.81 univariate to 0.82 multivariate; sales-to-price from 4.38 to 1.79. The five value proxies correlate 0.5–0.8. The block total is interpretable; individual coefficients within the block are not, and reporting one in isolation would be misleading.
Sector controls change little. Coefficients and t-statistics are broadly stable, indicating stock-selection effects rather than sector bets. This is corroborated later: sector-neutral construction produced Sharpe of 0.56 against 0.55 unconstrained.
4.3 Where the signal lives
Monthly rank IC by size decile:
Decile
Median cap
Rank IC
t
1
$692M
0.0248
2.73
2
$956M
0.0262
2.71
3
$1,310M
0.0244
2.51
4
$1,785M
0.0378
3.74
5
$2,467M
0.0241
2.27
6
$3,472M
0.0116
1.17
7
$5,064M
0.0240
2.42
8
$8,414M
0.0260
2.63
9
$16,601M
0.0170
1.80
10
$54,523M
0.0013
0.14

Positive in nine of ten deciles, significant in seven, and absent in the largest. This holds in both sample halves (−0.004 and +0.007), making it one of the few conditional patterns in the study with both a mechanism and out-of-sample persistence: mega-cap stocks are the most heavily analysed and most efficiently priced segment of the market, and accounting-based signals should not work there.
The fine-grained size pattern does not persist (rank correlation 0.067 across halves; see Section 7), so no specific decile should be treated as optimal.
4.4 Asset growth: the one factor that survives everything
Conditioning dimension
Result
Size quintiles
Significant in all 5 (t = −9.83 to −5.61)
Sectors
Negative in 10 of 10, significant in 9
Eras
Significant in all 4 sub-periods
Volatility terciles
Significant in all 3
Sector controls
Survives (t = −2.97 both with and without)

No other characteristic in the study passes all five tests. The finding independently replicates Cooper, Gulen and Schill (2008), and is robust enough in the broader literature that Fama and French added an investment factor (CMA) to their five-factor model in 2015 specifically to capture it.
The volatility gradient is the theoretically substantive result:
Volatility tercile
Asset growth spread
t
Low
−0.86%
−2.41
Medium
−4.79%
−7.18
High
−11.9%
−9.47

A fourteenfold difference in magnitude, monotonic. Sales-to-price exhibits the same shape (2.4% → 3.8% → 11.8%); gross margin reverses sign entirely (−0.8% → +5.0%).
This is the limits-to-arbitrage prediction: mispricing persists where arbitrage is risky, and nearly vanishes in stable, liquid securities.
Critically, the gradient is visible using pre-2010 data alone (−0.76%, −9.36%, −15.3%; t = −1.58, −4.37, −10.7). A researcher standing in 2009 would have specified the same conditioning. This distinguishes it from pattern-fitting after the fact — a distinction Section 7 demonstrates is not merely academic.

5. RESULTS II — WHAT THE MODELS LEARNED
5.1 Out-of-sample predictive accuracy
Walk-forward validation, 168 monthly test periods, 2010–2023:
Model
Rank IC
t
Hit rate
Elastic net
0.0777
6.29
78.2%
Random forest
0.0604
4.97
63.1%
XGBoost
0.0594
5.42
64.3%

At monthly horizon across the full sample: rank IC 0.0229, t = 2.97, hit rate 62.6% over 179 months. The two figures are consistent — annual-horizon IC exceeds monthly because predictable variation accumulates while characteristics persist.
For calibration: rank IC in the 0.02–0.06 range is what professional quantitative managers operate on. A model reporting 0.30 on equity returns has a look-ahead leak, not a discovery.
5.2 Decile portfolios and the divergence between metrics
Model
Gross spread
Turnover
Cost
Net spread
FF6 α
t
Random forest
6.82%
0.190
1.83%
5.00%
6.05%
2.90
XGBoost
6.96%
0.243
2.33%
4.63%
6.17%
3.15
Elastic net
4.21%
0.149
1.43%
2.78%
3.13%
1.44

Elastic net achieves the highest rank IC but the lowest decile spread and no significant alpha. Rank IC weights accuracy across the entire cross-section; decile spreads depend only on the tails. The linear model orders the middle of the distribution well; tree models discriminate better at the extremes, which is where portfolios form.
These alpha estimates are subsequently shown to be overstated. They use monthly-sampled twelve-month returns without HAC correction, which understates standard errors under overlapping observations. Section 6.3, computed on non-overlapping monthly returns at matched breadth, produces substantially weaker results and supersedes them. Both are reported: the discrepancy is itself instructive about how easily overlapping-return specifications inflate significance.
5.3 The form of the nonlinearity
Partial dependence analysis on out-of-sample data (baseline IC 0.0804):
Feature
Linear R²
Sign flips
Revenue growth
0.238
5
Net margin
0.550
11
Dividend yield
0.659
9
ROA
0.682
10
Gross margin
0.737
7
Sales-to-price
0.955
2
Current ratio
0.961
6
FCF yield
0.965
1

Several features exhibit non-monotonic relationships with forward returns. A linear model cannot represent these regardless of what interaction terms it is supplied.
The two-dimensional asset-growth × volatility surface is fit by a multiplicative specification (x₁ + x₂ + x₁x₂) with R² of only 0.699.
This resolves an otherwise puzzling result. Seven interaction terms, specified directly from the Section 4.4 conditional analysis, added nothing to the elastic net (ΔIC = −0.0017, t = −1.25) despite being the theoretically correct variables. The tree model's learned structure is threshold-shaped rather than multiplicative, so supplying the product term was supplying the wrong functional form.
Training separate models per volatility tercile did improve performance (ΔIC = +0.0117, t = 1.94 — marginally short of conventional significance), producing the highest alpha estimate in the study (6.24%, t = 3.87 under the same overlapping-return caveat as Section 5.2).
This is the study's most novel technical finding: in this setting, the machine-learning advantage derives from non-monotonicity within individual features, not from interactions between them.
5.4 What the model actually uses
Permutation importance — drop in out-of-sample rank IC when a feature is shuffled within month:
Rank
Feature
IC drop
1
FCF yield
0.0149
2
Dividend yield
0.0144
3
Sales-to-price
0.00885
4
Current ratio
0.00496
5
Gross margin
0.00404
6
ROA
0.00359
⋮




13
Asset growth
0.00158

Asset growth ranks thirteenth despite dominating every univariate test. Dividend yield ranks second despite being insignificant univariate.
The model exploits conditional behaviour rather than marginal predictive strength. This is a direct explanation for why single-factor sorts and machine-learning feature importance routinely disagree: they answer different questions.
5.5 The asymmetry — the study's central mechanism
Average feature percentile ranks of the predicted extremes:
Feature
Bottom decile (SELL)
Top decile (BUY)
Dividend yield
0.065
0.438
Sales-to-price
0.172
0.680
ROA
0.204
0.479
FCF yield
0.214
0.650
Net margin
0.259
0.461
Revenue growth
0.667
0.490
Current ratio
0.687
0.482

The model holds a sharp picture of an underperformer — no dividend, expensive relative to sales, unprofitable, growing revenue rapidly, sitting on cash — and a diffuse picture of an outperformer, near the cross-sectional median on nearly every characteristic.
The bottom-decile profile describes a firm that recently raised capital and is spending it without generating profit. That the model identifies this cluster sharply while having no comparably distinctive picture of a winner is the single fact that explains the implementation results in Section 6.
Practical consequence: the tradeable component of the signal requires short exposure, which faces borrowing costs, availability constraints, and recall risk that a symmetric cost model does not capture.

6. RESULTS III — IMPLEMENTATION
6.1 Signal decay determines holding period
Average monthly return by month-in-holding-period, 180 overlapping cohorts:
Month
1
2
3
4
5
6
7
8
9
10
11
12
Return
1.46%
1.11%
1.14%
1.14%
1.14%
1.20%
0.85%
0.86%
0.88%
0.75%
0.63%
0.36%

Approximately 75% decay across the holding year, with a clear break after month six.
This measurement generated a prediction — that intermediate holding periods should dominate — which was then confirmed. Six-month holds beat twelve-month holds (8.8% versus 4.0% net at 25 names) despite five times lower turnover, and performance plateaus across five to nine months rather than spiking at a single value. A plateau is meaningful evidence against overfitting on this parameter: a genuine optimum has neighbours.
Caveat: part of this decay reflects information staleness rather than signal decay. By month 12 the underlying fundamentals may be 24 months old. The two effects cannot be separated cleanly with this design.
6.2 Portfolio construction
Construction
Names
Hold
Net return
Vol
Sharpe
Max DD
Concentrated, monthly
25
1
9.5%
32.1%
0.44
−55.5%
Concentrated
25
6
8.8%
29.5%
0.43
−58.2%
Concentrated
25
12
4.0%
28.3%
0.28
−59.0%
Broad
100
6
12.0%
24.3%
0.59
−46.8%
Broad
150
6
12.5%
23.0%
0.63
−44.7%
CRSP value-weighted market
—
—
15.0%
16.0%
0.93
—

Note on benchmark figures. Market return and Sharpe ratio differ across tables in this paper because evaluation windows differ. The machine-learning results (Sections 5–6) cover 2010–2023, when the market returned 15.0% at Sharpe 0.93. The composite results (Section 8) cover 1994–2024, when the market returned 10.6–10.9% at Sharpe 0.58–0.59. Strategy performance must always be compared against the benchmark in the same row, never against a figure recalled from another table. Several apparent results during this project dissolved once this was enforced.
Performance improves monotonically with breadth across return, volatility, Sharpe ratio, and drawdown. This reflects diversification rather than signal strength: alpha remains negative at every breadth level. Twenty-five names cannot diversify idiosyncratic risk, and the edge is not large enough to compensate for the added variance.
6.3 Long/short decomposition at matched breadth
100 names per side, six-month hold, 20bp per side — identical signal, identical construction, only the tail differs:
Leg
Return
Vol
Sharpe
Beta
CAPM α
t
FF6 α
t
Long only
12.5%
24.2%
0.56
1.40
−6.0%
−1.78
+0.3%
0.14
Short only
−12.0%
27.6%
−0.37
−1.44
+10.0%
2.12
+1.3%
0.55
Long-short
4.7%
16.0%
0.37
−0.04
+6.4%
1.48
+3.9%
1.51

No leg produces FF6 alpha significantly different from zero at matched breadth. The short leg's CAPM alpha of 10.0% (t = 2.12) disappears under the six-factor model — its beta of −1.44 indicates the result primarily reflects negative market exposure rather than security selection.
Cost sensitivity for the long-short portfolio: FF6 alpha falls from 3.9% at 20bp to 3.1% at 30bp to 1.5% at 50bp. Given a universe extending to $100M market capitalization, 50bp is the realistic assumption.
6.4 Systematic configuration grid
120 configurations spanning breadth (25–300 names) × size floor ($100M–$2B) × holding period (1–12 months) × transaction cost (20/50bp).
Marginal effects, averaged over all other dimensions:
Dimension
Range
Return
Sharpe
FF6 α
Breadth
25 → 300
7.0% → 8.9%
0.35 → 0.46
−2.4% → −3.2%
Size floor
$100M → $2B
7.5% → 9.4%
0.37 → 0.47
−2.9% → −2.6%
Holding period
1 → 6 → 12 mo
4.1% → 10.9% → 9.2%
0.24 → 0.51 → 0.45
−7.1% → −1.0% → −1.1%
Cost
20 → 50 bp
9.6% → 6.6%
0.46 → 0.34
−1.4% → −4.3%

Of 120 configurations: 0% exceeded the market on return, 0% on Sharpe ratio, and 0% produced positive Fama-French six-factor alpha significant at the 5% level.
6.5 Why the strategies underperform
Market beta is the mechanism. Every configuration carries beta between 1.24 and 1.48. With market excess return of approximately 13.5% over the sample, a beta of 1.34 predicts a return near 19.6%. The strategy delivered 13.7%. The shortfall is the negative alpha.
A leveraged index position replicates beta 1.34 with no analysis whatsoever and would have returned approximately 19.6%.
Beta neutralization does not restore alpha. Selecting stocks within beta quintiles reduced portfolio beta from 1.34 to 1.24 and left CAPM alpha essentially unchanged (−4.4% → −4.3%). The model's preferred characteristics — small, cheap, volatile — correlate with beta inside every beta bucket, so exposure cannot be separated from signal.
No defensive property. Capture ratios are 1.30 in down markets and 1.15 in up markets: the portfolio amplifies losses more than gains. It beat the market in only 34.5% of down months. In March 2020 the market fell 13.2% while the strategy fell 22.8%.
Weighting scheme. All portfolios are equal-weighted while benchmarks are cap-weighted. Over 2010–2024, cap-weighted indices substantially outperformed equal-weighted equivalents because a small number of mega-cap stocks drove index returns. Any equal-weighted strategy faced this headwind regardless of signal quality. This is a genuine confound and is not resolved in this study.
6.6 Year-by-year performance
Year
Strategy
Market
Diff


Year
Strategy
Market
Diff
2010
25.6%
24.6%
+1.1%


2018
−5.1%
−5.0%
−0.1%
2011
−4.3%
0.5%
−4.8%


2019
26.9%
30.5%
−3.6%
2012
17.8%
16.3%
+1.5%


2020
33.1%
24.1%
+9.0%
2013
51.9%
35.2%
+16.7%


2021
24.6%
23.9%
+0.7%
2014
1.3%
11.8%
−10.4%


2022
−13.3%
−19.9%
+6.6%
2015
−10.2%
0.3%
−10.4%


2023
24.2%
26.7%
−2.5%
2016
7.3%
13.6%
−6.2%


2024
17.4%
25.0%
−7.6%
2017
22.2%
22.3%
−0.1%











Six of fifteen years positive. Cumulative outperformance concentrates in 2013 and 2020; excluding those two years the strategy underperforms consistently.

7. MEASURING SPECIFICATION-SEARCH BIAS
A standing concern in this literature is that reported performance reflects search rather than predictability. Rather than acknowledge this abstractly, the study measures it three times.
7.1 Sector rankings do not persist
Sector
2010–2017 rank
2018–2024 rank
Tech
1 (t = 2.41)
9 (t = 0.45)
Financials
2
5
Industrials
3
2
Energy
10 (t = −0.39)
3 (t = 1.14)

Rank correlation across halves: −0.055. Indistinguishable from noise. Applied to size deciles, the equivalent figure is 0.067, and the best first-half decile ranked ninth of ten in the second half.
7.2 Hindsight selection was actively harmful
Out-of-sample test, 2018–2024, 50 names, $2B floor, six-month hold:
Portfolio
Return
Sharpe
FF6 α
t
Sectors chosen on prior data only
13.8%
0.54
+4.2%
1.19
Sectors chosen with hindsight
11.6%
0.44
+0.4%
0.09
No sector selection
13.0%
0.49
+4.6%
1.34
Market
14.3%
0.70
—
—

The hindsight-selected portfolio underperformed the unselected portfolio by 1.4 percentage points annually. Selection bias here was not merely optimistic — it destroyed value.
7.3 Unequal comparison windows produce phantom results
An accruals quality screen initially appeared to deliver:


Return
Sharpe
CAPM α
t
Months
Apparent result
15.6%
0.78
7.6%
2.54
162
Matched-months result
12.9%
0.60
2.4%
1.21
372

The screen reduced the candidate pool below the minimum-portfolio-size constraint in 210 of 372 months, and those months were silently dropped. The surviving months had a lower market return (9.1% versus 10.6%) — an easier sample, not a better strategy. Roughly two-thirds of the apparent advantage was sample selection.
The general rule this establishes: any comparison in which the number of periods or the benchmark return differs across rows is not a comparison. This artifact was caught three times during the project and would have been invisible without explicitly checking period counts.
7.4 What separates real findings from artifacts
Every finding that replicated across sample halves shares one property: an identifiable economic mechanism.
Replicated
Mechanism
Signal absent in mega caps
Efficient pricing in the most-analysed segment
Performance rises with breadth
Diversification
Holding-period plateau at 5–9 months
Matches measured signal decay
Asset growth volatility gradient
Limits to arbitrage


Did not replicate
Nature
Best sector
Maximum of 10 noisy draws
Best size decile
Maximum of 10 noisy draws
Best specific configuration
Maximum of ~400 draws
Accruals screen at 30%
Sample-selection artifact


8. RESULTS IV — RULES-BASED CONSTRUCTION
8.1 A transparent alternative
Given that learned models systematically acquired unwanted beta exposure, a rules-based composite was constructed as a comparison: 15 factors, each signed according to its direction in prior literature rather than fitted to this sample, converted to percentile ranks and equally weighted. No training, no hyperparameters, nothing to overfit.
Factor group
Members
Sign source
Investment
Asset growth, revenue growth
Cooper, Gulen & Schill (2008)
Value
FCF yield, S/P, B/P, E/P, EBITDA/EV
Fama & French (1992), Basu (1977)
Profitability
Gross margin, ROE, ROA
Novy-Marx (2013)
Quality
FCF growth, interest coverage, accruals
Sloan (1996)
Quality (sample-signed)
Current ratio, cash ratio
This sample — disclosed

Two factors carry signs motivated by this sample rather than published research; this is disclosed rather than concealed, and results are materially unchanged when they are excluded.
8.2 Composite performance, 1994–2024
Configuration
Return
Market
Vol
Sharpe
Mkt Sharpe
Beta
CAPM α
t
50 names
11.1%
10.6%
20.5%
0.50
0.58
1.05
+1.0%
0.43
100 names
11.8%
10.6%
19.6%
0.55
0.58
1.02
+1.6%
0.77
200 names
12.3%
10.8%
19.1%
0.58
0.59
1.02
+1.8%
0.92
P60-95 band
12.2%
10.9%
18.1%
0.60
0.59
0.97
+1.9%
1.04
P80-100 band
11.9%
10.8%
15.5%
0.66
0.59
0.86
+2.3%
1.60

Composite rank IC: 0.0351 (t = 4.69), exceeding the trained XGBoost model's 0.0229. Sub-period stability: IC 0.0488 (t = 4.32) in 1994–2009, 0.0242 (t = 2.45) in 2010–2024 — weaker in the second half but still significant.
The composite outperformed the trained model, and the reason is beta. Machine-learning configurations carried beta of 1.29–1.79 across every specification tested; equal-weighted composites carried 0.86–1.05. Trees trained on raw forward returns learn to select volatile securities, because volatile securities have higher unconditional returns. The composite has no such incentive.
This is a substantive result: a transparent, untrained, equal-weighted combination of published factors achieved higher risk-adjusted returns than gradient-boosted trees trained on the same features — not through better prediction, but through the absence of unpaid risk accumulation.
8.3 Weighting by evidence strength does not help
Weighting factors by their t-statistics was tested in two forms: in-sample weights (contaminated, shown for comparison) and expanding-window weights computed from prior data only.
Method
Rank IC
t
Portfolio return
Sharpe
Equal weight
0.0353
4.20
10.6%
0.50
T-weighted, in-sample
0.0338
4.30
10.1%
0.49
T-weighted, expanding
0.0235
3.78
9.7%
0.48

Equal weighting wins. The expanding-window version — the only honest one — performs worst. Weighting by measured evidence strength adds nothing once hindsight is removed.
8.4 Additional spending and earnings-quality factors
Six factors measuring spending relative to earnings were constructed and tested: accruals, capex/OCF, capex/assets, capex/sales, opex/sales, and cash conversion.
Standalone, all six looked promising (IC t-statistics of 3.75, 3.46, 2.65, 2.61, 0.37, 0.29). In multivariate regression controlling for the existing factor set, none survived — the strongest was capex/OCF at t = −1.79.
The correlation structure explains why: capex/OCF correlates −0.71 with FCF yield, capex/sales −0.41, cash conversion −0.42 with ROA. They repackage value and profitability rather than adding a new dimension.
Accruals was the exception, though the evidence is mixed. Initial testing gave t = −0.72; after excluding 14 periods where predictors had zero variance, t = −2.91. It correlates only 0.06 with asset growth, making it genuinely orthogonal. However, within the composite its rank IC is 0.0014 (t = 0.37), the lowest of all fifteen factors, and adding it as a portfolio screen produced no significant improvement (Section 7.3).
Conclusion: the existing feature set already spans the spending signal. This is a clean negative result from a well-specified question.
8.5 Regime dependence
Splitting the composite at 2010:
Period
Return
Market
Sharpe
Mkt Sharpe
Beta
CAPM α
t
1994–2009
14.7%
8.0%
0.67
0.34
0.85
+7.2%
2.49
2010–2024
9.5%
14.8%
0.48
0.92
1.25
−6.9%
−2.28

Identical construction, opposite outcome, both statistically significant. Beta also flipped from 0.85 to 1.25: the same characteristics that selected defensive securities in the earlier period selected aggressive ones later.
A regime hypothesis was tested and not supported. Extending the machine-learning backtest to 2000–2023 produced the opposite pattern: rank IC of 0.0066 (t = 1.26) in 2000–2009 and 0.0237 (t = 3.13) in 2010–2023, with negative CAPM alpha in both. The two model families do not share a common regime story, which weakens any simple "value stopped working" interpretation. The one variable consistent across every era and both model families is beta exposure.

9. RESULTS V — FORMAL TESTS FOR MARKET OUTPERFORMANCE
9.1 Design
Eight strategies were evaluated against five criteria specified before the analysis was run:
Sharpe ratio exceeds the market in both sample halves
CAPM alpha positive in both halves
Full-sample CAPM alpha t > 2
Beta below 1.10 (the edge is not disguised leverage)
Full-sample Sharpe exceeds the best of 30 randomly-selected portfolios
Criterion 5 is a search correction. If randomly-selected portfolios achieve comparable Sharpe ratios, a designed strategy has demonstrated nothing.
9.2 Results
Strategy
Return
Sharpe
Mkt Sharpe
Beta
CAPM α
t
Criteria met
P80-100 composite
11.9%
0.66
0.59
0.86
+2.3%
1.60
2 of 5
Combined (size + beta + quality)
12.0%
0.64
0.59
0.87
+2.3%
1.42
2 of 5
Combined + vol target
10.2%
0.62
0.59
0.70
+1.9%
1.32
2 of 5
Low-beta screen
11.6%
0.57
0.58
0.95
+1.8%
0.95
2 of 5
Baseline composite
11.8%
0.55
0.58
1.02
+1.6%
0.77
2 of 5
Vol-targeted 12%
9.1%
0.48
0.58
0.81
+0.4%
0.22
2 of 5
Vol-targeted 15%
9.9%
0.47
0.58
0.98
+0.2%
0.10
2 of 5

Permutation benchmark (30 random portfolios): mean Sharpe 0.43, best 0.45, best alpha −1.7%, best alpha t = −1.22.
Verdict: 0 of 8 strategies pass. Every strategy clears criterion 5, confirming genuine signal — all beat the best random draw. All fail criterion 3: no alpha t-statistic reaches 2.
Volatility targeting, contrary to expectation, reduced performance (Sharpe 0.55 → 0.48). It lowered beta as designed but cut returns more than it cut risk.
9.3 The best result, stated honestly
A large-capitalization, equal-weighted composite of fifteen published factors achieved an annualized return of 11.9% against the market's 10.8%, with volatility of 15.5% against approximately 18%, producing a Sharpe ratio of 0.66 versus 0.59 at a market beta of 0.86. CAPM alpha was 2.3% (t = 1.60), not statistically significant. The advantage was concentrated in 1994–2009 (Sharpe 0.56 versus market 0.33) and reversed thereafter (0.75 versus 0.86). Maximum drawdown was −49.0%. This configuration was selected from approximately 400 evaluated across the project, and the study's own testing demonstrates that hindsight-selected configurations underperform out of sample.

10. LIMITATIONS
Equal versus capitalization weighting. The most material limitation. All portfolios equal-weight holdings while benchmarks are cap-weighted, over a period when mega-cap concentration drove index returns. A cap-weighted implementation of the same signal was not tested and would materially change the comparison.
Transaction cost model. The 20bp baseline is optimistic for a universe reaching $100M market capitalization, where spreads can exceed 50bp. The 50bp case is reported throughout as the conservative alternative; it reduces long-short FF6 alpha from 3.9% to 1.5%.
Short-side feasibility. The tradeable signal concentrates in the short leg, which faces borrowing costs, availability limits, and recall risk not modelled here. A symmetric cost assumption understates the true friction.
Sample period. 2010–2024 was historically unfavourable for the value, small-cap, and quality tilts this model produces. The composite's split results (Section 8.5) show a significant reversal at 2010, though the machine-learning results do not share this pattern, which complicates any simple regime interpretation.
Overlapping-return inference. Initial decile-level alpha estimates (Section 5.2) used monthly-sampled annual returns without HAC correction and are overstated. Section 6.3 supersedes them. This error was caught during the project and both sets are reported.
Configuration search. Approximately 400 portfolio configurations were evaluated. Marginal-effect tables are reported rather than individual cells. No single configuration should be interpreted as "the strategy."
Data end date. CRSP legacy monthly tables end December 2024, preventing evaluation of more recent periods.
Real Estate sector. GICS separated Real Estate from Financials in September 2016, leaving 5,926 observations. Excluded from sector analysis, reducing that test to ten sectors.
Untested extensions. International replication using Compustat Global (verified available: 1.09M firm-years, 25,000+ developed-market firms) was designed but not executed. Training on beta-residualized returns — which addresses the study's central diagnosis directly — was designed but not executed.
Consistency with prior literature. Hou, Xue and Zhang (2020) find most published anomalies fail replication under consistent methodology. This study's null result on implementability corroborates rather than contradicts that finding.

11. WHAT I WOULD DO DIFFERENTLY
Written after the fact, and more useful than a conventional future-work section.
Test cap-weighted portfolios from the outset. The equal-weight confound is the largest unresolved issue and would have been inexpensive to address early.
Use Newey-West standard errors in the first alpha regression. The initial decile-level t-statistics were overstated by roughly a factor of two. Catching this earlier would have calibrated expectations correctly from the start.
Check period counts on every comparison. The sample-selection artifact in Section 7.3 was invisible until period counts were compared explicitly. This should be an automatic assertion, not a manual check.
Fix the configuration grid before running it. Deciding on dimensions in advance and reporting the full surface is cleaner than the incremental expansion that actually occurred.
Train on beta-residualized returns. The diagnosis is clear — trees accumulate beta of 1.29–1.79 while composites hold 0.86–1.02, and that gap is the entire performance difference. Residualizing the target against market exposure before training attacks the cause directly.
Model borrowing costs explicitly. Given that alpha sits on the short side, symmetric cost assumptions understate friction.

12. CONCLUSION
Firm characteristics predict cross-sectional equity returns out of sample. The evidence is consistent across methods and specifications: monthly rank IC of 0.023 (t = 2.97) for gradient-boosted trees and 0.035 (t = 4.69) for a rules-based composite, positive spreads in all ten sectors, significant predictive power in seven of ten size deciles, and outperformance of the best of thirty randomly-selected portfolios in every strategy tested. Asset growth is the most robust individual predictor, surviving every conditioning dimension examined, with an effect that scales monotonically with idiosyncratic volatility in a manner consistent with limits-to-arbitrage explanations.
Tree ensembles capture structure that linear models cannot, and the structure is non-monotonic within features rather than multiplicative across them — a distinction with practical consequence, since hand-specified interaction terms derived from the study's own conditional analysis failed to close the performance gap.
None of this survives implementation. Across 120 systematically varied configurations and eight further constructions evaluated against pre-specified criteria, none exceeded the market on a risk-adjusted basis. The causes are identifiable rather than mysterious: learned models inherit market beta of 1.24–1.79 that beta-neutral construction cannot remove, transaction costs consume 1–4% annually, and the model's discriminative power concentrates on the short side, inaccessible to long-only implementations. A transparent equal-weighted composite outperformed the trained model precisely because it accumulated no unwanted risk exposure.
The methodological finding may prove the most portable. Repeated tests demonstrated that fine-grained configuration choices do not persist out of sample; that selecting them with hindsight produced worse performance than not selecting at all; and that a single unmatched comparison window inflated an apparent result by two-thirds. The findings that replicated were those with identifiable economic mechanisms. This is a concrete, quantified instance of the specification-search problem that motivates the replication literature — measured on the author's own data rather than cited from others'.
A statistically significant signal is not automatically an economically exploitable one. Establishing precisely where the gap opens, and demonstrating that it does not close across 400 attempts, is the contribution.

APPENDIX A — REPRODUCTION
AlphaQuant/
├── README.md                      Headline result, reproduction steps
├── research_log.md                Decision log with dates and rationale
├── scripts/
│   ├── 00_synthetic_data.R        Synthetic validation panel
│   ├── 01b_extended_pull.R        WRDS extraction
│   ├── 03_build_panel.R           Panel construction + look-ahead audit
│   ├── 05_factor_analysis.R       Sorts, Fama-MacBeth, elastic net
│   ├── 06_ml_backtest.R           Walk-forward ML with embargo
│   ├── 07_conditional_analysis.R  Size / sector / volatility / era
│   ├── 08_conditional_models.R    Interaction and regime models
│   ├── 09_interpretation.R        Permutation importance, PDP, surfaces
│   ├── 13_cohort_portfolio.R      Overlapping cohort backtest
│   ├── 14_final_analyses.R        Long/short decomposition
│   ├── 15_robustness_grid.R       120-configuration grid
│   ├── 16_best_combination.R      Selection-bias measurement
│   ├── 17_beta_and_regimes.R      Beta control, regime analysis
│   ├── 18_size_bands.R            Size band analysis
│   ├── 19_composite_score.R       Rules-based factor composite
│   ├── 20_spending_factors.R      Accruals and spending factors
│   ├── 22_accruals_screen.R       Quality screen tests
│   ├── 23_ml_extended.R           Extended-period ML backtest
│   └── 24_market_beating.R        Five-criterion formal evaluation
├── output/                        Result tables (CSV)
├── figures/                       Publication figures (PNG)
└── paper.pdf

Note: WRDS data is licensed and cannot be redistributed. data/ and data_global/ are listed in .gitignore.

APPENDIX B — FIGURES
Decile sorts by feature, with 95% confidence intervals
Cross-sectional feature correlation heatmap
Fama-MacBeth coefficients, with and without sector controls
Rank IC over time by model
Partial dependence, top eight features, with linear fit overlay
Asset growth × volatility interaction surface
Permutation importance
Factor spreads by size quintile
Factor spreads by era
Robustness grid Sharpe surface across 120 configurations
Equity curves by construction versus benchmark
Long / short / long-short leg decomposition
Composite versus individual factor spreads
Strategy equity curves with split-sample boundary marked

APPENDIX C — RESUME MATERIAL
Projects section (full):
AlphaQuant — Machine Learning and Factor Investing in US Equities Built an end-to-end quantitative research pipeline in R on 763,000 firm-month observations from CRSP and Compustat (1994–2024, 9,354 securities). Engineered 32 firm characteristics with point-in-time earnings-announcement alignment to eliminate look-ahead bias, and validated the full pipeline against synthetic data containing signals of known strength before analysing real data. Estimated Fama-MacBeth regressions with Newey-West correction and trained gradient-boosted trees under expanding-window validation with a 12-month embargo, achieving out-of-sample rank IC of 0.023 (t = 2.97). Evaluated 120 portfolio configurations against pre-specified criteria and demonstrated the signal does not survive implementation once market beta (1.24–1.79) and transaction costs are accounted for. Quantified specification-search bias empirically, showing hindsight-selected configurations underperformed unselected portfolios out of sample.
One-line version:
Built an ML equity-return model in R on 763k firm-months of CRSP/Compustat data; achieved out-of-sample rank IC of 0.023 (t = 2.97) and demonstrated across 120 portfolio configurations that the signal does not survive transaction costs and market beta exposure.
Skills demonstrated: R · SQL (PostgreSQL/WRDS) · panel data construction · point-in-time data handling · Fama-MacBeth estimation · HAC/Newey-West inference · gradient boosting · random forests · regularized regression · walk-forward validation · partial dependence analysis · permutation importance · portfolio construction · performance attribution · factor model alpha testing

APPENDIX D — INTERVIEW PREPARATION
Be able to answer each of these without notes.
Question
Core of the answer
How did you avoid look-ahead bias?
Fundamentals lagged to the month after rdq (present 71.6% of firm-years), datadate + 6mo fallback otherwise, rolling backward join with 24-month staleness cap. Audit confirms zero negative gaps, minimum 28 days. Verified independently against Apple's FY2015 announcement.
How did you know your code was correct?
Synthetic panels with signals of known strength. Recovered 6 of 6 planted coefficients with 0 of 25 false positives before touching real data. Also confirmed trees detect non-monotonic signal (IC 0.196) where linear models score zero.
Why rank IC rather than RMSE?
The strategy uses ordering, not point estimates. RMSE is dominated by extreme returns. Directional accuracy has a ~70% base rate and is misleading.
Why a 12-month embargo?
The target is a 12-month forward return. Any training observation formed within 12 months of the test window has an outcome overlapping it. Standard k-fold CV leaks here.
Why did trees beat linear models?
Non-monotonic feature relationships — linear R² as low as 0.238 on partial dependence with 5–11 sign flips — not multiplicative interactions, which I tested explicitly and which added nothing (ΔIC = −0.0017).
Why didn't it beat the market?
Beta of 1.24–1.79 with negative alpha. The signal is smaller than the risk premium required to harvest it, and discriminative power sits on the short side, which long-only cannot access.
Why did the simple composite beat the ML model?
Beta. Trees trained on raw forward returns learn to pick volatile stocks because volatile stocks have higher unconditional returns. The composite has no such incentive: beta 0.86–1.05 versus 1.29–1.79.
How did you guard against overfitting?
Expanding-window validation with embargo, marginal-effect reporting rather than best-cell, permutation benchmark against 30 random portfolios, and three direct measurements showing hindsight selection underperformed no selection.
What surprised you?
That an untrained equal-weighted composite outperformed gradient boosting, and that the reason was risk exposure rather than predictive accuracy. Also that a single unmatched comparison window inflated a result by two-thirds.
What would you do differently?
Test cap-weighted portfolios, use HAC errors from the first regression, assert on period counts automatically, and train on beta-residualized returns.



