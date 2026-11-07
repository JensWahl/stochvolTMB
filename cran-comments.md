# stochvolTMB 0.4.0

This release adds two models (skew t with and without leverage), exact one-step-ahead residuals, Value at Risk and
Expected Shortfall with backtests, and model comparison. It also fixes the instability of the skew-Gaussian leverage
model. See NEWS.md for the details and the breaking changes.

## Test environments

* local macOS, R 4.6.1
* GitHub Actions: ubuntu-latest, windows-latest and macos-latest, R release
* win-builder: R-devel and R-release

## R CMD check results

0 errors | 0 warnings | 1 note

The note lists possibly misspelled words in DESCRIPTION. They are spelled correctly: Kupiec and Christoffersen are
the authors of the cited backtests, and "backtested" is the standard term in finance for testing a risk model on past
data.

The installed size is about 21 Mb, most of it the compiled TMB model in libs. This is usual for packages that use TMB.

## Downstream dependencies

There are currently no downstream dependencies for this package.
