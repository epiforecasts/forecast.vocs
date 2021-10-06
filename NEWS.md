# forecast.vocs 0.0.3

## Functionality

* `summarise_posterior` now returns a single data.table and all downstream processing and plotting functions have been updated to use this forma
* `forecast` has been updated to use the framework from `forecast_dt` and `forecast_dt` has been removed.
* `combine_posteriors_dt` has been removed in favour of `unnest_posterior`. This allows for the output of `forecast` (and wrappers that produce a data.table) to be easily untested and then used without additional changes in downstream plotting and processing functions.
* add `update_voc_label` as part of the workflow in `forecast` and exposed a user facing argument to control the label.
* Fixes a bug when specifying the number of initial weeks without sequence data. This was resulting in all weeks without sequence data being assumed to at the start and likely causing some model fitting issues.
* Added check functions to make sure input observation and arguments have the correct format.
* Added unit tests for  all preprocessing functions.

## Infrastructure

* Switched to installing `cmdstanr` from GitHub.

# forecast.vocs 0.0.2

* Package generalised to all variants of concern beyond Delta.
* Tooling added to forecast across dates and  scenarios.
* Tooling added for prior and posterior predictive checks.
* Tooling added for simulating data from the model (useful for simulation-based calibration amongst other things).
* Example data updated to use snapshots of COVID-19 notifications and Delta sequences sourced for  the RKI (Germany).

# forecast.vocs 0.0.1

* Extracted package code from analysis code and started work on generalising use case.
* Added initial version of forecasting model for  1 and 2 strains with optional independence, partial pooling, and scaled relationships.
