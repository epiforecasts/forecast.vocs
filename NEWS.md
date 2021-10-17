# forecast.vocs 0.0.7

* Further generalised the `forecast` family to accept custom data and forecast extraction functions.
* Split generic posterior summary function from specific posterior summary function.
* Catch data passing bugs in `forecast`  with new tests
* Add argument passing in  `fv_posterior` to allow it be used with `forecast`.
* Added runtime to diagnostic fit output.
* Fixes `generate_obs()` to work with all strain models and adds unit tests for all function functionality.

# forecast.vocs 0.0.6

* Add a vignette defining the package models.
* Updated documentation to use `{preferably}`.
* Add basic tests for all modelling functions.
* Add tests for all postprocessing functions.
* Add tests for all plotting functions.
* Add tests for all forecasting functions.
* Add a new utility, `fv_example()` which can be used to load built in example output and scripts. This is used for postprocessing and plotting examples as well as for testing plotting on static input.
* Model specific functions (i.e `load_model`, `summarise_posterior`, and functions prefixed with `stan_` have been renamed with the `fv_` prefix).
* Added the ability to pass model specific functions to the `forecast` function and wrappers enabling users to supply custom models but make use of the remaining package functionality.

# forecast.vocs 0.0.5

* Overhaul all package documentation and examples.
* Enforce proper printing of all `data.table` output.
* Add structure to documentation website.

# forecast.vocs 0.0.4

* Improve the robustness of plotting observations and forecast dates so that they can be faceted using the same variables as used for plotting estimates/forecasts.
* Added the option to scale growth rates and reproduction numbers. This allows crude scaling by the generation time etc increasing the interpretability of these estimates.
* Surfaced more options when plotting to allow for increased generalisability.

# forecast.vocs 0.0.3

* `fv_tidy_posterior` now returns a single data.table and all downstream processing and plotting functions have been updated to use this forma
* `forecast` has been updated to use the framework from `forecast_dt` and `forecast_dt` has been removed.
* `combine_posteriors_dt` has been removed in favour of `unnest_posterior`. This allows for the output of `forecast` (and wrappers that produce a data.table) to be easily untested and then used without additional changes in downstream plotting and processing functions.
* add `update_voc_label` as part of the workflow in `forecast` and exposed a user facing argument to control the label.
* Fixes a bug when specifying the number of initial weeks without sequence data. This was resulting in all weeks without sequence data being assumed to at the start and likely causing some model fitting issues.
* Added check functions to make sure input observation and arguments have the correct format.
* Added unit tests for  all preprocessing functions.
* Custom VOC labelling is now a feature of `fv_tidy_posterior()` (and hence `forecast()`) as well as a standalone option via `update_voc_label()`.

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
