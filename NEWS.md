# forecast.vocs 0.0.9

## Features

### Model

* Switched the "pooled" model to use a vector autoregression set up and renamed the resulting control parameter "correlated". On test data this performs substantially better than the old model and should be more readily extensible.
* Added the ability (via the `r_step` argument) to set the step width for the AR(1) process on the growth rate. This means that when fitting to daily data, for example, the growth rate can be specified to only change weekly. This increases the tractability of the model hence reducing run-times and may match the underlying epidemiology better in some settings.
* Added increased prior flexibility with users able to alter the prior on the AR(1) process to approximate a random walk (via `beta`) and specify the expected amount of correlation between strains in the "correlated" model (`lkj`).
* Update the internal model configuration to be non-vectorised so that the case AR(1) process can be easily generalised to an AR(P) process. Thisfunctionality has not yet been exposed to users via the control interface.
* Added new functionality to delay case reporting and sequence reporting by some probability mass function. Thisfunctionality has not yet been exposed to users via the control interface.
* Made piecewise constant growth rates backwards looking from the last available data rather than forward looking as was previously the case. This means that forecasts are made using a full window of data to estimate the growth rate but also means that the precise dates of steps change as new data is added.
* Add a new argument `r_forecast` which controls if the growth rate is allowed to vary outside the range of the data. 
* Add optional adjustment for periodic case data based on a random effect. This can be used to account for day of the week or month effects.


### Evaluation and visualisation

* Added an optional integration with the [`scoringutils`](https://epiforecasts.io/scoringutils/) package to streamline evaluating forecasts using proper scoring rules.
* Added an example of using `fv_score_forecast()` to the README.
* Generalise plot labels. 
* Updated `plot_cases()` to use a log 2 scaling rather than a log scaling.
* Updated the default scaling to be logit for `plot_voc_frac().
* Added a new plotting argument to all plot functions - `central`. This controls whether central estimates (i.e the mean and median) are shown. In a change to the default visualisations these are now not shown automatically but by setting `central = TRUE` the user can elect to add these estimates to the plot. 

## Bug fixes

* Fixed a bug in the output of the `forecast()` function where the prior specifications for the initial growth and the variant scaling were only partially included.
* Tighten up overly permissive argument passing in the S3 methods that allowed users to pass arguments that didn't exist. This could lead to bugs in user code that are hard to spot. 
* Beta-binomial in the output log likelihood (but not during fitting) has been corrected to use the overdispersion for sequences and not cases. 
* Fixed a bug where `summary.fv_forecast()` failed to return diagnostics.
* Fixed a bug whereby certain combinations of data availability and truncation can lead to attempts to access non-existing indices. (@sbfnk in #111).

# forecast.vocs 0.0.8

## Features

* Adds `summary` and `plot` methods for `forecast()` and `fv_tidy_posterior()` as interfaces to lower level functions.
* Adds a new summary variable for the transmission advantage of the VOC to the output from
 `fv_tidy_posterior()`.
* Adds a new plotting function `plot_voc_advantage()` to extract and plot this.
* Adds `plot_voc_advantage()` to `plot_posterior` as an additional output.
* Added a new `timespan` argument that allows custom date processing. This allows the use
of data with non-weekly spaces (though spacing must be constant across both sequences and case counts). 
* Updates all example data to include this new output.
* Adds tests for all new functionality.

## Breaking changes

* The variant fraction label has been renamed `voc_frac` from `voc` due to the new variant advantage label.
* `plot_voc()` has been renamed `plot_voc_frac()` due to the new `plot_voc_advantage()` function.

## Bug fixes

* Spell checks documentation.
* Refactors model description to improve clarity.
* Fixed an issue where `voc_label` was not being passed by `forecast()` to lower level methods and hence new labels specifying the VOC were not being assigned.

# forecast.vocs 0.0.7

* Further generalised the `forecast` family to accept custom data and forecast extraction functions.
* Split generic posterior summary function from specific posterior summary function.
* Catch data passing bugs in `forecast`  with new tests
* Add argument passing in  `fv_posterior` to allow it be used with `forecast`.
* Added runtime to diagnostic fit output.
* Fixes `generate_obs()` to work with all strain models and adds unit tests for all function functionality.
* Fixed package building on `r-universe`.

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
