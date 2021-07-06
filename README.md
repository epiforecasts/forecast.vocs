# Two strain branching process

Adapted from work by Johannes Bracher (johannes.bracher@kit.edu). See: https://github.com/jbracher/branching_process_delta

A two strain branching process model for DELTA and non-DELTA COVID-19 cases applied to data in Germany. See the package vignette for further details and results.


## Installing

Either install the package from GitHub using the following, 

```r
devtools::install_github("seabbs/bp.delta", dependencies = TRUE)
```

Or install the package locally (with the working directory at the package root),

```r
devtools::install_dev_deps()
devtools::load_all()
```

Install stan using:

```r
cmdstanr::install_cmdstan()
```

## Using the model

Run a forecast using the following function call (change the `save_path` argument to alter the location where results are saved).

```r
options(mc.cores = 4)
results <- forecast(germany_cases, adapt_delta = 0.99, max_treedepth = 15)
```

See the following for more detailed analysis:

- `inst/forecast.R:` for a forecasting application.
- `inst/retrospective.R` for retrospective fitting and model evaluation