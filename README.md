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

See the following:

- `inst/forecast.R:` for a forecasting application.
- `inst/retrospective.R` for retrospective fitting and model evaluation