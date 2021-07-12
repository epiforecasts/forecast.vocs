
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Evaluating the impact of modelling strain dynamics on short-term COVID-19 forecast performance

[![R-CMD-check](https://github.com/epiforecasts/bp.delta/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/bp.delta/actions)
[![paper](https://github.com/epiforecasts/bp.delta/workflows/paper/badge.svg)](https://github.com/epiforecasts/bp.delta/actions)

This repository contains stan code for one and two strain branching
process process models as well as an R package for using these models to
produce forecasts of COVID-19 notified cass in Germany. See the paper
for further method details and the rest of this README for installation
and example functionality.

## Installation

Either install the package from GitHub using the following,

``` r
devtools::install_github("seabbs/bp.delta", dependencies = TRUE)
```

Or install the package locally (with the working directory at the
package root),

``` r
devtools::install_dev_deps()
devtools::load_all()
```

Install stan using:

``` r
cmdstanr::install_cmdstan()
```

## Using the model

### Step by step forecast

*Note: Stan throws some initialisation warnings at the moment and a low
number of divergent transitions (\<0.5%) regardless of auto-regressive
model formulation or stan settings. This is a work in progress - remove
the `refresh = 0, show_messages = FALSE` settings to see these issues.*

``` r
library(bp.delta)
options(mc.cores = 4)

dt <- stan_data(germany_cases, horizon = 4)

model <- load_model(strains = 2)

inits <- stan_inits(dt, strains = 2)

fit <- stan_fit(data = dt, model = model, init = inits,
                adapt_delta = 0.99, max_treedepth = 15,
                refresh = 0, show_messages = FALSE)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 2 finished in 13.9 seconds.
#> Chain 3 finished in 14.0 seconds.
#> Chain 4 finished in 16.0 seconds.
#> Chain 1 finished in 18.9 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 15.7 seconds.
#> Total execution time: 19.0 seconds.
#> 
#> Warning: 9 of 4000 (0.0%) transitions ended with a divergence.
#> This may indicate insufficient exploration of the posterior distribution.
#> Possible remedies include: 
#>   * Increasing adapt_delta closer to 1 (default is 0.8) 
#>   * Reparameterizing the model (e.g. using a non-centered parameterization)
#>   * Using informative or weakly informative prior distributions
#> Processing csv files: /tmp/RtmpUIDlKk/twostrainbp-202107121605-1-691874.csv, /tmp/RtmpUIDlKk/twostrainbp-202107121605-2-691874.csv, /tmp/RtmpUIDlKk/twostrainbp-202107121605-3-691874.csv, /tmp/RtmpUIDlKk/twostrainbp-202107121605-4-691874.csv
#> 
#> Checking sampler transitions treedepth.
#> Treedepth satisfactory for all transitions.
#> 
#> Checking sampler transitions for divergences.
#> 9 of 4000 (0.23%) transitions ended with a divergence.
#> These divergent transitions indicate that HMC is not fully able to explore the posterior distribution.
#> Try increasing adapt delta closer to 1.
#> If this doesn't remove all divergences, try to reparameterize the model.
#> 
#> Checking E-BFMI - sampler transitions HMC potential energy.
#> E-BFMI satisfactory.
#> 
#> Effective sample size satisfactory.
#> 
#> Split R-hat values satisfactory all parameters.
#> 
#> Processing complete.

posterior <- summarise_posterior(fit)
```

Plot the posterior prediction for cases.

``` r
plot_cases(posterior, germany_cases, max(germany_cases$date), log = TRUE)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

Plot the posterior prediction for the fraction of cases that are DELTA.

``` r
plot_delta(posterior, germany_cases, max(germany_cases$date))
#> Warning: Removed 2 rows containing missing values (geom_point).
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

Plot the posterior estimate for the effective reproduction number of
DELTA and non-DELTA cases.

``` r
plot_rt(posterior, max(germany_cases$date))
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

### Forecast wrapper

Run a complete forecast for both the one and two strain models using the
`forecast` function (change the `save_path` argument to alter the
location where results are saved). See `names(results)` for a breakdown
of the output (including summarised posteriors and plots).

``` r
results <- forecast(germany_cases, strains = c(1, 2),
                    adapt_delta = 0.99, max_treedepth = 15,
                    refresh = 0, show_messages = FALSE)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 3 finished in 5.9 seconds.
#> Chain 1 finished in 8.7 seconds.
#> Chain 2 finished in 9.8 seconds.
#> Chain 4 finished in 10.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 8.6 seconds.
#> Total execution time: 10.1 seconds.
#> Processing csv files: /tmp/RtmpUIDlKk/bp-202107121605-1-860b1f.csv, /tmp/RtmpUIDlKk/bp-202107121605-2-860b1f.csv, /tmp/RtmpUIDlKk/bp-202107121605-3-860b1f.csv, /tmp/RtmpUIDlKk/bp-202107121605-4-860b1f.csv
#> 
#> Checking sampler transitions treedepth.
#> Treedepth satisfactory for all transitions.
#> 
#> Checking sampler transitions for divergences.
#> 6 of 4000 (0.15%) transitions ended with a divergence.
#> These divergent transitions indicate that HMC is not fully able to explore the posterior distribution.
#> Try increasing adapt delta closer to 1.
#> If this doesn't remove all divergences, try to reparameterize the model.
#> 
#> Checking E-BFMI - sampler transitions HMC potential energy.
#> E-BFMI satisfactory.
#> 
#> Effective sample size satisfactory.
#> 
#> Split R-hat values satisfactory all parameters.
#> 
#> Processing complete.
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 4 finished in 14.6 seconds.
#> Chain 1 finished in 15.2 seconds.
#> Chain 3 finished in 15.6 seconds.
#> Chain 2 finished in 20.3 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 16.4 seconds.
#> Total execution time: 20.4 seconds.
#> Processing csv files: /tmp/RtmpUIDlKk/twostrainbp-202107121605-1-253e38.csv, /tmp/RtmpUIDlKk/twostrainbp-202107121605-2-253e38.csv, /tmp/RtmpUIDlKk/twostrainbp-202107121605-3-253e38.csv, /tmp/RtmpUIDlKk/twostrainbp-202107121605-4-253e38.csv
#> 
#> Checking sampler transitions treedepth.
#> Treedepth satisfactory for all transitions.
#> 
#> Checking sampler transitions for divergences.
#> 1 of 4000 (0.025%) transitions ended with a divergence.
#> These divergent transitions indicate that HMC is not fully able to explore the posterior distribution.
#> Try increasing adapt delta closer to 1.
#> If this doesn't remove all divergences, try to reparameterize the model.
#> 
#> Checking E-BFMI - sampler transitions HMC potential energy.
#> E-BFMI satisfactory.
#> 
#> Effective sample size satisfactory.
#> 
#> Split R-hat values satisfactory all parameters.
#> 
#> Processing complete.
#> Warning: Transformation introduced infinite values in continuous y-axis
#> Warning: Removed 2 rows containing missing values (geom_point).
```

Plot the posterior prediction for cases for both models.

``` r
results$plots$log_cases
#> Warning: Transformation introduced infinite values in continuous y-axis
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

Plot the posterior estimate for the effective reproduction number of
DELTA, non-DELTA cases, and overall.

``` r
results$plots$rt
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

### Further details

See the following for more detailed analysis:

  - `inst/forecast.R:` for a forecasting application.
  - `inst/retrospective.R` for retrospective fitting and model
    evaluation
