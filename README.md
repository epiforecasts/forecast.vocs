
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Evaluating the impact of modelling strain dynamics on short-term COVID-19 forecast performance

[![R-CMD-check](https://github.com/epiforecasts/bp.delta/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/bp.delta/actions/workflows/R-CMD-check.yaml)
[![paper](https://github.com/epiforecasts/bp.delta/workflows/paper/badge.svg)](https://github.com/epiforecasts/bp.delta/actions/workflows/paper.yaml)

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
#> Chain 2 finished in 13.5 seconds.
#> Chain 1 finished in 15.2 seconds.
#> Chain 4 finished in 15.6 seconds.
#> Chain 3 finished in 17.7 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 15.5 seconds.
#> Total execution time: 17.9 seconds.
#> 
#> Warning: 18 of 4000 (0.0%) transitions ended with a divergence.
#> This may indicate insufficient exploration of the posterior distribution.
#> Possible remedies include: 
#>   * Increasing adapt_delta closer to 1 (default is 0.8) 
#>   * Reparameterizing the model (e.g. using a non-centered parameterization)
#>   * Using informative or weakly informative prior distributions
#> Processing csv files: /tmp/Rtmp314rAB/twostrainbp-202107121720-1-1d514e.csv, /tmp/Rtmp314rAB/twostrainbp-202107121720-2-1d514e.csv, /tmp/Rtmp314rAB/twostrainbp-202107121720-3-1d514e.csv, /tmp/Rtmp314rAB/twostrainbp-202107121720-4-1d514e.csv
#> 
#> Checking sampler transitions treedepth.
#> Treedepth satisfactory for all transitions.
#> 
#> Checking sampler transitions for divergences.
#> 18 of 4000 (0.45%) transitions ended with a divergence.
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
#> Warning: Transformation introduced infinite values in continuous y-axis
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
#> Chain 2 finished in 5.0 seconds.
#> Chain 3 finished in 5.9 seconds.
#> Chain 1 finished in 6.3 seconds.
#> Chain 4 finished in 14.8 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 8.0 seconds.
#> Total execution time: 14.9 seconds.
#> Processing csv files: /tmp/Rtmp314rAB/bp-202107121720-1-925993.csv, /tmp/Rtmp314rAB/bp-202107121720-2-925993.csv, /tmp/Rtmp314rAB/bp-202107121720-3-925993.csv, /tmp/Rtmp314rAB/bp-202107121720-4-925993.csv
#> 
#> Checking sampler transitions treedepth.
#> Treedepth satisfactory for all transitions.
#> 
#> Checking sampler transitions for divergences.
#> 99 of 4000 (2.5%) transitions ended with a divergence.
#> These divergent transitions indicate that HMC is not fully able to explore the posterior distribution.
#> Try increasing adapt delta closer to 1.
#> If this doesn't remove all divergences, try to reparameterize the model.
#> 
#> Checking E-BFMI - sampler transitions HMC potential energy.
#> E-BFMI satisfactory.
#> 
#> Effective sample size satisfactory.
#> 
#> The following parameters had split R-hat greater than 1.1:
#>   mean_cases[16], sim_cases[16]
#> Such high values indicate incomplete mixing and biased estimation.
#> You should consider regularizating your model with additional prior information or a more effective parameterization.
#> 
#> Processing complete.
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 4 finished in 12.4 seconds.
#> Chain 3 finished in 14.1 seconds.
#> Chain 1 finished in 14.3 seconds.
#> Chain 2 finished in 19.3 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 15.0 seconds.
#> Total execution time: 19.4 seconds.
#> Processing csv files: /tmp/Rtmp314rAB/twostrainbp-202107121721-1-1b3aac.csv, /tmp/Rtmp314rAB/twostrainbp-202107121721-2-1b3aac.csv, /tmp/Rtmp314rAB/twostrainbp-202107121721-3-1b3aac.csv, /tmp/Rtmp314rAB/twostrainbp-202107121721-4-1b3aac.csv
#> 
#> Checking sampler transitions treedepth.
#> Treedepth satisfactory for all transitions.
#> 
#> Checking sampler transitions for divergences.
#> 25 of 4000 (0.62%) transitions ended with a divergence.
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
