
<!-- README.md is generated from README.Rmd. Please edit that file -->

# One and two strain branching process forecasts

Adapted from work by Johannes Bracher (<johannes.bracher@kit.edu>). See:
<https://github.com/jbracher/branching_process_delta>

A one and two strain branching process model for DELTA and non-DELTA
COVID-19 cases applied to data from Germany. See the vignette for
further method details.

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

dt <- stan_data(germany_cases, horizon = 4)

model <- load_model(strains = 2)

inits <- stan_inits(dt, strains = 2)

fit <- stan_fit(data = dt, model = model, init = inits,
                adapt_delta = 0.99, max_treedepth = 15,
                refresh = 0, show_messages = FALSE)
#> Running MCMC with 4 sequential chains...
#> 
#> Chain 1 finished in 12.8 seconds.
#> Chain 2 finished in 12.1 seconds.
#> Chain 3 finished in 12.7 seconds.
#> Chain 4 finished in 13.8 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 12.9 seconds.
#> Total execution time: 51.9 seconds.
#> 
#> Warning: 4 of 4000 (0.0%) transitions ended with a divergence.
#> This may indicate insufficient exploration of the posterior distribution.
#> Possible remedies include: 
#>   * Increasing adapt_delta closer to 1 (default is 0.8) 
#>   * Reparameterizing the model (e.g. using a non-centered parameterization)
#>   * Using informative or weakly informative prior distributions
#> Processing csv files: /tmp/RtmpG9wRBP/twostrainbp-202107092220-1-462fa6.csv, /tmp/RtmpG9wRBP/twostrainbp-202107092220-2-462fa6.csv, /tmp/RtmpG9wRBP/twostrainbp-202107092220-3-462fa6.csv, /tmp/RtmpG9wRBP/twostrainbp-202107092220-4-462fa6.csv
#> 
#> Checking sampler transitions treedepth.
#> Treedepth satisfactory for all transitions.
#> 
#> Checking sampler transitions for divergences.
#> 4 of 4000 (0.1%) transitions ended with a divergence.
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
#> Warning: Removed 1 rows containing missing values (geom_point).
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
options(mc.cores = 4)
results <- forecast(germany_cases, strains = c(1, 2),
                    adapt_delta = 0.99, max_treedepth = 15,
                    refresh = 0, show_messages = FALSE)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 4 finished in 3.1 seconds.
#> Chain 1 finished in 3.5 seconds.
#> Chain 3 finished in 3.7 seconds.
#> Chain 2 finished in 4.7 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 3.7 seconds.
#> Total execution time: 4.8 seconds.
#> Processing csv files: /tmp/RtmpG9wRBP/bp-202107092221-1-63c61c.csv, /tmp/RtmpG9wRBP/bp-202107092221-2-63c61c.csv, /tmp/RtmpG9wRBP/bp-202107092221-3-63c61c.csv, /tmp/RtmpG9wRBP/bp-202107092221-4-63c61c.csv
#> 
#> Checking sampler transitions treedepth.
#> Treedepth satisfactory for all transitions.
#> 
#> Checking sampler transitions for divergences.
#> 3 of 4000 (0.075%) transitions ended with a divergence.
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
#> Chain 4 finished in 9.5 seconds.
#> Chain 1 finished in 9.7 seconds.
#> Chain 3 finished in 11.1 seconds.
#> Chain 2 finished in 13.3 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 10.9 seconds.
#> Total execution time: 13.4 seconds.
#> Processing csv files: /tmp/RtmpG9wRBP/twostrainbp-202107092221-1-570c83.csv, /tmp/RtmpG9wRBP/twostrainbp-202107092221-2-570c83.csv, /tmp/RtmpG9wRBP/twostrainbp-202107092221-3-570c83.csv, /tmp/RtmpG9wRBP/twostrainbp-202107092221-4-570c83.csv
#> 
#> Checking sampler transitions treedepth.
#> Treedepth satisfactory for all transitions.
#> 
#> Checking sampler transitions for divergences.
#> 7 of 4000 (0.17%) transitions ended with a divergence.
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
#> Warning: Removed 1 rows containing missing values (geom_point).
```

Plot the posterior prediction for cases for both models.

``` r
results$plots$log_cases
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
