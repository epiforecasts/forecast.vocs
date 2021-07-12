
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
options(mc.cores = 4)

dt <- stan_data(germany_cases, horizon = 4)

model <- load_model(strains = 2)

inits <- stan_inits(dt, strains = 2)

fit <- stan_fit(data = dt, model = model, init = inits,
                adapt_delta = 0.99, max_treedepth = 15,
                refresh = 0, show_messages = FALSE)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 2 finished in 13.4 seconds.
#> Chain 1 finished in 13.9 seconds.
#> Chain 3 finished in 13.8 seconds.
#> Chain 4 finished in 14.2 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 13.8 seconds.
#> Total execution time: 14.3 seconds.
#> 
#> Warning: 4 of 4000 (0.0%) transitions ended with a divergence.
#> This may indicate insufficient exploration of the posterior distribution.
#> Possible remedies include: 
#>   * Increasing adapt_delta closer to 1 (default is 0.8) 
#>   * Reparameterizing the model (e.g. using a non-centered parameterization)
#>   * Using informative or weakly informative prior distributions
#> Processing csv files: /tmp/RtmpCZNZMR/twostrainbp-202107121114-1-05672d.csv, /tmp/RtmpCZNZMR/twostrainbp-202107121114-2-05672d.csv, /tmp/RtmpCZNZMR/twostrainbp-202107121114-3-05672d.csv, /tmp/RtmpCZNZMR/twostrainbp-202107121114-4-05672d.csv
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
results <- forecast(germany_cases, strains = c(1, 2),
                    adapt_delta = 0.99, max_treedepth = 15,
                    refresh = 0, show_messages = FALSE)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 2 finished in 3.6 seconds.
#> Chain 3 finished in 4.0 seconds.
#> Chain 1 finished in 4.2 seconds.
#> Chain 4 finished in 5.5 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 4.3 seconds.
#> Total execution time: 5.5 seconds.
#> Processing csv files: /tmp/RtmpCZNZMR/bp-202107121114-1-22c6d4.csv, /tmp/RtmpCZNZMR/bp-202107121114-2-22c6d4.csv, /tmp/RtmpCZNZMR/bp-202107121114-3-22c6d4.csv, /tmp/RtmpCZNZMR/bp-202107121114-4-22c6d4.csv
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
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 2 finished in 13.0 seconds.
#> Chain 4 finished in 13.3 seconds.
#> Chain 3 finished in 14.4 seconds.
#> Chain 1 finished in 15.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 13.9 seconds.
#> Total execution time: 15.0 seconds.
#> Processing csv files: /tmp/RtmpCZNZMR/twostrainbp-202107121114-1-92689d.csv, /tmp/RtmpCZNZMR/twostrainbp-202107121114-2-92689d.csv, /tmp/RtmpCZNZMR/twostrainbp-202107121114-3-92689d.csv, /tmp/RtmpCZNZMR/twostrainbp-202107121114-4-92689d.csv
#> 
#> Checking sampler transitions treedepth.
#> Treedepth satisfactory for all transitions.
#> 
#> Checking sampler transitions for divergences.
#> 12 of 4000 (0.3%) transitions ended with a divergence.
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
