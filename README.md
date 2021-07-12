
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
#> Chain 4 finished in 13.5 seconds.
#> Chain 3 finished in 14.7 seconds.
#> Chain 2 finished in 15.9 seconds.
#> Chain 1 finished in 21.7 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 16.5 seconds.
#> Total execution time: 21.8 seconds.
#> 
#> Warning: 8 of 4000 (0.0%) transitions ended with a divergence.
#> This may indicate insufficient exploration of the posterior distribution.
#> Possible remedies include: 
#>   * Increasing adapt_delta closer to 1 (default is 0.8) 
#>   * Reparameterizing the model (e.g. using a non-centered parameterization)
#>   * Using informative or weakly informative prior distributions
#> Processing csv files: /tmp/Rtmp5mjLew/twostrainbp-202107121334-1-808693.csv, /tmp/Rtmp5mjLew/twostrainbp-202107121334-2-808693.csv, /tmp/Rtmp5mjLew/twostrainbp-202107121334-3-808693.csv, /tmp/Rtmp5mjLew/twostrainbp-202107121334-4-808693.csv
#> 
#> Checking sampler transitions treedepth.
#> Treedepth satisfactory for all transitions.
#> 
#> Checking sampler transitions for divergences.
#> 8 of 4000 (0.2%) transitions ended with a divergence.
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
#> Chain 1 finished in 4.9 seconds.
#> Chain 2 finished in 5.5 seconds.
#> Chain 4 finished in 7.0 seconds.
#> Chain 3 finished in 7.5 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 6.2 seconds.
#> Total execution time: 7.6 seconds.
#> Processing csv files: /tmp/Rtmp5mjLew/bp-202107121335-1-36ca92.csv, /tmp/Rtmp5mjLew/bp-202107121335-2-36ca92.csv, /tmp/Rtmp5mjLew/bp-202107121335-3-36ca92.csv, /tmp/Rtmp5mjLew/bp-202107121335-4-36ca92.csv
#> 
#> Checking sampler transitions treedepth.
#> Treedepth satisfactory for all transitions.
#> 
#> Checking sampler transitions for divergences.
#> 36 of 4000 (0.9%) transitions ended with a divergence.
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
#> Chain 2 finished in 13.8 seconds.
#> Chain 1 finished in 14.6 seconds.
#> Chain 4 finished in 14.8 seconds.
#> Chain 3 finished in 16.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 14.8 seconds.
#> Total execution time: 16.1 seconds.
#> Processing csv files: /tmp/Rtmp5mjLew/twostrainbp-202107121335-1-3a8bc2.csv, /tmp/Rtmp5mjLew/twostrainbp-202107121335-2-3a8bc2.csv, /tmp/Rtmp5mjLew/twostrainbp-202107121335-3-3a8bc2.csv, /tmp/Rtmp5mjLew/twostrainbp-202107121335-4-3a8bc2.csv
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
