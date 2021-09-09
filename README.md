
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Forecast case notifications using variant of concern strain dynamics

[![R-CMD-check](https://github.com/epiforecasts/forecast.vocs/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/forecast.vocs/actions/workflows/R-CMD-check.yaml)

This package contains models and processing code to allow sequencing of
variants of concern to be used to forecast case notifications.

## Installation

Either install the package from GitHub using the following,

``` r
devtools::install_github("epiforecasts/forecast.vocs", dependencies = TRUE)
```

Install stan to enable model fitting and forecasting using:

``` r
cmdstanr::install_cmdstan()
```

## Quick start

This quick start uses data from Germany that includes COVID-19
notificatons and sequences with sequences either being positive or
negative for the Delta variant.

### Step by step forecast

``` r
library(forecast.vocs)
options(mc.cores = 4)

obs <- filter_by_availability(
  germany_covid19_delta_obs, date = as.Date("2021-07-05")
)
curr_obs <- latest_obs(germany_covid19_delta_obs)

dt <- stan_data(obs, horizon = 4)

model <- load_model(strains = 2)

inits <- stan_inits(dt, strains = 2)

fit <- stan_fit(
  data = dt, model = model, init = inits,
  adapt_delta = 0.99, max_treedepth = 15,
  refresh = 0, show_messages = FALSE
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 2 finished in 82.7 seconds.
#> Chain 4 finished in 100.6 seconds.
#> Chain 1 finished in 102.4 seconds.
#> Chain 3 finished in 145.1 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 107.7 seconds.
#> Total execution time: 145.3 seconds.
#> 
#> Warning: 6 of 4000 (0.0%) transitions ended with a divergence.
#> This may indicate insufficient exploration of the posterior distribution.
#> Possible remedies include: 
#>   * Increasing adapt_delta closer to 1 (default is 0.8) 
#>   * Reparameterizing the model (e.g. using a non-centered parameterization)
#>   * Using informative or weakly informative prior distributions
#> Processing csv files: /tmp/RtmplLeyRX/twostrainbp-202109091130-1-36c387.csv, /tmp/RtmplLeyRX/twostrainbp-202109091130-2-36c387.csv, /tmp/RtmplLeyRX/twostrainbp-202109091130-3-36c387.csv, /tmp/RtmplLeyRX/twostrainbp-202109091130-4-36c387.csv
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

posterior <- summarise_posterior(fit)
posterior <- update_voc_label(posterior, "Delta")
```

Plot the posterior prediction for cases.

``` r
plot_cases(posterior, curr_obs, log = TRUE)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

Plot the posterior prediction for the fraction of cases that have the
Delta variant.

``` r
plot_voc(posterior, curr_obs, voc_label = "Delta variant")
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

Plot the posterior estimate for the effective reproduction number of
Delta and non-Delta cases.

``` r
plot_rt(posterior)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

### Forecast wrapper

Run a complete forecast for both the one and two strain models using the
`forecast` function (change the `save_path` argument to alter the
location where results are saved). See `names(results)` for a breakdown
of the output (including summarised posteriors and plots).

``` r
results <- forecast(obs,
  strains = c(1, 2),
  adapt_delta = 0.99, max_treedepth = 15,
  refresh = 0, show_messages = FALSE
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 2 [0.0001,0.0001,2.61538e+141,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 2 11.3774 
#> Chain 2 0.01 
#> Chain 2 -1139.31 
#> Chain 2 732.521 
#> Chain 2 [-0.102317,-0.16192,-0.207311,-0.149114,-0.309786,-0.483704,-0.724303,-1.05227,-0.941118,-0.667717,-0.927358,-0.590029,-0.976228,-0.647173,-0.581032,-0.679033,-0.632327,-0.688291] 
#> Chain 2 [732.521,732.419,732.257,732.05,731.901,731.591,731.107,730.383,729.331,728.39,727.722,726.795,726.205,725.228,724.581,724,723.321,722.689,722] 
#> Chain 3 [2.42752e+71,1.24623e+245,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 3 11.3774 
#> Chain 3 0.01 
#> Chain 3 164.37 
#> Chain 3 399.983 
#> Chain 3 [-0.561411,-7.36534,-3.89348,2.81783,0.391361,-2.88033,-9.87789,-13.6004,-13.9848,-8.60623,-5.68096,-4.83658,-6.56518,-2.37839,-7.04788,-4.43691,-7.24552,-7.21597] 
#> Chain 3 [399.983,399.422,392.056,388.163,390.981,391.372,388.492,378.614,365.013,351.029,342.422,336.741,331.905,325.34,322.961,315.913,311.476,304.231,297.015] 
#> Chain 2 finished in 33.7 seconds.
#> Chain 1 finished in 37.5 seconds.
#> Chain 3 finished in 38.6 seconds.
#> Chain 4 finished in 41.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 37.7 seconds.
#> Total execution time: 41.1 seconds.
#> Processing csv files: /tmp/RtmplLeyRX/bp-202109091132-1-331678.csv, /tmp/RtmplLeyRX/bp-202109091132-2-331678.csv, /tmp/RtmplLeyRX/bp-202109091132-3-331678.csv, /tmp/RtmplLeyRX/bp-202109091132-4-331678.csv
#> 
#> Checking sampler transitions treedepth.
#> Treedepth satisfactory for all transitions.
#> 
#> Checking sampler transitions for divergences.
#> No divergent transitions found.
#> 
#> Checking E-BFMI - sampler transitions HMC potential energy.
#> E-BFMI satisfactory.
#> 
#> Effective sample size satisfactory.
#> 
#> Split R-hat values satisfactory all parameters.
#> 
#> Processing complete, no problems detected.
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 4 finished in 96.6 seconds.
#> Chain 3 finished in 108.8 seconds.
#> Chain 1 finished in 124.4 seconds.
#> Chain 2 finished in 139.2 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 117.2 seconds.
#> Total execution time: 139.3 seconds.
#> Processing csv files: /tmp/RtmplLeyRX/twostrainbp-202109091133-1-62874f.csv, /tmp/RtmplLeyRX/twostrainbp-202109091133-2-62874f.csv, /tmp/RtmplLeyRX/twostrainbp-202109091133-3-62874f.csv, /tmp/RtmplLeyRX/twostrainbp-202109091133-4-62874f.csv
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
```

Update variant of concern labels for the summarised posterior estimates.

``` r
results$posteriors <- update_voc_label(results$posteriors, "Delta")
```

Generate summary plots for the forecasts:

``` r
plots <- plot_posterior(
  results$posteriors, curr_obs, voc_label = "Delta variant"
)
```

Plot the posterior prediction for cases for both models.

``` r
plots$log_cases
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

Plot the posterior estimate for the effective reproduction number of
Delta, non-Delta cases, and overall.

``` r
plots$rt
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />
