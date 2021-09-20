
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
  germany_covid19_delta_obs,
  date = as.Date("2021-07-05")
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
#> Chain 3 finished in 76.6 seconds.
#> Chain 1 finished in 79.1 seconds.
#> Chain 2 finished in 100.4 seconds.
#> Chain 4 finished in 105.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 90.2 seconds.
#> Total execution time: 105.3 seconds.
#> 
#> Warning: 5 of 4000 (0.0%) transitions ended with a divergence.
#> This may indicate insufficient exploration of the posterior distribution.
#> Possible remedies include: 
#>   * Increasing adapt_delta closer to 1 (default is 0.8) 
#>   * Reparameterizing the model (e.g. using a non-centered parameterization)
#>   * Using informative or weakly informative prior distributions
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
#> Chain 2 finished in 28.0 seconds.
#> Chain 4 finished in 28.2 seconds.
#> Chain 1 finished in 32.8 seconds.
#> Chain 3 finished in 36.4 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 31.3 seconds.
#> Total execution time: 36.6 seconds.
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 finished in 70.3 seconds.
#> Chain 2 finished in 77.3 seconds.
#> Chain 3 finished in 80.2 seconds.
#> Chain 4 finished in 91.1 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 79.7 seconds.
#> Total execution time: 91.3 seconds.
```

Update variant of concern labels for the summarised posterior estimates.

``` r
results$posteriors <- update_voc_label(results$posteriors, "Delta")
```

Generate summary plots for the forecasts:

``` r
plots <- plot_posterior(
  results$posteriors, curr_obs,
  voc_label = "Delta variant"
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
