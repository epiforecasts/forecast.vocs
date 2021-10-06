
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Forecast case notifications using variant of concern strain dynamics

[![R-CMD-check](https://github.com/epiforecasts/forecast.vocs/workflows/R-CMD-check/badge.svg)](https://github.com/epiforecasts/forecast.vocs/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/epiforecasts/forecast.vocs/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epiforecasts/forecast.vocs)

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
#> Chain 3 finished in 39.7 seconds.
#> Chain 1 finished in 55.6 seconds.
#> Chain 4 finished in 56.3 seconds.
#> Chain 2 finished in 59.1 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 52.7 seconds.
#> Total execution time: 59.3 seconds.
#> 
#> Warning: 3 of 4000 (0.0%) transitions ended with a divergence.
#> This may indicate insufficient exploration of the posterior distribution.
#> Possible remedies include: 
#>   * Increasing adapt_delta closer to 1 (default is 0.8) 
#>   * Reparameterizing the model (e.g. using a non-centered parameterization)
#>   * Using informative or weakly informative prior distributions

posterior <- summarise_posterior(fit, voc_label = "Delta")
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
`forecast` function. This provides a wrapper around the individual
functions used above. Multiple forecasts can be performed efficiently
across dates and scenarios using `forecast_across_dates()` and
`forecast_accross_scenarios()`.

``` r
forecasts <- forecast(obs,
  strains = c(1, 2), voc_label = "Delta",
  adapt_delta = 0.99, max_treedepth = 15,
  refresh = 0, show_messages = FALSE,
)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 2 finished in 16.4 seconds.
#> Chain 3 finished in 16.6 seconds.
#> Chain 4 finished in 17.0 seconds.
#> Chain 1 finished in 22.7 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 18.2 seconds.
#> Total execution time: 22.8 seconds.
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 4 finished in 38.4 seconds.
#> Chain 2 finished in 43.3 seconds.
#> Chain 1 finished in 49.9 seconds.
#> Chain 3 finished in 87.7 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 54.9 seconds.
#> Total execution time: 87.8 seconds.
```

Unnest posterior estimates from each model.

``` r
posteriors <- unnest_posterior(forecasts)
posteriors
#>      id forecast_date strains overdispersion variant_relationship r_init
#>   1:  0    2021-07-03       1           TRUE               pooled 0,0.25
#>   2:  0    2021-07-03       1           TRUE               pooled 0,0.25
#>   3:  0    2021-07-03       1           TRUE               pooled 0,0.25
#>   4:  0    2021-07-03       1           TRUE               pooled 0,0.25
#>   5:  0    2021-07-03       1           TRUE               pooled 0,0.25
#>  ---                                                                    
#> 608:  0    2021-07-03       2           TRUE               pooled 0,0.25
#> 609:  0    2021-07-03       2           TRUE               pooled 0,0.25
#> 610:  0    2021-07-03       2           TRUE               pooled 0,0.25
#> 611:  0    2021-07-03       2           TRUE               pooled 0,0.25
#> 612:  0    2021-07-03       2           TRUE               pooled 0,0.25
#>      voc_scale error               fit       data  fit_args samples max_rhat
#>   1:     0,0.2       <CmdStanMCMC[30]> <list[20]> <list[5]>    4000 1.005186
#>   2:     0,0.2       <CmdStanMCMC[30]> <list[20]> <list[5]>    4000 1.005186
#>   3:     0,0.2       <CmdStanMCMC[30]> <list[20]> <list[5]>    4000 1.005186
#>   4:     0,0.2       <CmdStanMCMC[30]> <list[20]> <list[5]>    4000 1.005186
#>   5:     0,0.2       <CmdStanMCMC[30]> <list[20]> <list[5]>    4000 1.005186
#>  ---                                                                        
#> 608:     0,0.2       <CmdStanMCMC[30]> <list[20]> <list[5]>    4000 1.003409
#> 609:     0,0.2       <CmdStanMCMC[30]> <list[20]> <list[5]>    4000 1.003409
#> 610:     0,0.2       <CmdStanMCMC[30]> <list[20]> <list[5]>    4000 1.003409
#> 611:     0,0.2       <CmdStanMCMC[30]> <list[20]> <list[5]>    4000 1.003409
#> 612:     0,0.2       <CmdStanMCMC[30]> <list[20]> <list[5]>    4000 1.003409
#>      divergent_transitions per_divergent_transitons max_treedepth
#>   1:                     5                  0.00125            11
#>   2:                     5                  0.00125            11
#>   3:                     5                  0.00125            11
#>   4:                     5                  0.00125            11
#>   5:                     5                  0.00125            11
#>  ---                                                             
#> 608:                     8                  0.00200            12
#> 609:                     8                  0.00200            12
#> 610:                     8                  0.00200            12
#> 611:                     8                  0.00200            12
#> 612:                     8                  0.00200            12
#>      no_at_max_treedepth per_at_max_treedepth            forecast value_type
#>   1:                 755              0.18875 <data.table[10x12]>      model
#>   2:                 755              0.18875 <data.table[10x12]>      model
#>   3:                 755              0.18875 <data.table[10x12]>      model
#>   4:                 755              0.18875 <data.table[10x12]>      model
#>   5:                 755              0.18875 <data.table[10x12]>      cases
#>  ---                                                                        
#> 608:                 998              0.24950 <data.table[48x12]>        raw
#> 609:                 998              0.24950 <data.table[48x12]>        raw
#> 610:                 998              0.24950 <data.table[48x12]>        raw
#> 611:                 998              0.24950 <data.table[48x12]>        raw
#> 612:                 998              0.24950 <data.table[48x12]>        raw
#>           variable                  clean_name       date    type   obs
#>   1:          beta                        Beta       <NA>    <NA>    NA
#>   2:        phi[1] Notification overdispersion       <NA>    <NA>    NA
#>   3:        r_init              Initial growth       <NA>    <NA>    NA
#>   4:       r_noise                 Growth (sd)       <NA>    <NA>    NA
#>   5:  sim_cases[1]                        <NA> 2021-03-20 Overall 87328
#>  ---                                                                   
#> 608: sim_cases[16]                        <NA>       <NA>    <NA>    NA
#> 609: sim_cases[17]                        <NA>       <NA>    <NA>    NA
#> 610: sim_cases[18]                        <NA>       <NA>    <NA>    NA
#> 611: sim_cases[19]                        <NA>       <NA>    <NA>    NA
#> 612: sim_cases[20]                        <NA>       <NA>    <NA>    NA
#>      observed exponentiated         mean       median           sd          mad
#>   1:       NA         FALSE 1.731518e-01 2.112630e-01 4.691284e-01 5.451431e-01
#>   2:       NA         FALSE 9.859423e+01 7.496235e+01 1.025173e+02 4.742756e+01
#>   3:       NA         FALSE 1.376726e-01 1.375165e-01 7.377239e-02 7.168208e-02
#>   4:       NA         FALSE 1.196517e-01 1.129010e-01 5.787825e-02 5.915055e-02
#>   5:     TRUE            NA 9.006464e+04 8.922600e+04 1.337581e+04 1.167918e+04
#>  ---                                                                           
#> 608:       NA            NA 4.251785e+03 4.187000e+03 7.874036e+02 5.678358e+02
#> 609:       NA            NA 3.970601e+03 3.791000e+03 1.165540e+03 9.770334e+02
#> 610:       NA            NA 4.107717e+03 3.677000e+03 2.086436e+03 1.573780e+03
#> 611:       NA            NA 4.687944e+03 3.776000e+03 4.102238e+03 2.318045e+03
#> 612:       NA            NA 5.982679e+03 3.855500e+03 9.713122e+03 3.177212e+03
#>                 q5           q20          q80          q95      rhat ess_bulk
#>   1: -6.427214e-01 -2.686274e-01 6.198720e-01 8.723930e-01 1.0010027 1550.619
#>   2:  2.326636e+01  4.232796e+01 1.278786e+02 2.376844e+02 1.0038973 1114.907
#>   3:  1.687728e-02  7.818434e-02 1.988848e-01 2.587547e-01 1.0011228 1804.901
#>   4:  4.090853e-02  6.697418e-02 1.660348e-01 2.245749e-01 1.0024508 1472.056
#>   5:  6.949270e+04  7.973560e+04 9.992880e+04 1.128736e+05 0.9997794 3524.518
#>  ---                                                                         
#> 608:  3.330850e+03  3.724000e+03 4.698200e+03 5.388150e+03 0.9997712 3897.544
#> 609:  2.449950e+03  3.059000e+03 4.760000e+03 6.072050e+03 1.0006151 4038.941
#> 610:  1.726950e+03  2.559800e+03 5.336200e+03 7.775300e+03 0.9994742 4134.009
#> 611:  1.149950e+03  2.127800e+03 6.377600e+03 1.081515e+04 0.9998705 4264.314
#> 612:  7.249500e+02  1.727000e+03 7.989400e+03 1.645260e+04 0.9996070 4342.985
#>       ess_tail
#>   1: 1826.0841
#>   2:  949.7581
#>   3: 2396.5365
#>   4: 2037.4316
#>   5: 3418.5038
#>  ---          
#> 608: 3193.2378
#> 609: 3744.9236
#> 610: 3612.7405
#> 611: 3299.0010
#> 612: 3241.6141
```

Generate summary plots for the forecasts:

``` r
plots <- plot_posterior(
  posteriors, curr_obs,
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
