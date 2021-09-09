
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

obs <- latest_obs(germany_covid19_delta_obs)

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
#> Chain 2 [0.5,1,1,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan] 
#> Chain 2 [0.0001,3.69305e+114,2.68654e+262,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 2 [0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001] 
#> Chain 2 [0.0001,0.0001,0.0001,0.0001,0.0002,3.69305e+114,2.68654e+262,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 2 [11.3774,4.95583] 
#> Chain 2 [0.01,0.01] 
#> Chain 2 [-45.7549,-201.269] 
#> Chain 2 4 
#> Chain 2 [-86.0143,67.207,-46.9307,60.2333,-124.612,30.4346,44.909,58.0183,-145.453,117.145,36.94,-47.0388,77.0651,-40.0676,-155.243,-89.3659,63.2241,-40.6732,49.3277,-115.867,13.5535] 
#> Chain 2 [4,-82.0143,-14.8073,-61.7381,-1.50478,-126.126,-95.5762,-50.7462,7.16543,-138.189,-21.0414,15.924,-31.1443,45.8872,5.91601,-149.413,-238.772,-175.447,-216.091,-166.702,-282.556,-268.956] 
#> Chain 2 466.575 
#> Chain 2 finished in 155.0 seconds.
#> Chain 3 finished in 166.8 seconds.
#> Chain 4 finished in 169.2 seconds.
#> Chain 1 finished in 197.5 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 172.1 seconds.
#> Total execution time: 197.6 seconds.
#> 
#> Warning: 8 of 4000 (0.0%) transitions ended with a divergence.
#> This may indicate insufficient exploration of the posterior distribution.
#> Possible remedies include: 
#>   * Increasing adapt_delta closer to 1 (default is 0.8) 
#>   * Reparameterizing the model (e.g. using a non-centered parameterization)
#>   * Using informative or weakly informative prior distributions
#> Processing csv files: /tmp/RtmpzISXYT/twostrainbp-202109081635-1-66420f.csv, /tmp/RtmpzISXYT/twostrainbp-202109081635-2-66420f.csv, /tmp/RtmpzISXYT/twostrainbp-202109081635-3-66420f.csv, /tmp/RtmpzISXYT/twostrainbp-202109081635-4-66420f.csv
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
plot_cases(posterior, obs, log = TRUE)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

Plot the posterior prediction for the fraction of cases that have the
Delta variant.

``` r
plot_voc(posterior, obs)
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
#> Chain 2 [0.0001,inf,inf,inf,inf,inf,inf,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001] 
#> Chain 2 11.3774 
#> Chain 2 0.01 
#> Chain 2 -708.541 
#> Chain 2 13220 
#> Chain 2 [3.67291e+30,3.75265e+30,-8.71883e+28,-6.84504e+30,-1.56175e+31,-2.59402e+31,-3.63105e+31,-4.61758e+31,-5.4818e+31,-6.24796e+31,-6.7916e+31,-7.18881e+31,-7.40054e+31,-7.52894e+31,-7.54888e+31,-7.53718e+31,-7.50824e+31,-7.48654e+31,-7.49911e+31,-7.49068e+31,-7.47365e+31] 
#> Chain 2 [13220,3.67291e+30,7.42556e+30,7.33837e+30,4.93331e+29,-1.51242e+31,-4.10644e+31,-7.73749e+31,-1.23551e+32,-1.78369e+32,-2.40848e+32,-3.08764e+32,-3.80653e+32,-4.54658e+32,-5.29947e+32,-6.05436e+32,-6.80808e+32,-7.5589e+32,-8.30756e+32,-9.05747e+32,-9.80654e+32,-1.05539e+33] 
#> Chain 2 [106.48,7.73669e+55,7.70949e+109,6.40481e+163,4.76345e+217,2.15104e+271,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 2 11.3774 
#> Chain 2 0.01 
#> Chain 2 4.66796 
#> Chain 2 124.02 
#> Chain 2 [0.315879,-0.181882,-0.110675,-0.49894,-0.872309,-0.976972,-0.768793,-1.04049,-0.89354,-0.731552,-0.553898,-0.479066,-0.182119,-0.0569797,-0.126311,0.136955,-0.0410404,0.0277835,-0.0460974,0.202051,0.0424345] 
#> Chain 2 [124.02,124.336,124.154,124.044,123.545,122.672,121.695,120.927,119.886,118.992,118.261,117.707,117.228,117.046,116.989,116.863,117,116.958,116.986,116.94,117.142,117.185] 
#> Chain 3 [inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 3 11.3774 
#> Chain 3 0.01 
#> Chain 3 1384.45 
#> Chain 3 11036.1 
#> Chain 3 [2.31259e+09,2.44491e+09,1.76606e+09,1.95066e+09,1.60975e+09,4.79043e+08,-3.45395e+08,-7.90462e+07,-2.58974e+08,-4.06529e+07,-6.95157e+08,4.68604e+08,8.51456e+08,-2.16586e+08,7.83515e+08,5.10464e+08,-2.81164e+07,3.19384e+08,3.71606e+08,7.30309e+08,-8.97706e+08] 
#> Chain 3 [11036.1,2.3126e+09,4.75752e+09,6.52357e+09,8.47424e+09,1.0084e+10,1.0563e+10,1.02176e+10,1.01386e+10,9.87962e+09,9.83896e+09,9.14381e+09,9.61241e+09,1.04639e+10,1.02473e+10,1.10308e+10,1.15413e+10,1.15131e+10,1.18325e+10,1.22041e+10,1.29344e+10,1.20367e+10] 
#> Chain 3 [4.1499e+08,1.56749e+38,5.92693e+67,2.24582e+97,8.51326e+126,3.23206e+156,1.23024e+186,4.68288e+215,1.78733e+245,6.79973e+274,2.58662e+304,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 3 11.3774 
#> Chain 3 0.01 
#> Chain 3 19.8438 
#> Chain 3 68.1039 
#> Chain 3 [0.00106081,0.00211512,0.00040547,0.00152507,0.00259797,2.47301e-05,0.00269486,-0.00323798,-0.000105374,-4.7256e-05,-0.00156098,-0.00227497,0.00228045,0.00211198,-0.000158643,-0.00289687,0.00256004,0.00257621,0.00402393,-0.000530758,-0.000621871] 
#> Chain 3 [68.1039,68.105,68.1071,68.1075,68.109,68.1116,68.1117,68.1144,68.1111,68.111,68.111,68.1094,68.1071,68.1094,68.1115,68.1114,68.1085,68.111,68.1136,68.1176,68.1171,68.1165] 
#> Chain 4 [inf,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001] 
#> Chain 4 11.3774 
#> Chain 4 0.01 
#> Chain 4 1023.27 
#> Chain 4 -2166.04 
#> Chain 4 [-233819,-435829,-681669,-938676,-1.19602e+06,-1.4597e+06,-1.66674e+06,-1.90269e+06,-2.12711e+06,-2.24874e+06,-2.39051e+06,-2.46201e+06,-2.52494e+06,-2.57243e+06,-2.58873e+06,-2.57965e+06,-2.56001e+06,-2.54569e+06,-2.5675e+06,-2.53439e+06,-2.54068e+06] 
#> Chain 4 [-2166.04,-235985,-671813,-1.35348e+06,-2.29216e+06,-3.48818e+06,-4.94788e+06,-6.61462e+06,-8.51732e+06,-1.06444e+07,-1.28932e+07,-1.52837e+07,-1.77457e+07,-2.02706e+07,-2.28431e+07,-2.54318e+07,-2.80114e+07,-3.05714e+07,-3.31171e+07,-3.56846e+07,-3.8219e+07,-4.07597e+07] 
#> Chain 1 finished in 72.3 seconds.
#> Chain 3 finished in 82.1 seconds.
#> Chain 2 finished in 91.7 seconds.
#> Chain 4 finished in 102.3 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 87.1 seconds.
#> Total execution time: 102.4 seconds.
#> Processing csv files: /tmp/RtmpzISXYT/bp-202109081639-1-131c10.csv, /tmp/RtmpzISXYT/bp-202109081639-2-131c10.csv, /tmp/RtmpzISXYT/bp-202109081639-3-131c10.csv, /tmp/RtmpzISXYT/bp-202109081639-4-131c10.csv
#> 
#> Checking sampler transitions treedepth.
#> Treedepth satisfactory for all transitions.
#> 
#> Checking sampler transitions for divergences.
#> 5 of 4000 (0.12%) transitions ended with a divergence.
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
#> Chain 1 [0,0,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan] 
#> Chain 1 [6.00861e+75,7.11171e+205,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 1 [inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,2.5114e+300,7.32261e+276,2.32089e+260,8.36545e+236,4.10326e+219,6.66407e+190,3.96694e+174,4.16144e+147,1.62318e+132] 
#> Chain 1 [inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 1 [11.3774,4.95583] 
#> Chain 1 [0.01,0.01] 
#> Chain 1 [942.453,174.487] 
#> Chain 1 4 
#> Chain 1 [-2.70863,9.65783,-7.87515,-1.24247,-4.63666,-7.14989,-4.3348,-10.1186,-10.7556,-8.21589,-3.30467,-3.32914,4.96856,-9.11819,16.2032,-15.9954,14.1297,-26.4343,28.9258,-24.7689,26.654] 
#> Chain 1 [4,1.29137,10.9492,3.07405,1.83158,-2.81298,-9.9547,-14.2965,-24.4291,-35.1838,-43.4038,-46.7168,-50.0422,-45.0627,-54.1919,-37.9904,-53.9799,-39.8563,-66.29,-37.3601,-62.1219,-35.4803] 
#> Chain 1 297.673 
#> Chain 3 [6.84237e-40,7.0709e-42,7.34955e-44,2.65633e-44,1,1,1,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan] 
#> Chain 3 [0.0001,0.0001,0.0001,0.00266272,2.26811e+99,1.7245e+201,1.07722e+303,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 3 [1.43716e+27,7.84665e+28,8.24221e+30,1.16173e+33,1.46148e+35,1.41425e+37,1.36063e+39,1.0024e+41,6.51147e+42,3.37768e+44,1.39329e+46,4.08815e+47,9.43399e+48,1.73879e+50,4.0105e+51,8.30548e+52,1.44039e+54,1.73575e+55,2.97298e+56,2.57316e+57,1.44538e+58,6.3018e+58,2.95361e+59] 
#> Chain 3 [1.43716e+27,7.84665e+28,8.24221e+30,1.16173e+33,1.46148e+35,1.41425e+37,1.36063e+39,1.0024e+41,2.26811e+99,1.7245e+201,1.07722e+303,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 3 [11.3774,4.95583] 
#> Chain 3 [0.01,0.01] 
#> Chain 3 [62.5325,-710.811] 
#> Chain 3 4 
#> Chain 3 [0.654352,0.294043,-0.113681,-0.262397,-0.00159477,-0.256926,-0.112045,-0.217483,-0.224652,-0.334189,-0.233266,-0.229814,0.228746,-0.111132,-0.172254,-0.373157,0.350002,-0.689707,-0.435294,-0.245012,0.0737307] 
#> Chain 3 [4,4.65435,4.94839,4.83471,4.57232,4.56652,4.29962,4.17375,3.94879,3.71965,3.37901,3.13881,2.91404,3.13831,3.03059,2.85317,2.48911,2.84071,2.15816,1.72583,1.47246,1.54478] 
#> Chain 3 230.424 
#> Chain 4 finished in 177.1 seconds.
#> Chain 1 finished in 183.5 seconds.
#> Chain 2 finished in 193.5 seconds.
#> Chain 3 finished in 209.2 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 190.8 seconds.
#> Total execution time: 209.4 seconds.
#> Processing csv files: /tmp/RtmpzISXYT/twostrainbp-202109081641-1-130e2d.csv, /tmp/RtmpzISXYT/twostrainbp-202109081641-2-130e2d.csv, /tmp/RtmpzISXYT/twostrainbp-202109081641-3-130e2d.csv, /tmp/RtmpzISXYT/twostrainbp-202109081641-4-130e2d.csv
#> 
#> Checking sampler transitions treedepth.
#> Treedepth satisfactory for all transitions.
#> 
#> Checking sampler transitions for divergences.
#> 2 of 4000 (0.05%) transitions ended with a divergence.
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

Generate summary plots for the forecasts:

``` r
plots <- plot_posterior(results$posteriors)
```

Plot the posterior prediction for cases for both models.

``` r
plots$log_cases
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

Plot the posterior estimate for the effective reproduction number of
Delta, non-Delta cases, and overall.

``` r
plots$rt
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />
