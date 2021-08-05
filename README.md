
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

obs <- latest_obs(germany_obs)

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
#> Chain 2 [0,-nan,0,0,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan] 
#> Chain 2 [0.0001,inf,0.0001,0.0001,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 2 [inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 2 [inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 2 [11.3774,4.95583] 
#> Chain 2 [0.01,0.01] 
#> Chain 2 [8600.18,-6880.84] 
#> Chain 2 4 
#> Chain 2 [1.68563e+12,1.57216e+13,5.23134e+12,4.58028e+11,-1.74394e+12,2.47806e+13,-1.93138e+13,2.3351e+13,-1.11183e+13,1.41806e+13,-1.40985e+13,2.14867e+13,-4.02579e+12,-1.57289e+13,2.38155e+13,-4.17872e+13,5.80368e+13,-5.76575e+13,6.59353e+13,-5.35196e+13,3.55791e+13] 
#> Chain 2 [4,1.68563e+12,1.74073e+13,2.26386e+13,2.30966e+13,3.0475e+25,6.2691e+25,9.32419e+25,1.78308e+26,1.2125e+26,6.14254e+25,3.26091e+25,-2.69478e+25,-4.90531e+24,-3.47592e+25,-4.30121e+25,-1.20952e+26,-3.50157e+25,-7.5442e+25,-6.34741e+25,4.59961e+25,3.47087e+25] 
#> Chain 2 1771.47 
#> Chain 4 finished in 64.1 seconds.
#> Chain 3 finished in 83.9 seconds.
#> Chain 1 finished in 88.6 seconds.
#> Chain 2 finished in 108.6 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 86.3 seconds.
#> Total execution time: 108.9 seconds.
#> 
#> Warning: 9 of 4000 (0.0%) transitions ended with a divergence.
#> This may indicate insufficient exploration of the posterior distribution.
#> Possible remedies include: 
#>   * Increasing adapt_delta closer to 1 (default is 0.8) 
#>   * Reparameterizing the model (e.g. using a non-centered parameterization)
#>   * Using informative or weakly informative prior distributions
#> Processing csv files: /tmp/Rtmpd6fBB3/twostrainbp-202108051137-1-5a1e73.csv, /tmp/Rtmpd6fBB3/twostrainbp-202108051137-2-5a1e73.csv, /tmp/Rtmpd6fBB3/twostrainbp-202108051137-3-5a1e73.csv, /tmp/Rtmpd6fBB3/twostrainbp-202108051137-4-5a1e73.csv
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
plot_cases(posterior, obs, max(obs$date), log = TRUE)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

Plot the posterior prediction for the fraction of cases that are DELTA.

``` r
plot_delta(posterior, obs, max(obs$date))
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

Plot the posterior estimate for the effective reproduction number of
DELTA and non-DELTA cases.

``` r
plot_rt(posterior, max(obs$date))
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

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
#> Chain 1 [inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 1 11.3774 
#> Chain 1 0.01 
#> Chain 1 1513.23 
#> Chain 1 693.187 
#> Chain 1 [78.8042,25.3216,-32.0095,-23.2391,-67.2496,-104.119,-89.7145,-48.6086,-82.8766,-43.1643,-42.0852,-2.92152,-5.26057,-4.36635,3.60785,7.85543,12.4144,7.22816,-6.33129,-20.6929,-3.46796] 
#> Chain 1 [693.187,771.991,797.312,765.303,742.064,674.814,570.695,480.98,432.372,349.495,306.331,264.246,261.324,256.064,251.697,255.305,263.161,275.575,282.803,276.472,255.779,252.311] 
#> Chain 2 [0.0001,0.0001,0.0001,inf,inf,0.0001,inf,0.0001,0.0001,0.0001,0.0001,0.0001,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,0.0001] 
#> Chain 2 11.3774 
#> Chain 2 0.01 
#> Chain 2 -3033.28 
#> Chain 2 1745.38 
#> Chain 2 [-1.49959e+08,3.31883e+08,1.03842e+08,-7.2015e+08,1.08293e+09,-2.11961e+09,1.97378e+09,-1.50786e+09,2.10754e+09,-1.16075e+09,9.19887e+08,-4.94682e+08,-1.79072e+08,4.30097e+08,-6.45768e+08,6.23639e+07,-2.07627e+08,1.21348e+08,-9.49424e+08,1.21685e+09,-1.57741e+09] 
#> Chain 2 [1745.38,-1.49957e+08,1.81926e+08,2.85767e+08,-4.34383e+08,6.48543e+08,-1.47106e+09,5.02719e+08,-1.00514e+09,1.10241e+09,-5.83473e+07,8.61539e+08,3.66858e+08,1.87786e+08,6.17883e+08,-2.78856e+07,3.44783e+07,-1.73148e+08,-5.18007e+07,-1.00122e+09,2.15627e+08,-1.36178e+09] 
#> Chain 4 [inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 4 11.3774 
#> Chain 4 0.01 
#> Chain 4 2662.96 
#> Chain 4 24803.1 
#> Chain 4 [12885,13632.3,12063.3,9437.28,7114.58,5764.18,4417.87,3658.66,1783.57,1978.63,1025.59,1596.45,998.093,537.248,1575.6,1216.73,403.206,281.088,-425.531,71.2925,-201.269] 
#> Chain 4 [24803.1,37688.1,51320.4,63383.8,72821,79935.6,85699.8,90117.7,93776.3,95559.9,97538.5,98564.1,100161,101159,101696,103271,104488,104891,105173,104747,104818,104617] 
#> Chain 4 [4.79643e+12,3.344e+84,2.3626e+156,1.66619e+228,1.19278e+300,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 4 11.3774 
#> Chain 4 0.01 
#> Chain 4 29.1989 
#> Chain 4 165.425 
#> Chain 4 [0.0132968,-0.00181691,0.0149733,0.00449149,0.00469454,-0.00351429,0.00527625,-0.00738323,0.0101935,-0.00550859,0.00492172,-0.00366612,0.00308607,-0.00650525,-0.000245367,0.00130088,0.00240502,0.00153447,-0.000518049,0.0113952,0.0051346] 
#> Chain 4 [165.425,165.439,165.437,165.452,165.456,165.461,165.458,165.463,165.455,165.466,165.46,165.465,165.461,165.464,165.458,165.458,165.459,165.461,165.463,165.462,165.474,165.479] 
#> Chain 2 finished in 35.9 seconds.
#> Chain 4 finished in 38.3 seconds.
#> Chain 3 finished in 39.0 seconds.
#> Chain 1 finished in 47.7 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 40.3 seconds.
#> Total execution time: 47.8 seconds.
#> Processing csv files: /tmp/Rtmpd6fBB3/bp-202108051139-1-83296c.csv, /tmp/Rtmpd6fBB3/bp-202108051139-2-83296c.csv, /tmp/Rtmpd6fBB3/bp-202108051139-3-83296c.csv, /tmp/Rtmpd6fBB3/bp-202108051139-4-83296c.csv
#> 
#> Checking sampler transitions treedepth.
#> Treedepth satisfactory for all transitions.
#> 
#> Checking sampler transitions for divergences.
#> 16 of 4000 (0.4%) transitions ended with a divergence.
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
#> Chain 3 [5.00966e-248,2.03358e-154,6.50746e-27,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan] 
#> Chain 3 [0.0001,1.47777e+118,2.72939e+276,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 3 [4.89945e+185,2.67501e+187,4.33842e+199,2.59918e+219,1.99614e+243,7.26684e+271,4.19424e+302,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 3 [4.89945e+185,2.67501e+187,4.33842e+199,2.59918e+219,1.99614e+243,7.26684e+271,4.19424e+302,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 3 [11.3774,4.95583] 
#> Chain 3 [0.01,0.01] 
#> Chain 3 [427.567,-87.1427] 
#> Chain 3 4 
#> Chain 3 [24.1146,17.4248,9.45867,10.7664,5.05897,1.48845,0.836179,0.605494,-1.21119,-3.22652,-0.924578,0.925253,-1.31235,-1.6406,0.565713,4.61679,0.483173,3.15565,-0.279401,-1.39427,-2.70582] 
#> Chain 3 [4,28.1146,45.5394,54.9981,65.7645,70.8305,72.3211,73.1491,73.762,72.5532,69.3137,68.3904,69.3184,68.0207,66.3788,66.9611,71.5756,72.0667,75.2176,74.9334,73.5337,70.8305] 
#> Chain 3 293.474 
#> Chain 4 [-nan,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5] 
#> Chain 4 [inf,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001] 
#> Chain 4 [0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001] 
#> Chain 4 [0.0001,0.0001,0.0001,0.0001,inf,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002] 
#> Chain 4 [11.3774,4.95583] 
#> Chain 4 [0.01,0.01] 
#> Chain 4 [-736.309,1495.08] 
#> Chain 4 -inf 
#> Chain 4 [-3865.26,-2671.24,-6343.86,-3472.33,-9122.91,-3539.91,-8738.79,-3921.82,-3918.82,-1664.56,-4440.84,-690.16,-386.698,-2819.57,910.938,-2093.68,1568.95,-2423.97,3758.84,-5632.54,4087.69] 
#> Chain 4 [-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf,-inf] 
#> Chain 4 1286.18 
#> Chain 3 finished in 88.3 seconds.
#> Chain 4 finished in 92.3 seconds.
#> Chain 2 finished in 108.3 seconds.
#> Chain 1 finished in 127.7 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 104.2 seconds.
#> Total execution time: 127.7 seconds.
#> Processing csv files: /tmp/Rtmpd6fBB3/twostrainbp-202108051140-1-4d9072.csv, /tmp/Rtmpd6fBB3/twostrainbp-202108051140-2-4d9072.csv, /tmp/Rtmpd6fBB3/twostrainbp-202108051140-3-4d9072.csv, /tmp/Rtmpd6fBB3/twostrainbp-202108051140-4-4d9072.csv
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

  - `inst/analysis/germany/forecast.R:` for a forecasting application.
  - `inst/analysis/germany/retrospective.R` for retrospective fitting
    and model evaluation
