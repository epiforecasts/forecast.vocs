
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
#> Chain 1 [7.83668e-05,1,1,1,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan] 
#> Chain 1 [0.0001,4.71931e+19,4.57106e+152,6.29803e+286,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 1 [0.0001,0.0001,0.0001,0.000100136,1.27595,2.74091e+08,1.25095e+18,1.23332e+28,1.33496e+38,2.769e+48,1.14696e+59,1.18386e+71,2.69843e+84,2.46822e+98,4.43999e+112,1.32829e+127,8.89761e+141,1.02243e+157,1.69512e+172,1.10104e+188,6.41866e+203,7.85613e+218,1.91128e+233] 
#> Chain 1 [0.0001,0.0001,0.0001,0.000100136,1.27605,4.71931e+19,4.57106e+152,6.29803e+286,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 1 [11.3774,4.95583] 
#> Chain 1 [0.01,0.01] 
#> Chain 1 [-40.4582,-254.155] 
#> Chain 1 4 
#> Chain 1 [4.44862,3.74901,3.85791,3.12982,3.06469,0.774817,0.0590716,0.639353,0.711501,3.23874,3.05996,1.35519,0.681072,0.495658,0.786235,0.545496,0.351924,1.39533,-0.141411,-1.54735,-1.56222] 
#> Chain 1 [4,8.44862,12.1976,16.0555,19.1854,22.2415,23.0117,23.105,23.7554,24.4471,27.6627,30.7575,32.147,32.8233,33.332,34.1381,34.6778,35.0444,36.4099,36.3017,34.7409,33.1253] 
#> Chain 1 280.27 
#> Chain 2 [-nan,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5] 
#> Chain 2 [inf,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001] 
#> Chain 2 [1.60821e+253,8.78051e+254,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001] 
#> Chain 2 [1.60821e+253,8.78051e+254,0.0001,0.0001,inf,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002,0.0002] 
#> Chain 2 [11.3774,4.95583] 
#> Chain 2 [0.01,0.01] 
#> Chain 2 [583.029,12605.3] 
#> Chain 2 4 
#> Chain 2 [-4.77471e+34,-4.52395e+36,-1.11055e+37,-2.27338e+37,-3.64111e+37,-5.0661e+37,-6.79532e+37,-8.27943e+37,-9.45674e+37,-1.07302e+38,-1.16156e+38,-1.22514e+38,-1.22956e+38,-1.25542e+38,-1.24984e+38,-1.24662e+38,-1.25973e+38,-1.2391e+38,-1.23101e+38,-1.2445e+38,-1.21158e+38] 
#> Chain 2 [4,-4.77471e+34,-4.5717e+36,-1.56772e+37,-3.8411e+37,-7.48221e+37,-1.25483e+38,-1.93436e+38,-2.76231e+38,-3.70798e+38,-4.781e+38,-5.94256e+38,-7.1677e+38,-8.39726e+38,-9.65268e+38,-1.09025e+39,-1.21491e+39,-1.34089e+39,-1.4648e+39,-1.5879e+39,-1.71235e+39,-1.83351e+39] 
#> Chain 2 4898.2 
#> Chain 2 [1,1,1,1,1,1,1,1,1,1,1,1,-nan,-nan,-nan,-nan,-nan,-nan,-nan] 
#> Chain 2 [4.6628e+54,1.54739e+77,5.35468e+99,1.64427e+122,5.25613e+144,1.52704e+167,4.92625e+189,1.41018e+212,4.203e+234,1.13524e+257,3.37088e+279,9.24433e+301,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 2 [1.76789e+07,9.65237e+08,5.09184e+10,2.9372e+12,1.5571e+14,8.18742e+15,4.51698e+17,2.17183e+19,1.08928e+21,4.94754e+22,2.48443e+24,1.09622e+26,5.02772e+27,2.07737e+29,9.49409e+30,4.00489e+32,1.81096e+34,7.54865e+35,3.36867e+37,1.38955e+39,5.99375e+40,2.73669e+42,1.24729e+44] 
#> Chain 2 [1.76789e+07,9.65237e+08,5.09184e+10,2.9372e+12,4.6628e+54,1.54739e+77,5.35468e+99,1.64427e+122,5.25613e+144,1.52704e+167,4.92625e+189,1.41018e+212,4.203e+234,1.13524e+257,3.37088e+279,9.24433e+301,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 2 [11.3774,4.95583] 
#> Chain 2 [0.01,0.01] 
#> Chain 2 [16.6879,125.879] 
#> Chain 2 4 
#> Chain 2 [-0.0343933,0.0893796,-0.0844459,-0.00818319,0.0426474,-0.126084,0.0421104,-0.0967662,0.103416,-0.122133,0.0358661,-0.102495,0.0936574,-0.0790023,0.0696876,-0.0881743,0.0733076,-0.0837747,0.0463564,0.0539086,0.000353248] 
#> Chain 2 [4,3.96561,4.05499,3.97054,3.96236,4.01042,3.87289,3.91512,3.81596,3.91632,3.78699,3.82569,3.72131,3.82215,3.74202,3.81151,3.73009,3.79832,3.71964,3.76432,3.8212,3.8194] 
#> Chain 2 47.8941 
#> Chain 3 [0.5,0.5,1,1,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan] 
#> Chain 3 [0.0001,0.0001,3.2325e+119,2.91175e+265,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 3 [0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001] 
#> Chain 3 [0.0001,0.0001,0.0001,0.0001,0.0002,0.0002,3.2325e+119,2.91175e+265,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 3 [11.3774,4.95583] 
#> Chain 3 [0.01,0.01] 
#> Chain 3 [-290.884,-397.496] 
#> Chain 3 4 
#> Chain 3 [-0.144656,0.109861,-0.202324,-1.26152,-1.6639,-2.27557,-3.48491,-3.8992,-4.06708,-4.04918,-3.59109,-2.85056,-2.44634,-1.79698,-1.12896,-0.727221,-0.856477,-1.09213,-0.849689,-1.09462,-0.794224] 
#> Chain 3 [4,3.85534,3.9652,3.76288,2.50136,-0.0205704,-2.68386,-5.99216,-6.1771,-11.6044,-18.887,-22.6697,-30.5129,-33.5567,-34.4946,-36.2341,-39.3984,-41.123,-42.1199,-43.777,-45.5622,-45.1839] 
#> Chain 3 334.472 
#> Chain 4 [-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan] 
#> Chain 4 [inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 4 [3.7118e+129,2.02657e+131,1.06983e+133,5.95976e+134,2.34778e+136,1.30475e+138,3.48871e+123,1.38243e+86,1.58684e+33,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001] 
#> Chain 4 [3.7118e+129,2.02657e+131,1.06983e+133,5.95976e+134,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 4 [11.3774,4.95583] 
#> Chain 4 [0.01,0.01] 
#> Chain 4 [298.345,1103.08] 
#> Chain 4 4 
#> Chain 4 [-0.0336718,0.0537829,-0.3465,0.344096,-0.430666,-0.108897,-0.1093,-0.0313436,-0.202829,-0.0884777,0.0221883,0.00889878,0.374867,-0.120972,0.207995,-0.410186,0.147104,-0.168527,-0.0123093,0.421735,-0.387013] 
#> Chain 4 [4,3.96633,4.02011,3.67361,4.01771,-33.5553,-86.1213,-121.899,-169.173,-191.217,-241.756,-271.718,-290.403,-290.684,-291.541,-307.627,-305.664,-315.196,-332.455,-345.867,-345.947,-329.178] 
#> Chain 4 445.705 
#> Chain 3 finished in 83.4 seconds.
#> Chain 2 finished in 84.0 seconds.
#> Chain 4 finished in 88.5 seconds.
#> Chain 1 finished in 89.6 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 86.4 seconds.
#> Total execution time: 89.7 seconds.
#> Processing csv files: /tmp/RtmpbPxRZk/twostrainbp-202108261352-1-2394ca.csv, /tmp/RtmpbPxRZk/twostrainbp-202108261352-2-2394ca.csv, /tmp/RtmpbPxRZk/twostrainbp-202108261352-3-2394ca.csv, /tmp/RtmpbPxRZk/twostrainbp-202108261352-4-2394ca.csv
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

posterior <- summarise_posterior(fit)
```

Plot the posterior prediction for cases.

``` r
plot_cases(posterior, obs, log = TRUE)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

Plot the posterior prediction for the fraction of cases that are DELTA.

``` r
plot_delta(posterior, obs)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

Plot the posterior estimate for the effective reproduction number of
DELTA and non-DELTA cases.

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
#> Chain 1 [inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 1 11.3774 
#> Chain 1 0.01 
#> Chain 1 1384.51 
#> Chain 1 3048.39 
#> Chain 1 [0.304433,-0.240073,-0.1623,-0.491745,-0.509264,-0.485865,-0.621492,-0.12185,-0.721034,-0.0696407,-0.432112,-0.0350624,-0.0227745,-0.047279,-0.168435,0.136513,-0.110852,-0.114279,0.111564,-0.138254,0.0867821] 
#> Chain 1 [3048.39,3048.7,3048.46,3048.29,3047.8,3047.29,3046.81,3046.19,3046.06,3045.34,3045.27,3044.84,3044.81,3044.78,3044.74,3044.57,3044.7,3044.59,3044.48,3044.59,3044.45,3044.54] 
#> Chain 3 [inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 3 11.3774 
#> Chain 3 0.01 
#> Chain 3 5726.84 
#> Chain 3 24736.9 
#> Chain 3 [8.63551e+07,1.77171e+08,2.5659e+08,2.83503e+08,2.53193e+08,2.65287e+08,2.55184e+08,2.92405e+08,2.73008e+08,2.82548e+08,2.77201e+08,2.62195e+08,2.4187e+08,2.3329e+08,1.98602e+08,2.13533e+08,2.18125e+08,2.16711e+08,2.14224e+08,1.88108e+08,2.01516e+08] 
#> Chain 3 [24736.9,8.63798e+07,2.63551e+08,5.20141e+08,8.03644e+08,1.05684e+09,1.32212e+09,1.57731e+09,1.86971e+09,2.14272e+09,2.42527e+09,2.70247e+09,2.96466e+09,3.20653e+09,3.43982e+09,3.63843e+09,3.85196e+09,4.07009e+09,4.2868e+09,4.50102e+09,4.68913e+09,4.89064e+09] 
#> Chain 3 [3.1963e+21,2.93339e+92,2.69259e+163,2.47156e+234,2.27185e+305,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 3 11.3774 
#> Chain 3 0.01 
#> Chain 3 49.5163 
#> Chain 3 163.398 
#> Chain 3 [0.000177847,2.48104e-06,0.00139885,0.000824349,-0.000212359,0.000107288,6.84211e-05,3.22349e-05,-0.000115977,-0.000528387,-0.000443189,0.000243579,-0.000300334,-0.000132192,-0.000522738,-0.00013982,0.000586989,-0.000117082,-0.000438236,0.000559903,0.000483311] 
#> Chain 3 [163.398,163.398,163.398,163.399,163.4,163.4,163.4,163.4,163.4,163.4,163.399,163.399,163.399,163.399,163.399,163.398,163.398,163.399,163.399,163.398,163.399,163.399] 
#> Chain 4 [inf,inf,inf,inf,inf,inf,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001,0.0001] 
#> Chain 4 11.3774 
#> Chain 4 0.01 
#> Chain 4 844.627 
#> Chain 4 5622.14 
#> Chain 4 [5.31941e+17,-4.48094e+17,-3.4923e+16,-4.66586e+17,-2.30629e+17,-4.99981e+17,-5.03547e+17,-4.75612e+17,-2.45552e+17,-3.94202e+17,-6.87034e+16,-2.22654e+17,-3.62231e+15,1.02644e+17,-2.52115e+17,1.25105e+16,5.36544e+16,-1.7382e+15,1.01079e+17,-2.50692e+16,4.67116e+16] 
#> Chain 4 [5622.14,5.31941e+17,8.38467e+16,4.89237e+16,-4.17663e+17,-6.48291e+17,-1.14827e+18,-1.65182e+18,-2.12743e+18,-2.37298e+18,-2.76719e+18,-2.83589e+18,-3.05854e+18,-3.06217e+18,-2.95952e+18,-3.21164e+18,-3.19913e+18,-3.14547e+18,-3.14721e+18,-3.04613e+18,-3.0712e+18,-3.02449e+18] 
#> Chain 4 [3.65485e+07,1.3496e+25,5.11372e+42,1.94675e+60,7.40283e+77,2.81217e+95,1.0555e+113,3.82582e+130,1.37196e+148,4.81467e+165,1.60149e+183,5.24363e+200,1.72351e+218,5.68942e+235,1.83619e+253,5.65688e+270,1.70277e+288,5.15929e+305,inf,inf,inf,inf,inf] 
#> Chain 4 11.3774 
#> Chain 4 0.01 
#> Chain 4 17.4142 
#> Chain 4 40.4503 
#> Chain 4 [0.0257879,0.00469344,-0.00111329,-0.00102358,-0.0120392,-0.0348781,-0.0107128,-0.0216182,-0.0535787,-0.0157739,0.00385437,0.00431307,-0.0225751,-0.0464916,-0.0232097,0.00657205,0.0080375,0.0114602,0.0145877,-0.00302912,-0.00146089] 
#> Chain 4 [40.4503,40.4761,40.4808,40.4796,40.4786,40.4666,40.4317,40.421,40.3994,40.3458,40.33,40.3339,40.3382,40.3156,40.2691,40.2459,40.2525,40.2605,40.272,40.2866,40.2835,40.2821] 
#> Chain 4 finished in 39.5 seconds.
#> Chain 1 finished in 44.0 seconds.
#> Chain 3 finished in 53.0 seconds.
#> Chain 2 finished in 56.2 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 48.2 seconds.
#> Total execution time: 56.4 seconds.
#> Processing csv files: /tmp/RtmpbPxRZk/bp-202108261354-1-3eb1b5.csv, /tmp/RtmpbPxRZk/bp-202108261354-2-3eb1b5.csv, /tmp/RtmpbPxRZk/bp-202108261354-3-3eb1b5.csv, /tmp/RtmpbPxRZk/bp-202108261354-4-3eb1b5.csv
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
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 [7.71343e-26,8.68036e-11,1,1,1,1,1,1,1,1,1,1,1,1,1,-nan,-nan,-nan,-nan] 
#> Chain 1 [0.000100001,7.61826e+12,4.57e+34,2.83032e+56,1.72253e+78,1.03258e+100,6.02357e+121,3.35656e+143,1.80815e+165,9.50509e+186,4.99282e+208,2.73887e+230,1.53001e+252,9.04308e+273,5.97757e+295,inf,inf,inf,inf] 
#> Chain 1 [9.53722e+13,5.20715e+15,3.12088e+17,1.96539e+19,1.29646e+21,8.77643e+22,5.89669e+24,4.10612e+26,2.78649e+28,1.8537e+30,1.19368e+32,7.37975e+33,4.36951e+35,2.55293e+37,1.48044e+39,9.08792e+40,5.66046e+42,3.69831e+44,2.72868e+46,2.24453e+48,2.0617e+50,2.0671e+52,2.17384e+54] 
#> Chain 1 [9.53722e+13,5.20715e+15,3.12088e+17,1.96539e+19,1.29646e+21,8.77643e+22,4.57e+34,2.83032e+56,1.72253e+78,1.03258e+100,6.02357e+121,3.35656e+143,1.80815e+165,9.50509e+186,4.99282e+208,2.73887e+230,1.53001e+252,9.04308e+273,5.97757e+295,inf,inf,inf,inf] 
#> Chain 1 [11.3774,4.95583] 
#> Chain 1 [0.01,0.01] 
#> Chain 1 [32.1888,-20.4858] 
#> Chain 1 4 
#> Chain 1 [0.0932525,0.0494961,0.0463634,0.0259083,-0.00427848,0.0342061,-0.0180077,-0.0177084,-0.0301818,-0.0399775,-0.0401194,-0.00862009,-0.00858002,0.0532893,0.0161129,0.0497634,0.119591,0.113049,0.106267,0.0888414,0.0439082] 
#> Chain 1 [4,4.09325,4.14275,4.18911,4.21502,4.20749,4.24326,4.21747,4.19757,4.16502,4.12429,4.08108,4.06776,4.06027,4.11719,4.13173,4.17954,4.30111,4.40985,4.5202,4.60779,4.65552] 
#> Chain 1 45.9324 
#> Chain 1 [1.35867e-25,6.22711e-10,1,1,1,1,1,1,1,1,1,1,1,1,1,-nan,-nan,-nan,-nan] 
#> Chain 1 [0.000100003,2.49316e+13,2.47755e+35,2.46122e+57,2.49873e+79,2.5667e+101,2.66319e+123,2.75624e+145,2.80332e+167,2.94143e+189,3.13609e+211,3.32673e+233,3.48653e+255,3.6677e+277,3.85177e+299,inf,inf,inf,inf] 
#> Chain 1 [8.19907e+13,4.47654e+15,2.45536e+17,1.34193e+19,7.36034e+20,4.00372e+22,2.16665e+24,1.1602e+26,6.19913e+27,3.29739e+29,1.75878e+31,9.40973e+32,4.95149e+34,2.62031e+36,1.38666e+38,7.3353e+39,3.90917e+41,2.09196e+43,1.12162e+45,6.00432e+46,3.21369e+48,1.72234e+50,9.17553e+51] 
#> Chain 1 [8.19907e+13,4.47654e+15,2.45536e+17,1.34193e+19,7.36034e+20,4.00372e+22,2.47755e+35,2.46122e+57,2.49873e+79,2.5667e+101,2.66319e+123,2.75624e+145,2.80332e+167,2.94143e+189,3.13609e+211,3.32673e+233,3.48653e+255,3.6677e+277,3.85177e+299,inf,inf,inf,inf] 
#> Chain 1 [11.3774,4.95583] 
#> Chain 1 [0.01,0.01] 
#> Chain 1 [32.0376,-19.7558] 
#> Chain 1 4 
#> Chain 1 [0.00459107,-0.00358416,0.00357624,-0.00829573,0.00451554,-0.00518038,-0.00231316,0.00125314,0.00305892,0.00067198,-0.00913197,0.00269585,0.00133503,-0.00281711,0.000158489,0.00303477,-0.00410455,-0.00514746,-0.00517873,0.000573635,-0.00823419] 
#> Chain 1 [4,4.00459,4.00101,4.00458,3.99629,3.99113,3.98058,3.9784,3.97389,3.97666,3.97971,3.96311,3.96877,3.96878,3.96839,3.9758,3.97995,3.98184,3.98029,3.98011,3.98143,3.97544] 
#> Chain 1 46.6067 
#> Chain 1 [0,-nan,-nan,0,0,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan] 
#> Chain 1 [0.0001,inf,inf,0.0001,0.0001,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 1 [inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,0.0001,0.0001,0.0001,0.0001,0.0001] 
#> Chain 1 [inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 1 [11.3774,4.95583] 
#> Chain 1 [0.01,0.01] 
#> Chain 1 [7326.08,-2581.03] 
#> Chain 1 4 
#> Chain 1 [1.31314e+12,6.89125e+11,3.63204e+12,1.0555e+12,-1.22544e+11,2.10438e+12,1.36764e+12,-4.34689e+12,6.65351e+12,-5.68002e+12,7.14137e+12,-4.59922e+12,3.69088e+12,2.0084e+12,-3.66795e+12,1.77568e+12,-1.55791e+12,2.38397e+12,1.66993e+12,-3.12628e+11,-2.46514e+12] 
#> Chain 1 [4,1.31314e+12,2.00226e+12,5.6343e+12,6.6898e+12,2.79015e+25,2.78845e+26,1.15049e+26,4.60163e+25,1.07951e+26,9.31352e+24,-1.07815e+25,9.15185e+25,1.29162e+25,1.08982e+26,-9.38335e+25,-3.12826e+26,-4.05686e+26,-3.56741e+26,-4.6084e+26,-5.49095e+26,-5.09181e+26] 
#> Chain 1 -105.743 
#> Chain 2 [0,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan,-nan] 
#> Chain 2 [0.0001,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 2 [inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 2 [inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf,inf] 
#> Chain 2 [11.3774,4.95583] 
#> Chain 2 [0.01,0.01] 
#> Chain 2 [3553.98,-763.208] 
#> Chain 2 4 
#> Chain 2 [1.76636e+06,3.05613e+06,3.87147e+06,4.30787e+06,4.41711e+06,4.23737e+06,4.01041e+06,3.59171e+06,3.25627e+06,2.93914e+06,2.61342e+06,2.39494e+06,2.22336e+06,2.04092e+06,1.86955e+06,1.82608e+06,1.70505e+06,1.49181e+06,1.26025e+06,1.27232e+06,1.15091e+06] 
#> Chain 2 [4,1.76637e+06,4.8225e+06,8.69396e+06,1.30018e+07,6.12167e+08,1.2015e+09,1.05258e+09,1.09864e+09,1.18808e+09,9.178e+08,1.09834e+09,9.39735e+08,1.05739e+09,9.04234e+08,1.07108e+09,1.47946e+09,1.86737e+09,2.03022e+09,2.15003e+09,2.31599e+09,2.23951e+09] 
#> Chain 2 1530.98 
#> Chain 3 finished in 83.1 seconds.
#> Chain 4 finished in 89.6 seconds.
#> Chain 1 finished in 106.3 seconds.
#> Chain 2 finished in 110.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 97.2 seconds.
#> Total execution time: 110.1 seconds.
#> Processing csv files: /tmp/RtmpbPxRZk/twostrainbp-202108261355-1-77d076.csv, /tmp/RtmpbPxRZk/twostrainbp-202108261355-2-77d076.csv, /tmp/RtmpbPxRZk/twostrainbp-202108261355-3-77d076.csv, /tmp/RtmpbPxRZk/twostrainbp-202108261355-4-77d076.csv
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
