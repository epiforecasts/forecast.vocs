# Set up initial conditions for model

Set up initial conditions for model

## Usage

``` r
fv_inits(data, strains = 2)
```

## Arguments

- data:

  A list of data as produced by
  [`fv_as_data_list()`](epiforecasts.io/forecast.vocs/reference/fv_as_data_list.md).

- strains:

  Integer number of strains. Defaults to 2. Current maximum is 2.

## Value

A function that when called returns a list of initial conditions for the
package stan models.

## See also

Functions used for modelling
[`fv_as_data_list()`](epiforecasts.io/forecast.vocs/reference/fv_as_data_list.md),
[`fv_model()`](epiforecasts.io/forecast.vocs/reference/fv_model.md),
[`fv_sample()`](epiforecasts.io/forecast.vocs/reference/fv_sample.md)

## Examples

``` r
dt <- fv_as_data_list(latest_obs(germany_covid19_delta_obs))
inits <- fv_inits(dt)
inits
#> function () 
#> {
#>     inits <- list(init_cases = array(log(abs(rnorm(1, data$X[1], 
#>         data$X[1] * 0.01)))), r_init = rnorm(1, data$r_init_mean, 
#>         data$r_init_sd * 0.1), r_scale = abs(rnorm(1, 0, 0.01)), 
#>         eta = rnorm(data$eta_n, 0, 0.01), beta = rnorm(1, 0, 
#>             0.1), sqrt_phi = array(abs(rnorm(2, 0, 0.01))), period_eff = numeric(0), 
#>         period_sd = numeric(0))
#>     if (data$period > 1) {
#>         inits$period_eff <- array(rnorm(data$period, 0, 0.1))
#>         inits$period_sd <- array(abs(rnorm(1, 0, 0.1)))
#>     }
#>     if (strains == 1) {
#>         inits$sqrt_phi <- array(inits$sqrt_phi[1])
#>     }
#>     else {
#>         inits$init_voc_cases <- array(log(abs(rnorm(1, max(2, 
#>             data$X[data$t_nseq + 1] * data$Y[1]/data$N[1]), max(2, 
#>             data$X[data$t_nseq + 1] * data$Y[1]/data$N[1]) * 
#>             0.01))))
#>         inits$voc_mod <- rnorm(1, data$voc_mean, data$voc_sd * 
#>             0.1)
#>         if (data$relat == 1) {
#>             inits$voc_beta <- array(rnorm(1, 0, 0.1))
#>         }
#>         else {
#>             inits$voc_beta <- numeric(0)
#>         }
#>         if (data$relat > 0) {
#>             inits$voc_scale <- array(abs(rnorm(1, 0, 0.01)))
#>             inits$voc_eta <- array(rnorm(data$voc_eta_n, 0, 0.01))
#>         }
#>         else {
#>             inits$voc_scale <- numeric(0)
#>             inits$voc_eta <- numeric(0)
#>         }
#>         if (data$relat == 2) {
#>             inits$L_Omega <- matrix(c(1, runif(1), 0, runif(1)), 
#>                 2, 2)
#>         }
#>     }
#>     if (data$overdisp == 0) {
#>         inits$sqrt_phi <- numeric(0)
#>     }
#>     return(inits)
#> }
#> <bytecode: 0x61bfddcab498>
#> <environment: 0x61bfddcb3310>
inits()
#> $init_cases
#> [1] 11.36333
#> 
#> $r_init
#> [1] 0.006382926
#> 
#> $r_scale
#> [1] 0.02437264
#> 
#> $eta
#>  [1] -5.571287e-05  6.215527e-03  1.148412e-02 -1.821818e-02 -2.473253e-03
#>  [6] -2.441996e-03 -2.827054e-03 -5.536994e-03  6.289820e-03  2.065025e-02
#> [11] -1.630989e-02  5.124269e-03 -1.863011e-02 -5.220125e-03 -5.260191e-04
#> [16]  5.429963e-03 -9.140748e-03  4.681544e-03  3.629513e-03 -1.304544e-02
#> [21]  7.377763e-03  1.888505e-02 -9.744510e-04 -9.358474e-03 -1.595031e-04
#> [26] -8.267890e-03 -1.512400e-02  9.353632e-03
#> 
#> $beta
#> [1] 0.01764886
#> 
#> $sqrt_phi
#> [1] 0.002436855 0.016235489
#> 
#> $period_eff
#> numeric(0)
#> 
#> $period_sd
#> numeric(0)
#> 
#> $init_voc_cases
#> [1] 5.340435
#> 
#> $voc_mod
#> [1] -0.00267994
#> 
#> $voc_beta
#> numeric(0)
#> 
#> $voc_scale
#> [1] 0.01910087
#> 
#> $voc_eta
#>  [1] -0.0027923724 -0.0031344598  0.0106730788  0.0007003485 -0.0063912332
#>  [6] -0.0004996490 -0.0025148344  0.0044479712  0.0275541758  0.0004653138
#> [11]  0.0057770907  0.0011819487 -0.0191172049  0.0086208648 -0.0024323674
#> [16] -0.0020608719  0.0001917759  0.0002956075  0.0054982754 -0.0227411486
#> [21]  0.0268255718 -0.0036122126  0.0021335575  0.0107434588
#> 
#> $L_Omega
#>          [,1]      [,2]
#> [1,] 1.000000 0.0000000
#> [2,] 0.252997 0.9188032
#> 
```
