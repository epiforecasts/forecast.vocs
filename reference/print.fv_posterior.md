# Print method for fv_tidy_posterior

`print` method for class "fv_posterior". Prints the available value
types and then falls back to the `data.table` print method.

## Usage

``` r
# S3 method for class 'fv_posterior'
print(x, ...)
```

## Arguments

- x:

  An output from output from
  [`fv_tidy_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_tidy_posterior.md).

- ...:

  Pass additional arguments to `data.table` printing method.

## Value

A summary data.frame

## See also

fv_tidy_posterior

Functions used for postprocessing of model fits
[`convert_to_stanfit()`](epiforecasts.io/forecast.vocs/reference/convert_to_stanfit.md),
[`extract_draws()`](epiforecasts.io/forecast.vocs/reference/extract_draws.md),
[`extract_forecast_dates()`](epiforecasts.io/forecast.vocs/reference/extract_forecast_dates.md),
[`fv_extract_forecast()`](epiforecasts.io/forecast.vocs/reference/fv_extract_forecast.md),
[`fv_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_posterior.md),
[`fv_tidy_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_tidy_posterior.md),
[`link_dates_with_posterior()`](epiforecasts.io/forecast.vocs/reference/link_dates_with_posterior.md),
[`link_obs_with_posterior()`](epiforecasts.io/forecast.vocs/reference/link_obs_with_posterior.md),
[`plot.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/plot.fv_posterior.md),
[`quantiles_to_long()`](epiforecasts.io/forecast.vocs/reference/quantiles_to_long.md),
[`summary.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/summary.fv_posterior.md),
[`update_voc_label()`](epiforecasts.io/forecast.vocs/reference/update_voc_label.md)

## Examples

``` r
posterior <- fv_example(strains = 2, type = "posterior")

# case summary
posterior
#> Available value types: model, cases, voc_frac, voc_advantage, growth, rt, raw
#>      value_type          variable                  clean_name   date   type
#>          <char>            <char>                      <char> <IDat> <char>
#>   1:      model avg_voc_advantage          Average VOC effect   <NA>       
#>   2:      model              beta                        Beta   <NA>       
#>   3:      model     init_cases[1]               Initial cases   <NA>       
#>   4:      model init_voc_cases[1]           Initial VOC cases   <NA>       
#>   5:      model            phi[1] Notification overdispersion   <NA>       
#>  ---                                                                       
#> 469:        raw       log_lik[11]                               <NA>       
#> 470:        raw       log_lik[12]                               <NA>       
#> 471:        raw       log_lik[13]                               <NA>       
#> 472:        raw       log_lik[14]                               <NA>       
#> 473:        raw       log_lik[15]                               <NA>       
#>        obs observed forecast_start exponentiated      mean    median       sd
#>      <num>   <lgcl>         <lgcl>        <lgcl>     <num>     <num>    <num>
#>   1:    NA       NA             NA         FALSE     0.589     0.589 3.08e-02
#>   2:    NA       NA             NA         FALSE     0.230     0.260 4.08e-01
#>   3:    NA       NA             NA         FALSE 89800.000 89600.000 6.14e+03
#>   4:    NA       NA             NA         FALSE   222.000   221.000 2.27e+01
#>   5:    NA       NA             NA         FALSE    95.300    78.000 7.42e+01
#>  ---                                                                         
#> 469:    NA       NA             NA            NA   -14.700   -14.600 6.93e-01
#> 470:    NA       NA             NA            NA   -13.800   -13.800 5.79e-01
#> 471:    NA       NA             NA            NA   -13.500   -13.400 8.67e-01
#> 472:    NA       NA             NA            NA    -8.160    -8.070 5.98e-01
#> 473:    NA       NA             NA            NA    -7.730    -7.590 6.97e-01
#>           mad        q5       q20       q80       q95  rhat ess_bulk ess_tail
#>         <num>     <num>     <num>     <num>     <num> <num>    <int>    <int>
#>   1: 2.97e-02     0.541     0.564     0.613     0.639  1.00     1270     1250
#>   2: 4.53e-01    -0.466    -0.151     0.621     0.846  1.00      779      923
#>   3: 5.82e+03 79900.000 84800.000 94600.000 99900.000  1.00     1360     1340
#>   4: 2.16e+01   186.000   203.000   240.000   261.000  1.00     1610     1060
#>   5: 5.32e+01    21.200    41.200   135.000   231.000  1.00      746     1030
#>  ---                                                                         
#> 469: 6.23e-01   -16.000   -15.200   -14.100   -13.700  1.00     1300     1500
#> 470: 5.44e-01   -14.800   -14.300   -13.400   -13.000  1.01      915     1160
#> 471: 7.77e-01   -15.100   -14.100   -12.700   -12.300  1.00     1220     1490
#> 472: 4.91e-01    -9.200    -8.560    -7.690    -7.420  1.00     1070     1250
#> 473: 5.32e-01    -9.010    -8.150    -7.200    -6.880  1.00      910     1320
```
