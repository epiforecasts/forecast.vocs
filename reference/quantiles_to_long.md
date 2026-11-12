# Convert summarised quantiles from wide to long format

Convert summarised quantiles from wide to long format

## Usage

``` r
quantiles_to_long(posterior)
```

## Arguments

- posterior:

  A dataframe as output by
  [`fv_tidy_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_tidy_posterior.md),
  [`fv_extract_forecast()`](epiforecasts.io/forecast.vocs/reference/fv_extract_forecast.md),
  etc.

## Value

A data frame of quantiles in long format.

## See also

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
[`print.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/print.fv_posterior.md),
[`summary.fv_posterior()`](epiforecasts.io/forecast.vocs/reference/summary.fv_posterior.md),
[`update_voc_label()`](epiforecasts.io/forecast.vocs/reference/update_voc_label.md)

## Examples

``` r
posterior <- fv_example(strains = 2, type = "posterior")
long_posterior <- quantiles_to_long(posterior)
long_posterior
#>       value_type          variable                  clean_name   date   type
#>           <char>            <char>                      <char> <IDat> <char>
#>    1:      model avg_voc_advantage          Average VOC effect   <NA>       
#>    2:      model              beta                        Beta   <NA>       
#>    3:      model     init_cases[1]               Initial cases   <NA>       
#>    4:      model init_voc_cases[1]           Initial VOC cases   <NA>       
#>    5:      model            phi[1] Notification overdispersion   <NA>       
#>   ---                                                                       
#> 1888:        raw       log_lik[11]                               <NA>       
#> 1889:        raw       log_lik[12]                               <NA>       
#> 1890:        raw       log_lik[13]                               <NA>       
#> 1891:        raw       log_lik[14]                               <NA>       
#> 1892:        raw       log_lik[15]                               <NA>       
#>         obs observed forecast_start exponentiated      mean    median       sd
#>       <num>   <lgcl>         <lgcl>        <lgcl>     <num>     <num>    <num>
#>    1:    NA       NA             NA         FALSE     0.589     0.589 3.08e-02
#>    2:    NA       NA             NA         FALSE     0.230     0.260 4.08e-01
#>    3:    NA       NA             NA         FALSE 89800.000 89600.000 6.14e+03
#>    4:    NA       NA             NA         FALSE   222.000   221.000 2.27e+01
#>    5:    NA       NA             NA         FALSE    95.300    78.000 7.42e+01
#>   ---                                                                         
#> 1888:    NA       NA             NA            NA   -14.700   -14.600 6.93e-01
#> 1889:    NA       NA             NA            NA   -13.800   -13.800 5.79e-01
#> 1890:    NA       NA             NA            NA   -13.500   -13.400 8.67e-01
#> 1891:    NA       NA             NA            NA    -8.160    -8.070 5.98e-01
#> 1892:    NA       NA             NA            NA    -7.730    -7.590 6.97e-01
#>            mad  rhat ess_bulk ess_tail quantile prediction
#>          <num> <num>    <int>    <int>    <num>      <num>
#>    1: 2.97e-02  1.00     1270     1250     0.05      0.541
#>    2: 4.53e-01  1.00      779      923     0.05     -0.466
#>    3: 5.82e+03  1.00     1360     1340     0.05  79900.000
#>    4: 2.16e+01  1.00     1610     1060     0.05    186.000
#>    5: 5.32e+01  1.00      746     1030     0.05     21.200
#>   ---                                                     
#> 1888: 6.23e-01  1.00     1300     1500     0.95    -13.700
#> 1889: 5.44e-01  1.01      915     1160     0.95    -13.000
#> 1890: 7.77e-01  1.00     1220     1490     0.95    -12.300
#> 1891: 4.91e-01  1.00     1070     1250     0.95     -7.420
#> 1892: 5.32e-01  1.00      910     1320     0.95     -6.880
```
