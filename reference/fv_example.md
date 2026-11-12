# Load a package example

Loads examples of posterior and forecast summaries produced using
example scripts. Used to streamline examples, in package tests and to
enable users to explore package functionality without needing to install
`cmdstanr`.

## Usage

``` r
fv_example(strains = 1, type = "posterior")
```

## Arguments

- strains:

  Integer number of strains. Defaults to 2. Current maximum is 2.

- type:

  A character string indicating the example to load. Supported options
  are "posterior", "forecast", "observations", and "script" which are
  the output of
  [`fv_tidy_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_tidy_posterior.md),
  [`fv_extract_forecast()`](epiforecasts.io/forecast.vocs/reference/fv_extract_forecast.md),
  `filter_by_availability` (with the date argument set to "2021-08-26"
  applied to the
  [germany_covid19_delta_obs](epiforecasts.io/forecast.vocs/reference/germany_covid19_delta_obs.md)
  package dataset), and the script used to generate these examples
  respectively.

## Value

A `data.table` of summarised output

## See also

Package data sets
[`germany_covid19_delta_obs`](epiforecasts.io/forecast.vocs/reference/germany_covid19_delta_obs.md)

## Examples

``` r
# Load the summarised posterior from an example fit of the one strain model
fv_example(strains = 1, type = "posterior")
#> Available value types: model, cases, growth, rt, raw
#>      value_type      variable                  clean_name   date   type   obs
#>          <char>        <char>                      <char> <IDat> <char> <int>
#>   1:      model          beta                        Beta   <NA>           NA
#>   2:      model init_cases[1]               Initial cases   <NA>           NA
#>   3:      model        phi[1] Notification overdispersion   <NA>           NA
#>   4:      model        r_init              Initial growth   <NA>           NA
#>   5:      model       r_scale                 Growth (sd)   <NA>           NA
#>  ---                                                                         
#> 187:        raw   log_lik[11]                               <NA>           NA
#> 188:        raw   log_lik[12]                               <NA>           NA
#> 189:        raw   log_lik[13]                               <NA>           NA
#> 190:        raw   log_lik[14]                               <NA>           NA
#> 191:        raw   log_lik[15]                               <NA>           NA
#>      observed forecast_start exponentiated      mean    median       sd
#>        <lgcl>         <lgcl>        <lgcl>     <num>     <num>    <num>
#>   1:       NA             NA         FALSE     0.146     0.141 3.99e-01
#>   2:       NA             NA         FALSE 89700.000 89400.000 6.15e+03
#>   3:       NA             NA         FALSE   108.000    88.400 8.50e+01
#>   4:       NA             NA         FALSE     0.141     0.139 7.16e-02
#>   5:       NA             NA         FALSE     0.117     0.112 5.20e-02
#>  ---                                                                   
#> 187:       NA             NA            NA    -9.540    -9.470 5.69e-01
#> 188:       NA             NA            NA    -8.940    -8.900 4.33e-01
#> 189:       NA             NA            NA    -8.920    -8.800 6.72e-01
#> 190:       NA             NA            NA    -8.150    -8.050 5.99e-01
#> 191:       NA             NA            NA    -7.600    -7.500 6.33e-01
#>           mad        q5       q20       q80       q95  rhat ess_bulk ess_tail
#>         <num>     <num>     <num>     <num>     <num> <num>    <int>    <int>
#>   1: 4.40e-01 -5.09e-01 -2.10e-01     0.522  7.99e-01     1     1050      781
#>   2: 6.02e+03  7.97e+04  8.46e+04 94800.000  1.00e+05     1     1930     1340
#>   3: 5.50e+01  2.66e+01  4.91e+01   149.000  2.65e+02     1      728      920
#>   4: 6.78e-02  2.25e-02  8.41e-02     0.198  2.61e-01     1     1310     1190
#>   5: 5.04e-02  4.42e-02  7.16e-02     0.157  2.11e-01     1      930     1060
#>  ---                                                                         
#> 187: 4.97e-01 -1.06e+01 -9.92e+00    -9.080 -8.76e+00     1     1010      853
#> 188: 3.95e-01 -9.72e+00 -9.27e+00    -8.590 -8.31e+00     1      834     1140
#> 189: 5.21e-01 -1.02e+01 -9.34e+00    -8.420 -8.07e+00     1     1300     1110
#> 190: 5.16e-01 -9.26e+00 -8.57e+00    -7.670 -7.38e+00     1     1320     1320
#> 191: 4.71e-01 -8.76e+00 -7.96e+00    -7.140 -6.85e+00     1      833      993

# Load the summarised forecast from this posterior
fv_example(strains = 1, type = "forecast")
#> Available value types: cases, growth, rt
#>     value_type    type       date horizon forecast_start         mean
#>         <char>  <char>     <IDat>   <int>         <lgcl>        <num>
#>  1:      cases Overall 2021-07-03       1          FALSE 3010.0000000
#>  2:      cases Overall 2021-07-10       2          FALSE 1910.0000000
#>  3:      cases Overall 2021-07-17       3          FALSE 1320.0000000
#>  4:      cases Overall 2021-07-24       4          FALSE 1070.0000000
#>  5:     growth Overall 2021-06-26       1          FALSE   -0.4140714
#>  6:     growth Overall 2021-07-03       2          FALSE   -0.4085714
#>  7:     growth Overall 2021-07-10       3          FALSE   -0.4109286
#>  8:     growth Overall 2021-07-17       4          FALSE   -0.4085714
#>  9:         rt Overall 2021-06-26       1          FALSE    0.6609537
#> 10:         rt Overall 2021-07-03       2          FALSE    0.6645990
#> 11:         rt Overall 2021-07-10       3          FALSE    0.6630343
#> 12:         rt Overall 2021-07-17       4          FALSE    0.6645990
#>           median       sd     mad           q5          q20          q80
#>            <num>    <num>   <num>        <num>        <num>        <num>
#>  1: 2910.0000000  797.000 661.000 1900.0000000 2400.0000000 3570.0000000
#>  2: 1720.0000000  942.000 660.000  843.0000000 1250.0000000 2450.0000000
#>  3: 1030.0000000 1440.000 592.000  342.0000000  628.0000000 1700.0000000
#>  4:  616.0000000 3900.000 477.000  133.0000000  297.0000000 1230.0000000
#>  5:   -0.4140714    0.163   0.145   -0.6277857   -0.5107143   -0.3166429
#>  6:   -0.4109286    0.227   0.198   -0.6945714   -0.5390000   -0.2757857
#>  7:   -0.4148571    0.272   0.236   -0.7472143   -0.5696429   -0.2514286
#>  8:   -0.4156429    0.317   0.276   -0.7857143   -0.5971429   -0.2231429
#>  9:    0.6609537    0.163   0.145    0.5337724    0.6000668    0.7285909
#> 10:    0.6630343    0.227   0.198    0.4992884    0.5833313    0.7589756
#> 11:    0.6604346    0.272   0.236    0.4736843    0.5657274    0.7776890
#> 12:    0.6599159    0.317   0.276    0.4557940    0.5503819    0.8000006
#>               q95
#>             <num>
#>  1:  4.470000e+03
#>  2:  3.420000e+03
#>  3:  3.010000e+03
#>  4:  2.780000e+03
#>  5: -2.145000e-01
#>  6: -1.265000e-01
#>  7: -6.655000e-02
#>  8:  4.714286e-03
#>  9:  8.069448e-01
#> 10:  8.811741e-01
#> 11:  9.356161e-01
#> 12:  1.004725e+00

# Load the script used to generate these examples
# Optionally source this script to regenerate the example
readLines(fv_example(strains = 1, type = "script"))
#>  [1] "options(mc.cores = 2)"                                              
#>  [2] "obs <- filter_by_availability("                                     
#>  [3] "  germany_covid19_delta_obs,"                                       
#>  [4] "  date = \"2021-06-26\""                                            
#>  [5] ")"                                                                  
#>  [6] ""                                                                   
#>  [7] "current_obs <- filter_by_availability("                             
#>  [8] "  germany_covid19_delta_obs,"                                       
#>  [9] "  date = \"2021-08-26\""                                            
#> [10] ")"                                                                  
#> [11] ""                                                                   
#> [12] "dt <- fv_as_data_list("                                             
#> [13] "  obs,"                                                             
#> [14] "  overdispersion = TRUE,"                                           
#> [15] "  voc_scale = c(0.4, 0.2)"                                          
#> [16] ")"                                                                  
#> [17] ""                                                                   
#> [18] "inits <- fv_inits(dt, strains = 1)"                                 
#> [19] ""                                                                   
#> [20] "model <- suppressMessages(fv_model(strains = 1))"                   
#> [21] ""                                                                   
#> [22] "fit <- fv_sample("                                                  
#> [23] "  data = dt, model = model, init = inits,"                          
#> [24] "  adapt_delta = 0.99, max_treedepth = 15, chains = 2"               
#> [25] ")"                                                                  
#> [26] ""                                                                   
#> [27] "# summarise posterior assuming a mean generation time of  5.5 days."
#> [28] "posterior <- fv_tidy_posterior(fit, scale_r = 5.5 / 7)"             
#> [29] "forecast <- fv_extract_forecast(posterior)"                         
```
