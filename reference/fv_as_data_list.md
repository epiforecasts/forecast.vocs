# Format data for use with stan

Format data for use with stan

## Usage

``` r
fv_as_data_list(
  obs,
  horizon = 4,
  r_init = c(0, 0.25),
  r_step = 1,
  r_forecast = TRUE,
  beta = c(0, 0.5),
  lkj = 0.5,
  voc_scale = c(0, 0.2),
  period = NULL,
  special_periods = c(),
  variant_relationship = "correlated",
  overdispersion = TRUE,
  likelihood = TRUE,
  output_loglik = TRUE,
  debug = FALSE
)
```

## Arguments

- obs:

  A data frame with the following variables: `date`, `cases`, `seq_voc`,
  and `seq_total`.

- horizon:

  Integer forecast horizon. Defaults to 4.

- r_init:

  Numeric vector of length 2. Mean and standard deviation for the normal
  prior on the initial log growth rate.

- r_step:

  Integer, defaults to 1. The number of observations between each change
  in the growth rate.

- r_forecast:

  Logical, defaults `TRUE`. Should the growth rate be forecast beyond
  the data horizon.

- beta:

  Numeric vector, defaults to c(0, 0.5). Represents the mean and
  standard deviation of the normal prior (truncated at 1 and -1) on the
  weighting in the differenced AR process of the previous difference.
  Placing a tight prior around zero effectively reduces the AR process
  to a random walk on the growth rate.

- lkj:

  Numeric defaults to 0.5. The assumed prior covariance between variants
  growth rates when using the "correlated" model. This sets the shape
  parameter for the Lewandowski-Kurowicka-Joe (LKJ) prior distribution.
  If set to 1 assigns a uniform prior for all correlations, values less
  than 1 indicate increased belief in strong correlations and values
  greater than 1 indicate increased belief weaker correlations. Our
  default setting places increased weight on some correlation between
  strains.

- voc_scale:

  Numeric vector of length 2. Prior mean and standard deviation for the
  initial growth rate modifier due to the variant of concern.

- period:

  Logical defaults to `NULL`. If specified should be a function that
  accepts a vector of dates. This can be used to assign periodic effects
  to dates which will then be adjusted for in the case model. An example
  is adjusting for day of the week effects for which the
  [`fv_dow_period()`](epiforecasts.io/forecast.vocs/reference/fv_dow_period.md)
  can be used.

- special_periods:

  A vector of dates to pass to the `period` function argument with the
  same name to be treated as "special" for example holidays being
  treated as sundays in
  [`fv_dow_period()`](epiforecasts.io/forecast.vocs/reference/fv_dow_period.md).

- variant_relationship:

  Character string, defaulting to "correlated". Controls the
  relationship of strains with options being "correlated" (strains
  growth rates are correlated over time), "scaled" (a fixed scaling
  between strains), and "independent" (fully independent strains after
  initial scaling).

- overdispersion:

  Logical, defaults to `TRUE`. Should the observations used include
  overdispersion.

- likelihood:

  Logical, defaults to `TRUE`. Should the likelihood be included in the
  model

- output_loglik:

  Logical, defaults to `FALSE`. Should the log-likelihood be output.
  Disabling this will speed up fitting if evaluating the model fit is
  not required.

- debug:

  Logical, defaults to `FALSE`. Should within model debug information be
  returned.

## Value

A list as required by stan.

## See also

Functions used for modelling
[`fv_inits()`](epiforecasts.io/forecast.vocs/reference/fv_inits.md),
[`fv_model()`](epiforecasts.io/forecast.vocs/reference/fv_model.md),
[`fv_sample()`](epiforecasts.io/forecast.vocs/reference/fv_sample.md)

## Examples

``` r
fv_as_data_list(latest_obs(germany_covid19_delta_obs))
#> $t
#> [1] 30
#> 
#> $t_nots
#> [1] 26
#> 
#> $t_nseq
#> [1] 4
#> 
#> $t_seq
#> [1] 14
#> 
#> $t_seqf
#> [1] 26
#> 
#> $X
#>  [1]  87328 109442 117965 107223 142664 145568 131887 107141  77261  57310
#> [11]  33052  22631  15553   7659   5033   4181   5067   7969  11122  14654
#> [21]  18474  28646  44862  62984  71057  75107
#> 
#> $N
#>  [1] 4108 4539 3667 4547 3688 3817 2781 2197 1566  991  706  803  735  564
#> 
#> $Y
#>  [1]   6  34  54  89 101 119 102 181 309 414 451 631 643 521
#> 
#> $start_date
#> [1] "2021-03-20"
#> 
#> $seq_start_date
#> [1] "2021-04-17"
#> 
#> $r_init_mean
#> [1] 0
#> 
#> $r_init_sd
#> [1] 0.25
#> 
#> $beta_mean
#> [1] 0
#> 
#> $beta_sd
#> [1] 0.5
#> 
#> $voc_mean
#> [1] 0
#> 
#> $voc_sd
#> [1] 0.2
#> 
#> $relat
#> [1] 2
#> 
#> $overdisp
#> [1] 1
#> 
#> $likelihood
#> [1] 1
#> 
#> $output_loglik
#> [1] 1
#> 
#> $debug
#> [1] 0
#> 
#> $period
#> [1] 1
#> 
#> $periodic
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#> 
#> $eta_n
#> [1] 28
#> 
#> $eta_loc
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#> 
#> $voc_eta_n
#> [1] 24
#> 
#> $voc_eta_loc
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#> 
#> $lkj_prior
#> [1] 0.5
#> 
```
