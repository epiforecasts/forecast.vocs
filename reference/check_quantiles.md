# Check Quantiles Required are Present

Check Quantiles Required are Present

## Usage

``` r
check_quantiles(posterior, req_probs = c(0.5, 0.95, 0.2, 0.8))
```

## Arguments

- posterior:

  A dataframe containing quantiles identified using the `q5` naming
  scheme.

- req_probs:

  A numeric vector of required probabilities.

## See also

Functions used for checking inputs
[`check_dataframe()`](epiforecasts.io/forecast.vocs/reference/check_dataframe.md),
[`check_observations()`](epiforecasts.io/forecast.vocs/reference/check_observations.md),
[`check_param()`](epiforecasts.io/forecast.vocs/reference/check_param.md)
