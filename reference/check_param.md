# Check a parameter is the correct type and length

Check a parameter is the correct type and length

## Usage

``` r
check_param(param, name = "param", type = "numeric", length)
```

## Arguments

- param:

  A parameter to check the format of.

- name:

  A character string naming the variable to check.

- type:

  A character string identifying the allowed parameter type (must be a
  type with a is.type function except for a Date).

- length:

  Numeric, allowed length of the variable. Defaults to any allowed
  length.

## See also

Functions used for checking inputs
[`check_dataframe()`](epiforecasts.io/forecast.vocs/reference/check_dataframe.md),
[`check_observations()`](epiforecasts.io/forecast.vocs/reference/check_observations.md),
[`check_quantiles()`](epiforecasts.io/forecast.vocs/reference/check_quantiles.md)
