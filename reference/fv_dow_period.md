# Calculate the day of the week periodicity

This helper function allows the user to generate a vector of day of the
the week periods.

## Usage

``` r
fv_dow_period(t, start_date, specials = c(), special_to = "Sunday")
```

## Arguments

- t:

  An integer indicating the number of dates

- start_date:

  A date indicating the start date

- specials:

  A vector of special dates to modify the day of the week for.

- special_to:

  A character string indicating which day of the week or other label to
  assign holidays. By default this is set to "Sunday"

## Value

A vector indicating the period of the dates.

## See also

Preprocessing functions
[`filter_by_availability()`](epiforecasts.io/forecast.vocs/reference/filter_by_availability.md),
[`latest_obs()`](epiforecasts.io/forecast.vocs/reference/latest_obs.md),
[`piecewise_steps()`](epiforecasts.io/forecast.vocs/reference/piecewise_steps.md)

## Examples

``` r
fv_dow_period(t = 10, start_date = as.Date("2021-12-01"))
#>  [1] 7 5 1 3 4 2 6 7 5 1
```
