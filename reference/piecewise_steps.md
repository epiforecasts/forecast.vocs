# Calculate piecewise steps

This helper function streamlines the calculation of piecewise steps.
This may be useful when specifying random walks, AR processes, etc.

## Usage

``` r
piecewise_steps(t, step, offset = 0, steps_post_offset = TRUE)
```

## Arguments

- t:

  Integer, the timespan over which to calculate steps

- step:

  Integer, the frequency at which to step.

- offset:

  Integer, the amount to offset steps. This can be used to index steps
  from this index.

- steps_post_offset:

  Logical, defaults to `TRUE`. Should steps be added after the offset.

## Value

A list containing two elements: `n` (the number of steps) and `steps`
the location of steps as a binary variable.

## See also

Preprocessing functions
[`filter_by_availability()`](epiforecasts.io/forecast.vocs/reference/filter_by_availability.md),
[`fv_dow_period()`](epiforecasts.io/forecast.vocs/reference/fv_dow_period.md),
[`latest_obs()`](epiforecasts.io/forecast.vocs/reference/latest_obs.md)
