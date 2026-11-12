# Fit a brancing process strain model

Fit a brancing process strain model

## Usage

``` r
fv_sample(
  data,
  model = forecast.vocs::fv_model(strains = 2),
  diagnostics = TRUE,
  ...
)
```

## Arguments

- data:

  A list of data as produced by
  [`fv_as_data_list()`](epiforecasts.io/forecast.vocs/reference/fv_as_data_list.md).

- model:

  A `cmdstanr` model object as loaded by
  [`fv_model()`](epiforecasts.io/forecast.vocs/reference/fv_model.md).

- diagnostics:

  Logical, defaults to `TRUE`. Should fitting diagnostics be returned as
  a `data.frame`.

- ...:

  Additional parameters passed to the `sample` method of `cmdstanr`.

## Value

A `data.frame` containing the `cmdstanr` fit, the input data, the
fitting arguments, and optionally summary diagnostics.

## See also

Functions used for modelling
[`fv_as_data_list()`](epiforecasts.io/forecast.vocs/reference/fv_as_data_list.md),
[`fv_inits()`](epiforecasts.io/forecast.vocs/reference/fv_inits.md),
[`fv_model()`](epiforecasts.io/forecast.vocs/reference/fv_model.md)

## Examples

``` r
if (FALSE) { # interactive()
options(mc.cores = 4)

# format example data
obs <- filter_by_availability(
  germany_covid19_delta_obs,
  date = as.Date("2021-06-12"),
)
dt <- fv_as_data_list(obs)

# single strain model
inits <- fv_inits(dt, strains = 1)
mod <- fv_model(strains = 1)
fit <- fv_sample(
  dt,
  model = mod, init = inits,
  adapt_delta = 0.99, max_treedepth = 15
)
fit

# two strain model
inits <- fv_inits(dt, strains = 2)

mod <- fv_model(strains = 2)

two_strain_fit <- fv_sample(dt,
  model = mod, init = inits,
  adapt_delta = 0.99, max_treedepth = 15
)
two_strain_fit
}
```
