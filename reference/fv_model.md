# Load and compile a strain model

Load and compile a strain model

## Usage

``` r
fv_model(model, include, strains = 2, compile = TRUE, verbose = FALSE, ...)
```

## Arguments

- model:

  A character string indicating the path to the model. If not supplied
  the package default model is used.

- include:

  A character string specifying the path to any stan files to include in
  the model. If missing the package default is used.

- strains:

  Integer number of strains. Defaults to 2. Current maximum is 2.

- compile:

  Logical, defaults to `TRUE`. Should the model be loaded and compiled
  using
  [`cmdstanr::cmdstan_model()`](https://mc-stan.org/cmdstanr/reference/cmdstan_model.html).

- verbose:

  Logical, defaults to `TRUE`. Should verbose messages be shown.

- ...:

  Additional arguments passed to
  [`cmdstanr::cmdstan_model()`](https://mc-stan.org/cmdstanr/reference/cmdstan_model.html).

## Value

A `cmdstanr` model.

## See also

Functions used for modelling
[`fv_as_data_list()`](epiforecasts.io/forecast.vocs/reference/fv_as_data_list.md),
[`fv_inits()`](epiforecasts.io/forecast.vocs/reference/fv_inits.md),
[`fv_sample()`](epiforecasts.io/forecast.vocs/reference/fv_sample.md)

## Examples

``` r
if (FALSE) { # interactive()
# one strain model
mod <- fv_model(strains = 1)

# two strain model
two_strain_mod <- fv_model(strains = 2)
}
```
