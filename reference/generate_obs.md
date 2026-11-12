# Generate Simulated Observations

Generate simulated observations from the prior or posterior
distributions of a `forecast.vocs` model. When a single strain model is
used only case data is simulated but when a multiple strain model is
used sequence data is also simulated.

## Usage

``` r
generate_obs(
  obs,
  strains = 2,
  model = forecast.vocs::fv_model(strains = strains),
  data_list = forecast.vocs::fv_as_data_list,
  inits = forecast.vocs::fv_inits,
  fit = forecast.vocs::fv_sample,
  type = "prior",
  datasets = 10,
  ...
)
```

## Arguments

- obs:

  Observed data to use to parameterise the model and used for fitting
  when the posterior is required.

- strains:

  Integer number of strains to use. Defaults to 2. Current maximum is 2.
  A numeric vector can be passed if forecasts from multiple strain
  models are desired.

- model:

  A `cmdstanr` model object as loaded by
  [`fv_model()`](epiforecasts.io/forecast.vocs/reference/fv_model.md).

- data_list:

  A function that returns a list of data as ingested by the `inits` and
  `fit` function. Must use arguments as defined in
  [`fv_as_data_list()`](epiforecasts.io/forecast.vocs/reference/fv_as_data_list.md).
  If not supplied the package default
  [`fv_as_data_list()`](epiforecasts.io/forecast.vocs/reference/fv_as_data_list.md)
  is used.

- inits:

  A function that returns a function to samples initial conditions with
  the same arguments as
  [`fv_inits()`](epiforecasts.io/forecast.vocs/reference/fv_inits.md).
  If not supplied the package default
  [`fv_inits()`](epiforecasts.io/forecast.vocs/reference/fv_inits.md) is
  used.

- fit:

  A function that fits the supplied model with the same arguments and
  return values as
  [`fv_sample()`](epiforecasts.io/forecast.vocs/reference/fv_sample.md).
  If not supplied the package default
  [`fv_sample()`](epiforecasts.io/forecast.vocs/reference/fv_sample.md)
  is used which performs MCMC sampling using cmdstanr.

- type:

  A character string indicating the type of data to generate. Supported
  options are data based on the "prior" or data based on the "posterior"
  with the default being the prior.

- datasets:

  Numeric, defaults to 10. Number of datasets to generate.

- ...:

  Additional arguments to pass
  [`fv_as_data_list()`](epiforecasts.io/forecast.vocs/reference/fv_as_data_list.md).

## Value

A dataframe with a sampled dataset on each row with the following
variables: parameters (prior/posterior parameters used to generate the
data), obs (simulated observed data), data, (the simulated data
formatted using the supplied `data_list` function (by default
[`fv_as_data_list()`](epiforecasts.io/forecast.vocs/reference/fv_as_data_list.md))
with the same arguments as specified for simulation).

## See also

Functions to generate simulated data
[`sample_sequences()`](epiforecasts.io/forecast.vocs/reference/sample_sequences.md)

## Examples

``` r
if (FALSE) { # interactive()
options(mc.cores = 4)
obs <- latest_obs(germany_covid19_delta_obs)

sim_obs <- generate_obs(obs, voc_scale = c(0.8, 0.1), r_init = c(-0.1, 0.05))

# fit a simulated dataset
sim_dt <- sim_obs$data[[1]]
inits <- fv_inits(sim_dt)
fit <- fv_sample(
  sim_dt,
  init = inits, adapt_delta = 0.95, max_treedepth = 15
)

# summarise and plot simualated fit
posterior <- fv_tidy_posterior(fit)

plot_cases(posterior, log = TRUE)

plot_voc(posterior)

plot_rt(posterior)
}
```
