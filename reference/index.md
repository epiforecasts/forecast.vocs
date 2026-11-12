# Package index

## Forecast

Functions for forecasting across models, dates, and scenarios.

- [`forecast()`](epiforecasts.io/forecast.vocs/reference/forecast.md) :
  Forecast using branching processes at a target date
- [`forecast_across_dates()`](epiforecasts.io/forecast.vocs/reference/forecast_across_dates.md)
  : Forecast across multiple dates
- [`forecast_across_scenarios()`](epiforecasts.io/forecast.vocs/reference/forecast_across_scenarios.md)
  : Forecast across multiple scenarios and dates
- [`forecast_n_strain()`](epiforecasts.io/forecast.vocs/reference/forecast_n_strain.md)
  : Forecast for a single model and summarise
- [`plot(`*`<fv_forecast>`*`)`](epiforecasts.io/forecast.vocs/reference/plot.fv_forecast.md)
  : Plot method for forecast
- [`summary(`*`<fv_forecast>`*`)`](epiforecasts.io/forecast.vocs/reference/summary.fv_forecast.md)
  : Summary method for forecast
- [`unnest_posterior()`](epiforecasts.io/forecast.vocs/reference/unnest_posterior.md)
  : Unnest posterior estimates from a forecast data.frame

## Model

Functions for fitting the forecasting models.

- [`fv_as_data_list()`](epiforecasts.io/forecast.vocs/reference/fv_as_data_list.md)
  : Format data for use with stan
- [`fv_inits()`](epiforecasts.io/forecast.vocs/reference/fv_inits.md) :
  Set up initial conditions for model
- [`fv_model()`](epiforecasts.io/forecast.vocs/reference/fv_model.md) :
  Load and compile a strain model
- [`fv_sample()`](epiforecasts.io/forecast.vocs/reference/fv_sample.md)
  : Fit a brancing process strain model

## Preprocess

Functions for preprocessing observations.

- [`filter_by_availability()`](epiforecasts.io/forecast.vocs/reference/filter_by_availability.md)
  : Filter data based on availability and forecast date
- [`fv_dow_period()`](epiforecasts.io/forecast.vocs/reference/fv_dow_period.md)
  : Calculate the day of the week periodicity
- [`latest_obs()`](epiforecasts.io/forecast.vocs/reference/latest_obs.md)
  : Filter for latest observations of all types
- [`piecewise_steps()`](epiforecasts.io/forecast.vocs/reference/piecewise_steps.md)
  : Calculate piecewise steps

## Postprocess

Functions for postprocessing model output.

- [`convert_to_stanfit()`](epiforecasts.io/forecast.vocs/reference/convert_to_stanfit.md)
  : Convert to stanfit object
- [`extract_draws()`](epiforecasts.io/forecast.vocs/reference/extract_draws.md)
  : Extract posterior draws
- [`extract_forecast_dates()`](epiforecasts.io/forecast.vocs/reference/extract_forecast_dates.md)
  : Extract forecast dates
- [`fv_extract_forecast()`](epiforecasts.io/forecast.vocs/reference/fv_extract_forecast.md)
  : Extract forecasts from a summarised posterior
- [`fv_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_posterior.md)
  : Summarise the posterior
- [`fv_tidy_posterior()`](epiforecasts.io/forecast.vocs/reference/fv_tidy_posterior.md)
  : Summarise the posterior tidily
- [`link_dates_with_posterior()`](epiforecasts.io/forecast.vocs/reference/link_dates_with_posterior.md)
  : Link dates by time for posterior parameter estimates
- [`link_obs_with_posterior()`](epiforecasts.io/forecast.vocs/reference/link_obs_with_posterior.md)
  : Link posterior estimates with observed data
- [`plot(`*`<fv_posterior>`*`)`](epiforecasts.io/forecast.vocs/reference/plot.fv_posterior.md)
  : Plot method for fv_tidy_posterior
- [`print(`*`<fv_posterior>`*`)`](epiforecasts.io/forecast.vocs/reference/print.fv_posterior.md)
  : Print method for fv_tidy_posterior
- [`quantiles_to_long()`](epiforecasts.io/forecast.vocs/reference/quantiles_to_long.md)
  : Convert summarised quantiles from wide to long format
- [`summary(`*`<fv_posterior>`*`)`](epiforecasts.io/forecast.vocs/reference/summary.fv_posterior.md)
  : Summary method for fv_tidy_posterior
- [`update_voc_label()`](epiforecasts.io/forecast.vocs/reference/update_voc_label.md)
  : Label the Variant of Concern

## Plot

Functions for plotting postprocessed forecast model output.

- [`add_forecast_dates()`](epiforecasts.io/forecast.vocs/reference/add_forecast_dates.md)
  : Add the forecast dates to a plot
- [`plot(`*`<fv_forecast>`*`)`](epiforecasts.io/forecast.vocs/reference/plot.fv_forecast.md)
  : Plot method for forecast
- [`plot(`*`<fv_posterior>`*`)`](epiforecasts.io/forecast.vocs/reference/plot.fv_posterior.md)
  : Plot method for fv_tidy_posterior
- [`plot_cases()`](epiforecasts.io/forecast.vocs/reference/plot_cases.md)
  : Plot the posterior prediction for cases
- [`plot_default()`](epiforecasts.io/forecast.vocs/reference/plot_default.md)
  : Default posterior plot
- [`plot_growth()`](epiforecasts.io/forecast.vocs/reference/plot_growth.md)
  : Plot the posterior prediction for the growth rate
- [`plot_pairs()`](epiforecasts.io/forecast.vocs/reference/plot_pairs.md)
  : Pairs plot of parameters of interest and fitting diagnostics
- [`plot_posterior()`](epiforecasts.io/forecast.vocs/reference/plot_posterior.md)
  : Plot posterior predictions
- [`plot_rt()`](epiforecasts.io/forecast.vocs/reference/plot_rt.md) :
  Plot the posterior prediction for the reproduction number
- [`plot_theme()`](epiforecasts.io/forecast.vocs/reference/plot_theme.md)
  : Add the default plot theme
- [`plot_voc_advantage()`](epiforecasts.io/forecast.vocs/reference/plot_voc_advantage.md)
  : Plot the posterior prediction for the transmission advantage for the
  variant of concern
- [`plot_voc_frac()`](epiforecasts.io/forecast.vocs/reference/plot_voc_frac.md)
  : Plot the population posterior prediction for the fraction of samples
  with the variant of concern
- [`save_plots()`](epiforecasts.io/forecast.vocs/reference/save_plots.md)
  : Save plots by name

## Model validation

Functions for validating model fits and forecasts.

- [`bp_launch_shinystan()`](epiforecasts.io/forecast.vocs/reference/bp_launch_shinystan.md)
  : Launch shinystan
- [`fv_score_forecast()`](epiforecasts.io/forecast.vocs/reference/fv_score_forecast.md)
  : Evaluate forecasts using proper scoring rules
- [`plot_pairs()`](epiforecasts.io/forecast.vocs/reference/plot_pairs.md)
  : Pairs plot of parameters of interest and fitting diagnostics

## Generate data

Functions for generating simulated data.

- [`generate_obs()`](epiforecasts.io/forecast.vocs/reference/generate_obs.md)
  : Generate Simulated Observations
- [`sample_sequences()`](epiforecasts.io/forecast.vocs/reference/sample_sequences.md)
  : Sample Sequence Observation Model

## Define and generate scenarios

Functions for defining and generating data scenarios.

- [`define_scenarios()`](epiforecasts.io/forecast.vocs/reference/define_scenarios.md)
  : Define data availability scenarios
- [`generate_obs_scenario()`](epiforecasts.io/forecast.vocs/reference/generate_obs_scenario.md)
  : Define observed data for a scenario
- [`update_obs_availability()`](epiforecasts.io/forecast.vocs/reference/update_obs_availability.md)
  : Update observations based on availability

## Datasets

Package datasets used in examples and by users to explore the package
functionality.

- [`fv_example()`](epiforecasts.io/forecast.vocs/reference/fv_example.md)
  : Load a package example
- [`germany_covid19_delta_obs`](epiforecasts.io/forecast.vocs/reference/germany_covid19_delta_obs.md)
  : Test positive COVID-19 cases and sequences in Germany

## Check inputs

Functions to check the structure of user inputs.

- [`check_dataframe()`](epiforecasts.io/forecast.vocs/reference/check_dataframe.md)
  : Check a data.frame
- [`check_observations()`](epiforecasts.io/forecast.vocs/reference/check_observations.md)
  : Check observations are in the correct format
- [`check_param()`](epiforecasts.io/forecast.vocs/reference/check_param.md)
  : Check a parameter is the correct type and length
- [`check_quantiles()`](epiforecasts.io/forecast.vocs/reference/check_quantiles.md)
  : Check Quantiles Required are Present
