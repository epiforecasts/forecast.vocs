% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecast.R
\name{forecast}
\alias{forecast}
\title{Forecast using branching processes at a target date}
\usage{
forecast(
  obs,
  forecast_date = max(obs$date),
  seq_date = forecast_date,
  case_date = forecast_date,
  data_list = forecast.vocs::fv_as_data_list,
  inits = forecast.vocs::fv_inits,
  fit = forecast.vocs::fv_sample,
  posterior = forecast.vocs::fv_tidy_posterior,
  extract_forecast = forecast.vocs::fv_extract_forecast,
  horizon = 4,
  r_init = c(0, 0.25),
  r_step = 1,
  r_forecast = TRUE,
  beta = c(0, 0.1),
  lkj = 0.5,
  period = NULL,
  special_periods = c(),
  voc_scale = c(0, 0.2),
  voc_label = "VOC",
  strains = 2,
  variant_relationship = "correlated",
  overdispersion = TRUE,
  models = NULL,
  likelihood = TRUE,
  output_loglik = FALSE,
  debug = FALSE,
  keep_fit = TRUE,
  scale_r = 1,
  digits = 3,
  timespan = 7,
  probs = c(0.05, 0.2, 0.8, 0.95),
  id = 0,
  ...
)
}
\arguments{
\item{obs}{A \code{data.frame} with the following variables:
\code{date}, \code{cases}, \code{seq_voc}, and \code{seq_total}, \code{cases_available},
and \code{seq_available}. \code{seq_available} and \code{case_available} must be
uniquely define data rows but other rows can be duplicated based
on data availability. This data format allows for multiple versions of
case and sequence data for a given date with different reporting dates.
This is important when using the package in evaluation settings or in
real-time where data sources are liable to be updated as new data becomes
available. See \link{germany_covid19_delta_obs} for an example of a supported
data set.}

\item{forecast_date}{Date at which to forecast. Defaults to the
maximum date in \code{obs}.}

\item{seq_date}{Date from which to use available sequence data. Defaults to
the \code{date}.}

\item{case_date}{Date from which to use available case data. Defaults to
the \code{date}.}

\item{data_list}{A function that returns a list of data as ingested by the
\code{inits} and \code{fit} function. Must use arguments as defined in
\code{\link[=fv_as_data_list]{fv_as_data_list()}}. If not supplied the package default \code{\link[=fv_as_data_list]{fv_as_data_list()}}
is used.}

\item{inits}{A function that returns a function to samples initial
conditions with the same arguments as \code{\link[=fv_inits]{fv_inits()}}. If not supplied the
package default \code{\link[=fv_inits]{fv_inits()}} is used.}

\item{fit}{A function that fits the supplied model with the same arguments
and return values as \code{\link[=fv_sample]{fv_sample()}}. If not supplied the
package default \code{\link[=fv_sample]{fv_sample()}} is used which performs MCMC sampling using
\link{cmdstanr}.}

\item{posterior}{A function that summarises the output from the supplied
fitting function with the same arguments and return values (depending on
the requirement for downstream package functionality to function) as
\code{\link[=fv_tidy_posterior]{fv_tidy_posterior()}}. If not supplied the package default
\code{\link[=fv_tidy_posterior]{fv_tidy_posterior()}} is used.}

\item{extract_forecast}{A function that extracts the forecast from
the summarised \code{posterior}. If not supplied the package default
\code{\link[=fv_extract_forecast]{fv_extract_forecast()}} is used.}

\item{horizon}{Integer forecast horizon. Defaults to 4.}

\item{r_init}{Numeric vector of length 2. Mean and
standard deviation for the normal prior on the initial log growth rate.}

\item{r_step}{Integer, defaults to 1. The number of observations between
each change in the growth rate.}

\item{r_forecast}{Logical, defaults \code{TRUE}. Should the growth rate be
forecast beyond the data horizon.}

\item{beta}{Numeric vector, defaults to c(0, 0.5). Represents the mean and
standard deviation of the normal prior (truncated at 1 and -1) on the
weighting in the differenced AR process of  the previous difference.
Placing a tight prior around zero effectively reduces the AR process to a
random walk on the growth rate.}

\item{lkj}{Numeric defaults to 0.5. The assumed prior covariance between
variants growth rates when using the "correlated" model. This sets the shape
parameter for the Lewandowski-Kurowicka-Joe (LKJ) prior distribution. If set
to 1 assigns a uniform prior for all correlations, values less than 1
indicate increased belief in strong correlations and values greater than 1
indicate increased belief weaker correlations. Our default setting places
increased weight on some correlation between strains.}

\item{period}{Logical defaults to \code{NULL}. If specified should be a function
that accepts a vector of dates. This can be used to assign periodic effects
to dates which will then be adjusted for in the case model. An example
is adjusting for day of the week effects for which the \code{\link[=fv_dow_period]{fv_dow_period()}}
can be used.}

\item{special_periods}{A vector of dates to pass to the \code{period} function
argument with the same name to be treated as "special" for example holidays
being treated as sundays in \code{\link[=fv_dow_period]{fv_dow_period()}}.}

\item{voc_scale}{Numeric vector of length 2. Prior mean and
standard deviation for the initial growth rate modifier
due to the variant of concern.}

\item{voc_label}{A character string, default to "VOC". Defines the label
to assign to variant of concern specific parameters. Example usage is to
rename parameters to use variant specific terminology.}

\item{strains}{Integer number of strains to use. Defaults to 2. Current
maximum is 2. A numeric vector can be passed if forecasts from multiple
strain models are desired.}

\item{variant_relationship}{Character string, defaulting to "correlated".
Controls the relationship of strains with options being "correlated"
(strains growth rates are correlated over time), "scaled" (a fixed scaling
between strains), and "independent" (fully independent strains after
initial scaling).}

\item{overdispersion}{Logical, defaults to \code{TRUE}. Should the observations
used include overdispersion.}

\item{models}{A model as supplied by \code{\link[=fv_model]{fv_model()}}. If not supplied the
default for that strain is used. If multiple strain models are being forecast
then \code{models} should be a list models.}

\item{likelihood}{Logical, defaults to \code{TRUE}. Should the likelihood be
included in the model}

\item{output_loglik}{Logical, defaults to \code{FALSE}. Should the
log-likelihood be output. Disabling this will speed up fitting
if evaluating the model fit is not required.}

\item{debug}{Logical, defaults to \code{FALSE}. Should within model debug
information be returned.}

\item{keep_fit}{Logical, defaults to \code{TRUE}. Should the stan model fit be
kept and returned. Dropping this can substantially reduce memory usage in
situations where multiple models are being fit.}

\item{scale_r}{Numeric, defaults to 1. Rescale the timespan over which
the growth rate and reproduction number is calculated. An example use case
is rescaling the growth rate from weekly to be scaled by the mean of
the generation time (for COVID-19 for example this would be 5.5 / 7.}

\item{digits}{Numeric, defaults to 3. Number of digits to round summary
statistics to.}

\item{timespan}{Integer, defaults to 7. Indicates the number of days between
each observation. Defaults to a week.}

\item{probs}{A vector of numeric probabilities to produce
quantile summaries for. By default these are the 5\%, 20\%, 80\%,
and 95\% quantiles which are also the minimum set required for
plotting functions to work (such as \code{\link[=plot_cases]{plot_cases()}}, \code{\link[=plot_rt]{plot_rt()}},
and \code{\link[=plot_voc_frac]{plot_voc_frac()}}).}

\item{id}{ID to assign to this forecast. Defaults to 0.}

\item{...}{Additional parameters passed to \code{\link[=fv_sample]{fv_sample()}}.}
}
\value{
A \code{data.frame} containing the output of \code{\link[=fv_sample]{fv_sample()}} in each row as
well as the summarised posterior, forecast and information about the
parameters specified.
}
\description{
Forecast using branching processes at a target date
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
options(mc.cores = 4)

forecasts <- forecast(
  germany_covid19_delta_obs,
  forecast_date = as.Date("2021-06-12"),
  horizon = 4,
  strains = c(1, 2),
  adapt_delta = 0.99,
  max_treedepth = 15,
  variant_relationship = "scaled"
)

# inspect forecasts
forecasts

# extract the model summary
summary(forecasts, type = "model")

# plot case posterior predictions
plot(forecasts, log = TRUE)

# plot voc posterior predictions
plot(forecasts, type = "voc_frac")

# extract the case forecast
summary(forecasts, type = "cases", forecast = TRUE)
\dontshow{\}) # examplesIf}
}
\seealso{
Functions used for forecasting across models, dates, and scenarios
\code{\link{forecast_across_dates}()},
\code{\link{forecast_across_scenarios}()},
\code{\link{forecast_n_strain}()},
\code{\link{plot.fv_forecast}()},
\code{\link{summary.fv_forecast}()},
\code{\link{unnest_posterior}()}
}
\concept{forecast}
