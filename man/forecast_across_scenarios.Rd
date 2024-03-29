% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecast-tools.R
\name{forecast_across_scenarios}
\alias{forecast_across_scenarios}
\title{Forecast across multiple scenarios and dates}
\usage{
forecast_across_scenarios(obs, scenarios, ...)
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

\item{scenarios}{A \code{data.frame} of scenarios as produced by
\code{\link[=define_scenarios]{define_scenarios()}}. If an \code{obs} variable is present this is
used as the scenario data but otherwise \code{\link[=generate_obs_scenario]{generate_obs_scenario()}}
is used to generate this data from the other variables in \code{scenarios}.}

\item{...}{Additional parameters passed to \code{\link[=forecast_across_dates]{forecast_across_dates()}}.}
}
\value{
A data table each rows containing the output from running
\code{\link[=forecast]{forecast()}} on a single scenario for a single forecast date.
}
\description{
Forecast across multiple scenarios and dates
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(ggplot2)
options(mc.cores = 4)

scenarios <- define_scenarios(
  voc_scale = list(c(0, 0.5), c(0.5, 0.25)),
  seq_lag = 1,
  seq_samples = 1
)
scenarios

forecasts <- forecast_across_scenarios(
  germany_covid19_delta_obs,
  scenarios,
  forecast_dates = c(as.Date("2021-05-01"), as.Date("2021-06-12")),
  horizon = 4,
  strains = 2,
  adapt_delta = 0.99,
  max_treedepth = 15,
  variant_relationship = "scaled"
)

# inspect forecasts
forecasts

# unnest posteriors
posteriors <- unnest_posterior(forecasts)

# plot case posterior predictions
plot_cases(posteriors, log = TRUE) +
  facet_grid(vars(forecast_date))
\dontshow{\}) # examplesIf}
}
\seealso{
Functions used for forecasting across models, dates, and scenarios
\code{\link{forecast_across_dates}()},
\code{\link{forecast_n_strain}()},
\code{\link{forecast}()},
\code{\link{plot.fv_forecast}()},
\code{\link{summary.fv_forecast}()},
\code{\link{unnest_posterior}()}
}
\concept{forecast}
