% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecast-methods.R
\name{plot.fv_forecast}
\alias{plot.fv_forecast}
\title{Plot method for forecast}
\usage{
\method{plot}{fv_forecast}(x, obs = NULL, target = "posterior", type = "cases", ...)
}
\arguments{
\item{x}{A \code{data.table} of output as produced by \code{\link[=forecast]{forecast()}} of class
"fv_forecast".}

\item{obs}{A data frame of observed data as produced by \code{\link[=latest_obs]{latest_obs()}}.}

\item{target}{A character string indicating the target object within the
\code{\link[=forecast]{forecast()}} to produce plots for. Current options are: posterior predictions
("posterior"), posterior forecasts ("forecast"), and the model fit ("fit").
When "posterior" or "forecast" are used then \code{\link[=plot.fv_posterior]{plot.fv_posterior()}} is called
whereas when "fit" is used \code{\link[=plot_pairs]{plot_pairs()}} is used.}

\item{type}{A character string indicating the type of plot required,
defaulting to "cases". Current options are: "cases" which calls
\code{\link[=plot_cases]{plot_cases()}}, "voc_frac" which calls \code{\link[=plot_voc_frac]{plot_voc_frac()}}, "voc_advantage"
which calls \code{\link[=plot_voc_advantage]{plot_voc_advantage()}}, "growth" which calls \code{\link[=plot_growth]{plot_growth()}},
"rt" which calls \code{\link[=plot_rt]{plot_rt()}}, and "all" which produces a list of all plots
by call \code{\link[=plot_posterior]{plot_posterior()}}.}

\item{...}{Pass additional arguments to lower level plot functions.}
}
\value{
\code{ggplot2} object
}
\description{
\code{plot} method for class "fv_forecast". The type of plot
produced can be controlled using the \code{target} and \code{type} arguments with the
latter only being functional when \code{target} is set to "posterior" or
"forecast".
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

# plot case posterior predictions
plot(forecasts, log = TRUE)

# plot case posterior predictions with central estimates
plot(forecasts, log = TRUE, central = TRUE)

# plot voc posterior predictions
plot(forecasts, type = "voc_frac")
\dontshow{\}) # examplesIf}
}
\seealso{
plot.fv_posterior

Functions used for forecasting across models, dates, and scenarios
\code{\link{forecast_across_dates}()},
\code{\link{forecast_across_scenarios}()},
\code{\link{forecast_n_strain}()},
\code{\link{forecast}()},
\code{\link{summary.fv_forecast}()},
\code{\link{unnest_posterior}()}

Plotting functions
\code{\link{add_forecast_dates}()},
\code{\link{plot.fv_posterior}()},
\code{\link{plot_cases}()},
\code{\link{plot_default}()},
\code{\link{plot_growth}()},
\code{\link{plot_pairs}()},
\code{\link{plot_posterior}()},
\code{\link{plot_rt}()},
\code{\link{plot_theme}()},
\code{\link{plot_voc_advantage}()},
\code{\link{plot_voc_frac}()},
\code{\link{save_plots}()}
}
\concept{forecast}
\concept{plot}
