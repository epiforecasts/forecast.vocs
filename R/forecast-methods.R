#' Summary method for forecast
#'
#' @description `summary` method for class "fv_forecast".
#'
#' @param object A `data.table` output from [forecast()] of class "fv_forecast".
#'
#' @param target A character string indicating the target object within the
#' [forecast()] to summarise. Current options are: posterior predictions
#' ("posterior"), posterior forecasts ("forecast"), the model fit ("fit"),
#' and the model diagnostics ("diagnostics"). When "posterior" or "forecast"
#' are used then [summary.fv_posterior()] is called on the nested posterior or
#' forecast.
#'
#' @inheritParams summary.fv_posterior
#' @family forecast
#' @seealso summary.fv_posterior forecast unnest_posterior
#' @return A summary `data.table`.
#' @export
#' @examplesIf interactive()
#' options(mc.cores = 4)
#'
#' forecasts <- forecast(
#'   germany_covid19_delta_obs,
#'   forecast_date = as.Date("2021-06-12"),
#'   horizon = 4,
#'   strains = c(1, 2),
#'   adapt_delta = 0.99,
#'   max_treedepth = 15,
#'   variant_relationship = "scaled"
#' )
#' # inspect forecasts
#' forecasts
#'
#' # extract the model summary
#' summary(forecasts, type = "model")
#'
#' # extract the fit object
#' summary(forecasts, target = "fit")
#'
#' # extract the case forecast
#' summary(forecasts, type = "cases", forecast = TRUE)
summary.fv_forecast <- function(object, target = "posterior", type = "model",
                                as_dt = FALSE, forecast = FALSE, ...) {
  target <- match.arg(target, c("fit", "diagnostics", "posterior", "forecast"))
  if (target == "fit") {
    out <- object$fit
    if (length(out) == 1) {
      out <- out[[1]]
    }
  } else if (target == "diagnostics") {
    out <- copy(object)[, c("posterior", "forecast") := NULL][]
  } else {
    out <- unnest_posterior(object, target = target)
    out <- summary(out, type = type, as_dt = as_dt, forecast = forecast)
  }
  return(out)
}

#' Plot method for forecast
#'
#' @description `plot` method for class "fv_forecast". The type of plot
#' produced can be controlled using the `target` and `type` arguments with the
#' latter only being functional when `target` is set to "posterior" or
#' "forecast".
#'
#' @param x A `data.table` of output as produced by [forecast()] of class
#' "fv_forecast".
#'
#' @param target A character string indicating the target object within the
#' [forecast()] to produce plots for. Current options are: posterior predictions
#' ("posterior"), posterior forecasts ("forecast"), and the model fit ("fit").
#' When "posterior" or "forecast" are used then [plot.fv_posterior()] is called
#' whereas when "fit" is used [plot_pairs()] is used.
#'
#' @param ... Pass additional arguments to lower level plot functions.
#'
#' @inheritParams plot.fv_posterior
#' @family forecast
#' @family plot
#' @seealso plot.fv_posterior
#' @return `ggplot2` object
#' @export
#' @examplesIf interactive()
#' options(mc.cores = 4)
#'
#' forecasts <- forecast(
#'   germany_covid19_delta_obs,
#'   forecast_date = as.Date("2021-06-12"),
#'   horizon = 4,
#'   strains = c(1, 2),
#'   adapt_delta = 0.99,
#'   max_treedepth = 15,
#'   variant_relationship = "scaled"
#' )
#' # inspect forecasts
#' forecasts
#'
#' # plot case posterior predictions
#' plot(forecasts, log = TRUE)
#'
#' # plot case posterior predictions with central estimates
#' plot(forecasts, log = TRUE, central = TRUE)
#'
#' # plot voc posterior predictions
#' plot(forecasts, type = "voc_frac")
plot.fv_forecast <- function(x, obs = NULL, target = "posterior",
                             type = "cases", ...) {
  target <- match.arg(target, c("fit", "posterior", "forecast"))
  if (target == "fit") {
    plot_pairs(x, ...)
  } else {
    x <- summary(x, target = target, type = "all")
    plot(x, obs = obs, type = type, ...)
  }
}
