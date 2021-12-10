#' Summary method for forecast
#'
#' @description `summary` method for class "fv_forecast".
#'
#' @param object A `data.table` output from [forecast()].
#'
#' @param type A character string indicating the type of summary to return.
#' Currently supported options are "nowcast" which summaries the nowcast
#' posterior using [enw_nowcast_summary()], "nowcast_samples" which returns
#' posterior samples from the most recent nowcast, "fit" which returns the
#' summarised `cmdstanr` fit using [enw_posterior()], and
#' "posterior_prediction" which returns summarised posterior predictions for
#' observations used in fitting (using [enw_pp_summary()]).
#'
#' @param ... Pass additional arguments to summary functions.
#'
#' @family forecast
#' @seealso summary forecast unnest_posterior
#' @return A summary data.frame
#' @export
summary.fv_forecast <- function(object, type = "posterior", ...) {
  type <- match.arg(type, c("fit", "diagnostics", "posterior", "forecast"))
  if (type == "fit") {
    out <- object$fit
    if (length(out) == 1) {
      out <- out[[1]]
    }
  }else if (type == "diagnostics") {
    out <- copy(out)[, c("posterior", "forecast") := NULL][]
  }else{
    out <- unnest_posterior(object, target = type)[]
  }
  return(out)
}

#' Plot method for forecast
#'
#' @description `plot` method for class "fv_forecast".
#'
#' @param x A `data.table` of output as produced by [forecast()].
#'
#'
#' @param type A character string indicating the type of plot required.
#'
#' @param ... Pass additional arguments to lower level plot functions.
#'
#' @family forecast
#' @family plot
#' @return `ggplot2` object
#' @export
plot.fv_forecast <- function(x, target = "posterior", type, ...) {
  target <- match.arg(target, c("posterior", "forecast"))
  type <- match.arg(
    type,
    c("cases", "voc_frac", "voc_advantage", "growth", "rt")
  )
  target <- summary(x, type = target)
  plot(target, type = type, ...)
}