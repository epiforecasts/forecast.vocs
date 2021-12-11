#' Summary method for forecast
#'
#' @description `summary` method for class "fv_forecast".
#'
#' @param object A `data.table` output from [forecast()] of class "fv_forecast".
#'
#' @param target gwgfw
#'
#' @param ... Pass additional arguments to [summary.fv_posterior()].
#'
#' @inheritParams summary.fv_posterior
#' @family forecast
#' @seealso summary.fv_posterior forecast unnest_posterior
#' @return A summary `data.table`.
#' @export
summary.fv_forecast <- function(object, target = "posterior", type = "all",
                                ...) {
  target <- match.arg(target, c("fit", "diagnostics", "posterior", "forecast"))
  if (target == "fit") {
    out <- object$fit
    if (length(out) == 1) {
      out <- out[[1]]
    }
  }else if (target == "diagnostics") {
    out <- copy(out)[, c("posterior", "forecast") := NULL][]
  }else{
    out <- unnest_posterior(object, target = target)
    out <- summary(out, type = type, as_dt = as_dt, ...)
  }
  return(out)
}

#' Plot method for forecast
#'
#' @description `plot` method for class "fv_forecast".
#'
#' @param x A `data.table` of output as produced by [forecast()] of class
#' "fv_forecast".
#'
#' @param target fwfwew
#'
#' @param ... Pass additional arguments to lower level plot functions.
#'
#' @inheritParams plot.fv_posterior
#' @family forecast
#' @family plot
#' @seealso plot.fv_posterior
#' @return `ggplot2` object
#' @export
plot.fv_forecast <- function(x, target = "posterior", type = "cases", ...) {
  target <- match.arg(target, c("posterior", "forecast"))
  target <- summary(x, target = target)
  plot(target, type = type, ...)
}