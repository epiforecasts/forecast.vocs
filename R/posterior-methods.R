#' Print method for fv_tidy_posterior
#'
#' @description `print` method for class "fv_posterior". Prints the available
#' value types and then falls back to the `data.table` print method.
#'
#' @param x An output from output from [fv_tidy_posterior()].
#'
#' @param ... Pass additional arguments to `data.table` printing method.
#'
#' @family postprocess
#' @seealso fv_tidy_posterior
#' @return A summary data.frame
#' @export
#' @examples
#' posterior <- fv_example(strains = 2, type = "posterior")
#'
#' # case summary
#' posterior
print.fv_posterior <- function(x, ...) {
  if (!is.null(x$value_type)) {
    cat(
      paste0(
        "Available value types: ",
        paste(unique(x$value_type), sep = ", ", collapse = ", "), 
        "\n"
      )
    )
  }
  class(x) <- class(x)[-1]
  print(x, ...)
}

#' Summary method for fv_tidy_posterior
#'
#' @description `summary` method for class "fv_tidy_posterior". Can be used to
#' filter the posterior for variables of interest, to return forecasts only, and
#' to summarise using the `data.table` method
#'
#' @param object An object of the class `fv_posterior` as returned by
#' `fv_tidy_posterior()` .
#'
#' @param type A character string used to filter the summarised output and
#' defaulting to "model". Current options are: "model" which returns a
#' summary of key model parameters, "cases" which returns summarised cases,
#' "voc_frac" which returns summarised estimates of the fraction of cases that
#' have the variant of concern, "voc_advantage" that returns summarised
#' estimates of the the transmission advantage of the variant of concern,
#' "growth" which returns summarised variant specific and overall growth rates,
#' "rt" which returns summarised variant specific and overall reproduction
#' number estimates, "raw" which returns a raw posterior summary, and "all"
#' which returns all tidied posterior estimates.
#'
#' @param as_dt Logical defaults to `FALSE`. Once any filtering has been applied
#' should [summary()] fall back to using the default `data.table` method.
#'
#' @param forecast Logical defaults to `FALSE`. Should [fv_extract_forecast()]
#' be used to return only forecasts rather than complete posterior.
#'
#' @param ... Additional arguments passed to [summary()] when `as_dt = TRUE`.
#'
#' @family postprocess
#' @seealso fv_tidy_posterior
#' @return A summary data.table table unless type "all" is used in which case
#' the output is still of type "fv_posterior"
#' @export
#' @examples
#' posterior <- fv_example(strains = 2, type = "posterior")
#'
#' # case summary
#' summary(posterior, type = "cases")
#'
#' # summary of the case summary
#' summary(posterior, type = "cases", as_dt = TRUE)
#'
#' # case forecast only
#' summary(posterior, type = "cases", forecast = TRUE)
#'
#' # voc fraction summary
#' summary(posterior, type = "voc_frac")
#'
#' # voc advantage summary
#' summary(posterior, type = "voc_advantage")
#'
#' # growth summary
#' summary(posterior, type = "growth")
#'
#' # Rt summary
#' summary(posterior, type = "rt")
#'
#' # model parameter summary
#' summary(posterior, type = "model")
#'
#' # raw posterior values
#' summary(posterior, type = "raw")
summary.fv_posterior <- function(object, type = "model", forecast = FALSE,
                                 as_dt = FALSE, ...) {
  type <- match.arg(
    type,
    c("model", "cases", "voc_frac", "voc_advantage", "growth", "rt", "raw",
      "all")
  )
  etype <- type
  out <- object
  if (forecast) {
    out <- fv_extract_forecast(out)
  }
  if (type != "all") {
    out <- out[value_type == etype]
    out[, value_type := NULL]
  }
  if (!(type %in% c("model", "all"))) {
    suppressWarnings(
      out[, c("clean_name", "exponentiated") := NULL]
    )
  }
  if (!(type %in% c("cases", "voc_frac"))) {
    suppressWarnings(
      out[, c("obs") := NULL]
    )
  }
  if (type %in% c("model", "raw")) {
    suppressWarnings(
      out[, c("observed", "type", "forecast_start", "date") := NULL]
    )
  }
  if (as_dt) {
    class(out) <- class(out)[-1]
    return(summary(out, ...))
  }else {
      if (!(type == "all")) {
        class(out) <- class(out)[-1]
      }
      return(out[])
  }
}

#' Plot method for fv_tidy_posterior
#'
#' @description `plot` method for class "fv_posterior". This function wraps all
#' lower level plot functions.
#'
#' @param x A `data.table` of output as produced by [fv_tidy_posterior()].
#'
#' @param type A character string indicating the type of plot required,
#' defaulting to "cases". Current options are: "cases" which calls
#' [plot_cases()], "voc_frac" which calls [plot_voc_frac()], "voc_advantage"
#' which calls [plot_voc_advantage()], "growth" which calls [plot_growth()],
#' "rt" which calls [plot_rt()], and "all" which produces a list of all plots
#' by call [plot_posterior()].
#'
#' @param ... Pass additional arguments to lower level plot functions.
#'
#' @family postprocess
#' @family plot
#' @inheritParams plot_posterior
#' @return `ggplot2` object
#' @export
#' @examples
#' posterior <- fv_example(strains = 2, type = "posterior")
#'
#' # plot cases on the log scale
#' plot(posterior, type = "cases", log = TRUE)
#'
#' # plot fraction that have the variant of concern
#' plot(posterior, type = "voc_frac")
#'
#' # plot the transmission advantage for the the variant of concern
#' plot(posterior, type = "voc_advantage")
#'
#' # plot the growth rates for both voc and non-voc cases
#' plot(posterior, type = "growth")
#'
#' # plot the reproduction number estimates
#' plot(posterior, type = "rt")
plot.fv_posterior <- function(x, obs = NULL, type = "cases",
                              forecast_dates = NULL, all_obs = FALSE,
                              voc_label = "variant of concern", ...) {
  type <- match.arg(
    type,
    c("cases", "voc_frac", "voc_advantage", "growth", "rt", "all")
  )
  if (type == "cases") {
    plot_cases(x, obs, forecast_dates, all_obs = all_obs, ...)
  } else if (type == "voc_frac") {
    plot_voc_frac(
      x, obs, forecast_dates, voc_label = voc_label, all_obs = all_obs, ...
    )
  } else if (type == "voc_advantage") {
    plot_voc_advantage(x, forecast_dates, voc_label, ...)
  } else if (type == "growth") {
    plot_growth(x, forecast_dates, ...)
  } else if (type == "rt") {
    plot_rt(x, forecast_dates, ...)
  } else if (type == "all") {
    plot_posterior(x, obs = obs, forecast_dates = forecast_dates,
                   all_obs = all_obs, voc_label = voc_label, ...)
  }
}
