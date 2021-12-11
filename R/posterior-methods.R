#' Summary method for fv_tidy_posterior
#'
#' @description `summary` method for class "fv_tidy_posterior".
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
#' filter the posterior for variables of interest, to return forecats only, and
#' to summarise using the `data.table` method
#'
#' @param object An object of the class `fv_posterior` as returned by
#' `fv_tidy_posterior()` .
#'
#' @param type A character string used to filter the summarised output.
#'
#' @param as_dt Logical defaults to `FALSE`. Should the output
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
summary.fv_posterior <- function(x, type = "model", forecast = FALSE,
                                 as_dt = FALSE, ...) {
  type <- match.arg(
    type,
    c("model", "cases", "voc_frac", "voc_advantage", "growth", "rt", "raw",
      "all")
  )
  etype <- type
  out <- x
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
#' @family postprocess
#' @family plot
#' @return `ggplot2` object
#' @export
#' @examples
#' posterior <- fv_example(strains = 2, type = "posterior")
#'
plot.fv_posterior <- function(x, type = "cases", obs = NULL, 
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
    plot_growth(x, forecast_date, ...)
  } else if (type == "rt") {
    plot_rt(x, forecast_dates, ...)
  } else if (type == "all") {
    plot_posterior(x, obs = obs, forecast_dates = forecast_dates,
                   all_obs = all_obs, voc_label = voc_label, ...)
  }
}


#' Plot posterior predictions
#'
#' @param save_path A character string indicating where to save plots
#' if required.
#'
#' @param type A character string indicating the format to use to save plots.
#'
#' @return A named list of all supported package plots with sensible defaults.
#'
#' @family plot
#' @export
#' @inheritParams plot_cases
#' @inheritParams plot_voc_frac
#' @importFrom purrr walk2
#' @examples
#' posterior <- fv_example(strains = 2, type = "posterior")
#' plot_posterior(posterior)
plot_posterior <- function(posterior, obs = NULL, forecast_dates = NULL,
                           save_path = NULL, type = "png",
                           all_obs = FALSE, voc_label = "variant of concern") {
  plots <- list()
  plots$cases <- plot_cases(
    posterior, obs, forecast_dates,
    log = FALSE, all_obs = all_obs
  )
  plots$log_cases <- plot_cases(
    posterior, obs, forecast_dates,
    log = TRUE, all_obs = all_obs
  )
  if (nrow(posterior[value_type %in% "voc_frac"]) > 0) {
    plots$voc_frac <- plot_voc_frac(
      posterior, obs, forecast_dates,
      voc_label = voc_label, all_obs = all_obs
    )
    plots$voc_advantage <- plot_voc_advantage(
      posterior, forecast_dates, voc_label
    )
  }
  plots$growth <- plot_growth(posterior, forecast_dates)
  plots$rt <- plot_rt(posterior, forecast_dates)
  return(plots)
}
