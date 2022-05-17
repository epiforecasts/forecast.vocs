#' @rawNamespace import(data.table, except = transpose)
#' @import cmdstanr
#' @import ggplot2
#' @importFrom stats median rnorm
NULL



#' Load a package example
#'
#' Loads examples of posterior and forecast summaries produced
#' using example scripts. Used to streamline examples,
#' in package tests and to enable users to explore package functionality
#' without needing to install `cmdstanr`.
#'
#' @param type A character string indicating the example to load.
#'  Supported options are "posterior", "forecast", "observations",
#'  and "script" which are the
#' output of [fv_tidy_posterior()],  [fv_extract_forecast()],
#' `filter_by_availability` (with the date argument set to "2021-08-26"
#' applied to the [germany_covid19_delta_obs] package dataset),
#' and the script used to generate these examples respectively.
#'
#' @return A `data.table` of summarised output
#'
#' @family data
#' @export
#' @inheritParams fv_inits
#' @examples
#' # Load the summarised posterior from an example fit of the one strain model
#' fv_example(strains = 1, type = "posterior")
#'
#' # Load the summarised forecast from this posterior
#' fv_example(strains = 1, type = "forecast")
#'
#' # Load the script used to generate these examples
#' # Optionally source this script to regenerate the example
#' readLines(fv_example(strains = 1, type = "script"))
fv_example <- function(strains = 1, type = "posterior") {
  type <- match.arg(
    type,
    choices = c("posterior", "forecast", "observations", "script")
  )

  if (type %in% c("posterior", "forecast")) {
    file <- system.file(
      "extdata", paste(type, strains, "strains_example.csv", sep = "_"),
      package = "forecast.vocs"
    )
  } else if (type %in% "observations") {
    file <- system.file(
      "extdata", "observations_example.csv",
      package = "forecast.vocs"
    )
  } else if (type %in% "script") {
    file <- system.file(
      "scripts", paste(strains, "strains_example.R", sep = "_"),
      package = "forecast.vocs"
    )
  }

  if (type %in% "script") {
    out <- file
  } else {
    out <- fread(file)
    if (type %in% c("posterior", "forecast")) {
      class(out) <- c("fv_posterior", class(out))
    }
  }
  return(out)
}

#' Save plots by name
#'
#' @param save_path A character string indicating where to save plots
#' if required.
#'
#' @param type A character string indicating the format to use to save plots.
#'
#' @param plots A named list of `ggplot2` plots.
#'
#' @param ... Additional arguments passed to [ggplot2::ggsave()]
#'
#' @return NULL
#'
#' @family plot
#' @export
#' @importFrom purrr walk2
#' @examples
#' posterior <- fv_example(strains = 2, type = "posterior")
#' p <- plot(posterior, type = "all")
#' save_plots(p, save_path = tempdir())
save_plots <- function(plots, save_path = NULL, type = "png", ...) {
  if (!is.null(save_path)) {
    walk2(
      plots, names(plots),
      ~ ggsave(
        filename = file.path(save_path, paste0(.y, ".", type)),
        plot = .x, device = type
      ),
      ...
    )
  }
  return(invisible(NULL))
}


#' Calculate piecewise steps
#'
#' This helper function streamlines the calculation of piecewise steps.
#' This may be useful when specifying random walks, AR processes, etc.
#'
#' @param t Integer, the timespan over which to calculate steps
#'
#' @param step Integer, the frequency at which to step.
#'
#' @param offset Integer, the amount to offset steps. This can be used to
#' index steps from this index.
#'
#' @param steps_post_offset Logical, defaults to `TRUE`. Should steps be added
#' after the offset.
#'
#' @return A list containing two elements: `n` (the number of steps) and `steps`
#' the location of steps as a binary variable.
#'
#' @family preprocess
piecewise_steps <- function(t, step, offset = 0, steps_post_offset = TRUE) {
  times <- 1:t - offset - 1
  steps <- as.integer(times %% step == 0)
  if (!steps_post_offset) {
    steps <- ifelse(1:t > offset, 0, steps)
  }
  return(list(n = sum(steps), steps = steps))
}

utils::globalVariables(
  c(
    ".", ".draw", "cases", "cases_available", "dates", "end", "exponentiated",
    "horizon", "id", "mad", "max_treedepth", "mean_share_voc",
    "no_at_max_treedepth", "observed", "patterns", "per_at_max_treedepth",
    "q20", "q5", "q80", "q95", "quantile", "results", "row_id", "sd",
    "seq_available", "seq_total", "seq_voc", "share_voc", "start", "type",
    "Type", "value", "value_type", "..non_list_cols", "forecast_start",
    "data", "time", "day_of_week", "perio", "special", "specials_to", "period",
    "model", "strains"
  )
)
