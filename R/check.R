#' @title Check a parameter is the correct type and length
#' @param param A parameter to check the format of.
#' @param name A character string naming the variable
#' to check.
#' @param type A character string identifying the allowed parameter
#' type (must be a type with a is.type function except for a Date).
#' @param length Numeric, allowed length of the variable. Defaults to
#' any allowed length
#' @return NULL
#' @export
#' @keywords internal
check_param <- function(param, name = "param",
                        type = "numeric", length) {
  if (is.null(param)) {
    stop(name, " does not exist")
  }
  is.Date <- function(x) { # nolint
    inherits(x, "Date")
  }
  if (!do.call(paste0("is.", type), list(param))) {
    stop(name, " is not ", type)
  }
  if (!missing(length)) {
    if (length(param) != length) {
      stop(name, " must be of length ", length)
    }
  }
  return(invisible(NULL))
}

#' @title Check a data.frame
#' @param dataframe A data.frame to check.
#' @param req_vars A character vector of variables that are required.
#' @param req_types A character vector of types for each required variable.
#' @export
#' @return NULL
#' @importFrom purrr walk2
#' @keywords internal
check_dataframe <- function(dataframe, req_vars, req_types) {
  if (!is.data.frame(dataframe)) {
    stop("The inputs is not a data.frame")
  }
  if (!missing(req_vars) | !missing(req_types)) {
    if (length(req_vars) != length(req_types)) {
      stop("req_vars is not the same length as req_types")
    }
    check_param(req_vars, "req_vars", type = "character")
    check_param(req_types, "req_types", type = "character")
    purrr::walk2(
      req_vars, req_types,
      ~ check_param(param = dataframe[[.x]], name = .x, type = .y)
    )
  }
  return(invisible(NULL))
}

#' @title Check observations are in the correct format
#' @param obs A data.frame of observations to check for formatting issues
#' @export
#' @return NULL
#' @examples
#' obs <- latest_obs(germany_covid19_delta_obs)
#' check_observations(obs)
check_observations <- function(obs) {
  req_vars <- c(
    "date", "cases", "cases_available", "seq_total",
    "seq_voc", "share_voc", "seq_available"
  )
  req_types <- c(
    "Date", "numeric", "Date", "numeric", "numeric",
    "numeric", "Date"
  )
  check_dataframe(obs, req_vars, req_types)
  if (length(obs$date) != length(unique(obs$date))) {
    stop("Dates are duplicated")
  }
  return(invisible(NULL))
}

#' Check Quantiles Required are Present
#' @param posterior A dataframe containing quantiles identified using
#' the "q5" approach.
#' @param req_probs A numeric vector of required probabilties.
#' @export
check_quantiles <- function(posterior, req_probs = c(0.5, 0.95, 0.2, 0.8)) {
  cols <- colnames(posterior)
  if (sum(cols %in% c("q5", "q95", "q20", "q80")) != 4) {
    stop(
      "Following quantiles must be present (set with probs): ",
      paste(req_probs, collapse = ", ")
    )
  }
  return(invisible(NULL))
}
