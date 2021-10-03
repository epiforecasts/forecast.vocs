#' @title Check a parameter is a length is 2 numeric vector
#' @description Check a parameter is a length 2 numeric vector
#' defining a mean and a standard deviation.
#' @param param A parameter to check the format of.
#' @param name A character string naming the variable
#' to check.
#' @return NULL
check_mean_sd <- function(param, name = "r_init") {
  if (!is.numeric(param)) {
    stop(name, " is not numeric")
  }

  if (length(param) != 2) {
    stop(
      name,
      " must be of length 2 (mean followed by standard deviation)"
    )
  }
  return(invisible(NULL))
}

#' @title Check a parameter is logical
#' @inheritparams check_mean_sd
#' @return NULL
check_logical <- function(param, name = "r_init") {
  if (!is.logical(param)) {
    stop(name, " is not logical")
  }

  if (length(param) != 1) {
    stop(name, " must be of length 1")
  }
  return(invisible(NULL))
}
