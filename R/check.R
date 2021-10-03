#' @title Check a parameter is the correct type and length
#' @param param A parameter to check the format of.
#' @param name A character string naming the variable
#' to check.
#' @param type A character string identifying the allowed parameter
#' type (must be a type with an is.type function).)
#' @param length Numeric, allowed length of the variable.
#' @return NULL
#' @export
#' @keywords internal
check_param <- function(param, name = "param",
                        type = "numeric", length = 1) {
  if (!do.call(paste0("is.", type), list(param))) {
    stop(name, " is not ", type)
  }

  if (length(param) != length) {
    stop(name, " must be of length ", length)
  }
  return(invisible(NULL))
}
