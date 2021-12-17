on_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI")))
}

not_on_cran <- function() {
  identical(Sys.getenv("NOT_CRAN"), "true")
}

silent_fv_sample <- function(...) {
  utils::capture.output(
    fit <- suppressMessages(fv_sample(...))
  )
  return(fit)
}
