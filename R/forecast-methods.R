summary.forecast <- function(x, type, ...) {
  type <- match.arg(type, c("fit", "diagnostics", "posterior", "forecast"))
  if (type == "fit") {
    out <- x$fit
    if (length(out) == 1) {
      out <- out[[1]]
    }
  }else if (type == "diagnostics") {

  }else{
    out <- unnest_posterior(x, target = type)
  }
  return(out)
}

plot.forecast <- function(target = "posterior", type, ...) {
  target <- match.arg(target, c("posterior", "forecast"))
  type <- match.arg(
    type,
    c("cases", "voc_frac", "voc_advantage", "growth", "rt")
  )
  target <- summary(forecast, type = target)
  plot(target, type = type, ...)
}