print.fv_posterior <- function(x, ...) {
  message(
    "Available value types: ",
    paste(unique(x$value_type), collpase = ", ")
  )
  class(x) <- class(x)[-1]
  print(x, ...)
}

summary.fv_posterior <- function(x, type = "cases") {
  type <- match.arg(
    type,
    c("model", "cases", "voc_frac", "voc_advantage", "growth", "rt")
  )
}

plot.fv_posterior <- function(x, obs, type = "cases", ...) {
  type <- match.arg(
    type,
    c("cases", "voc_frac", "voc_advantage", "growth", "rt", "all")
  )
  if (type == "cases") {
    plot_cases()
  } else if (type == "voc_frac") {
    plot_voc_frac()
  } else if (type == "voc_advantage") {
    plot_voc_advantage()
  } else if (type == "growth") {
    plot_growth()
  } else if (type == "rt") {
    plot_rt()
  } else if (type == "all") {
    plot_posterior()
  }
}
