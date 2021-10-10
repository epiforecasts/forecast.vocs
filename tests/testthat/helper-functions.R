on_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI")))
}

not_on_cran <- function() {
  on_ci() || identical(Sys.getenv("NOT_CRAN"), "true")
}

silent_stan_fit <- function(...) {
  utils::capture.output(
    fit <- suppressMessages(stan_fit(...))
  )
  return(fit)
}

test_stan_fit <- function(message, dt, model, inits) {
  test_that(message, {
    skip_on_cran()
    fit <- silent_stan_fit(
      data = dt, model = model, init = inits, chains = 2, adapt_delta = 0.9,
      max_treedepth = 15, refresh = 0, show_messages = FALSE,
      iter_warmup = 1000, iter_sampling = 1000
    )
    expect_type(fit, "list")
    expect_true(data.table::is.data.table(fit))
    expect_equal(nrow(fit), 1)
    expect_named(
      fit,
      expected = c(
        "fit", "data", "fit_args", "samples", "max_rhat",
        "divergent_transitions", "per_divergent_transitons", "max_treedepth",
        "no_at_max_treedepth", "per_at_max_treedepth"
      )
    )
    expect_equal(class(fit$fit[[1]])[1], "CmdStanMCMC")
    expect_lt(fit$per_divergent_transitons, 0.15)
    expect_lt(fit$max_treedepth, 15)
    expect_lt(fit$max_rhat, 1.1)
    expect_type(fit$fit_args[[1]], "list")
    expect_type(fit$data[[1]], "list")
  })
}
