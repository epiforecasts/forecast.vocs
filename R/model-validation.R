#' Launch shinystan
#'
#' Launch shinystan an interactive tool for stan model evaluation
#'
#' @return NULL
#'
#' @family modelvalidation
#' @export
#' @inheritParams fv_tidy_posterior
#' @examplesIf interactive()
#' obs <- filter_by_availability(
#'   germany_covid19_delta_obs,
#'   date = as.Date("2021-06-12")
#' )
#'
#' dt <- fv_as_data_list(obs)
#' inits <- fv_inits(dt)
#' fit <- fv_sample(dt, init = inits, adapt_delta = 0.99, max_treedepth = 15)
#' bp_launch_shinystan(fit)
bp_launch_shinystan <- function(fit) {
  requireNamespace("shinystan", quietly = TRUE)
  stanfit <- convert_to_stanfit(fit)
  shinystan::launch_shinystan(stanfit)
  return(invisible(NULL))
}
