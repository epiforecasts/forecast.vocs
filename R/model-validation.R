#' Launch shinystan
#'
#' Launch shinystan an interactive tool for stan model evaluation
#'
#' @return NULL
#'
#' @family postprocess
#' @inheritParams summarise_posterior
#' @examples
#' \dontrun{
#' obs <- latest_obs(germany_covid19_delta_obs)
#' dt <- stan_data(obs)
#' inits <- stan_inits(dt)
#' fit <- stan_fit(dt, init = inits, adapt_delta = 0.99, max_treedepth = 15)
#' bp_launch_shinystan(fit)
#' }
bp_launch_shinystan <- function(fit) {
  requireNamespace("shinystan", quietly = TRUE)
  stanfit <- convert_to_stanfit(fit)
  shinystan::launch_shinystan(stanfit)
  return(invisible(NULL))
}
