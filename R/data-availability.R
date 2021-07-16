#' Define data availability scenarios
#'
#' @param seq_lag The number of  weeks that sequences lag the date. Default is
#' to test 0 to 3 weeks of lag.
#' @param seq_samples Fraction of samples to include (deterministic scaling).
#' The default is to test all samples down to 25% of samples by 25% increments.
#' @param prior_delta A list of mean and standard  deviations to use to inform
#' the prior for additional transmissibility of the delta variant. The default
#' a uninformed no prior knowledge prior (0, 0.5), a weak assumption of a
#' transmissibility advantage (0.2, 0.2), an estimate based on early UK travel
#' adjusted growth (0.74, 0.1), and an estimate based on the posterior in
#' Germany estimated with all available data (0.27, 0.1).
#' @return A data frame of scenario definitions with ids
#' @examples
#' define_scenarios()
define_scenarios <- function(seq_lag = 0:3,
                             seq_samples = seq(1, by = -0.25, length.out = 4),
                             prior_delta = list(
                               c(0, 0.5), c(0.2, 0.2),
                               c(0.74, 0.1), c(0.27, 0.1)
                             )) {
  scenarios <- CJ(seq_lag, seq_samples, prior_delta, sorted = FALSE)
  scenarios[, id := 1:.N]
  setcolorder(scenarios, neworder = "id")
  return(scenarios)
}

generate_scenario <- function(obs, scenario) {

}
