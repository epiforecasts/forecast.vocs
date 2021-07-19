#' Link dates by time for posterior parameter estimates
#' @param posterior A data frame of summarised posterior estimates
#' as returned by `cmdstanr::summary` with  an additional type variable
#' which contains the following character string options: "non-DELTA",
#' "DELTA", "Combined", "Overall".
#' @param data A list of data as returned in the "data" entry of the output
#' returned by `stan_fit()`.
#' @param mod_end Integer, defaults to 0. Amount to shift the end date of
#' estimates
#' @export
#' @return A dataframe with an additional data column
link_dates_by_type <- function(posterior, data, mod_end = 0) {
    # extract info from lis
    t <- data$t
    t_nseq <- data$t_nseq
    t_seqf <- data$t_seqf
    start_date <- data$start_date
    end_date <- start_date + 7 * t - 1 - mod_end

    # build dates data frame
    dates <- data.table(
     start = c(rep(start_date, 3), start_date + t_nseq * 7),
    end = end_date,
    type = c("non-DELTA", "Combined", "Overall", "DELTA")
    )
    dates <- dates[, .(date = seq(start, end, by = "weeks")), by = "type"]
    dates <- dates[, id := 1:.N, by = "type"]

    # link to input data frame
    posterior <- setDT(posterior)
    posterior <- posterior[, id := 1:.N, by = "type"]
    posterior <- merge(posterior, dates, by = c("type", "id"))
    posterior <- posterior[, id := NULL]
    setcolorder(posterior, neworder = c("type", "date"))
    return(posterior)
  }

#' Summarise the posterior
#' @export
#' @importFrom purrr reduce map walk
#' @importFrom posterior quantile2 default_convergence_measures
#' @importFrom data.table .SD .N := setcolorder
#' @examples
#' \dontrun{
#' dt <- stan_data(latest_obs(germany_obs))
#' inits <- stan_inits(dt)
#' options(mc.cores = 4)
#' fit <- stan_fit(dt, init = inits, adapt_delta = 0.99, max_treedepth = 15)
#' summarise_posterior(fit) -> p
#' }
summarise_posterior <- function(fit,
                                probs = c(
                                  0.01, 0.025,
                                  seq(0.05, 0.95, by = 0.05),
                                  0.975, 0.99
                                )) {
  # NULL out variables
  variable <- type <- NULL
  # extract useful model info
  data <- fit$data
  fit <- fit$fit

  # extract summary parameters of interest and join
  sfit <- list(
    fit$summary(variables = NULL, mean, median, sd, mad),
    fit$summary(
      variables = NULL, quantile2,
      .args = list(probs = probs)
    ),
    fit$summary(variables = NULL, default_convergence_measures())
  )
  cbind_custom <- function(x, y) {
    x <- setDT(x)
    y <- setDT(y)[, variable := NULL]
    cbind(x, y)
  }
  sfit <- purrr::reduce(sfit, cbind_custom)
  # detect if delta is in the data
  delta_present <- any(grepl("delta", sfit$variable))

  # summarise cases with delta label
  cases <- sfit[grepl("sim_", variable)]
  cases[, type := data.table::fcase(
    grepl("_ndelta", variable), "non-DELTA",
    grepl("_delta", variable), "DELTA",
    rep(delta_present, .N), "Combined",
    default = "Overall"
  )]
  cases <- link_dates_by_type(cases, data)

  # summarise delta if present
  delta <- sfit[grepl("frac_delta", variable)]
  delta[, type := "DELTA"]
  delta <- link_dates_by_type(delta, data)

  # summarise Rt and label
  rt <- sfit[grepl("r\\[", variable)]
  rt[, type := fcase(
    grepl("delta_r", variable), "DELTA",
    grepl("com_r", variable), "Combined",
    grepl("r\\[", variable) & delta_present, "non-DELTA",
    grepl("r\\[", variable), "Overall"
  )]
  rt <- link_dates_by_type(rt, data, mod_end = 1)

  # copy into growth
  growth <- copy(rt)

  # transform growth to Rt
  cols <- c("mean", "median", paste0("q", probs * 100))
  rt[, (cols) := lapply(.SD, exp), .SDcols = cols, by = "type"]

  # summarise model parameters
  param_lookup <- data.table(
    variable = c(
      "r_init", "r_noise", "beta", "delta_mod", "avg_delta_mod",
      "delta_noise", "ndelta_noise", "init_cases[1]", "init_cases[2]",
      "phi[1]", "phi[2]"
    ),
    clean_name = c(
      "Initial growth", "Growth (sd)", "Beta",
      "Initial DELTA effect", "Average DELTA effect",
      "DELTA (sd)", "Non-DELTA (sd)", "Initial cases",
      "Initial DELTA cases", "Notification overdispersion",
      "Sequencing overdispersion"
    ),
    exponentiated = c(
      rep(FALSE, 3), rep(TRUE, 2), rep(FALSE, 2),
      rep(TRUE, 2), rep(FALSE, 2)
    )
  )
  model <- merge(param_lookup, sfit, by = "variable")
  model[exponentiated == TRUE, (cols) := lapply(.SD, exp), .SDcols = cols]

  # join output and reorganise as needed
  out <- list(cases = cases, delta = delta, growth = growth, rt = rt)
  out <- purrr::map(out, ~ .x[, variable := NULL])
  out$model <- model
  return(out)
}

#' Combine multiple summarised posteriors
#' @export
#' @importFrom purrr map transpose
combine_posteriors <- function(posteriors_list) {
  posteriors <- purrr::transpose(posteriors_list)
  posteriors <- purrr::map(posteriors, rbindlist,
    use.names = TRUE, fill = TRUE,
    idcol = "model"
  )
  return(posteriors)
}

#' Save a summarised posterior
#' @export
#' @importFrom purrr safely walk2
#' @importFrom data.table fwrite
save_posterior <- function(posterior, save_path = tempdir()) {
  file_names <- names(posterior)
  sfwrite <- purrr::safely(fwrite)
  purrr::walk2(
    posterior, file_names,
    ~ sfwrite(.x, paste0(save_path, "/", .y, ".csv"))
  )
}
