test_strain_inits <- function(message, strains) {
  test_that(message, {
    inits <- stan_inits(dt, strains = strains)
    expect_true(is.function(inits))
    inits1 <- inits()
    inits2 <- inits()
    expect_type(inits1, "list")
    names <- c("init_cases", "r_init", "r_noise", "eta", "beta", "sqrt_phi")
    if (strains == 2) {
      names <- c(
        names,
        c(
          "voc_mod", "voc_noise", "voc_eta", "voc_noise", "nvoc_noise",
          "voc_eta", "nvoc_eta"
        )
      )
    }
    expect_named(
      inits1[names],
      ignore.order = TRUE,
      expected = names
    )
    expect_false(isTRUE(all.equal(inits1, inits2)))
    expect_length(inits1$init_cases, strains)
    expect_type(inits1$init_cases, "double")
    expect_length(inits1$sqrt_phi, strains)
    expect_type(inits1$sqrt_phi, "double")
    expect_type(inits1$eta, "double")
    expect_length(inits1$eta, dt$t - 2)
    if (strains == 2) {
      expect_type(inits1$voc_mod, "double")
      expect_type(inits1$voc_noise, "double")
      expect_type(inits1$nvoc_noise, "double")
      expect_length(inits1$voc_mod, 1)
      expect_length(inits1$voc_noise, 1)
      expect_length(inits1$nvoc_noise, 1)
      expect_length(inits1$voc_eta, dt$t_seqf - 2)
      expect_length(inits1$nvoc_eta, dt$t_seqf - 2)
    }
  })
}

test_stan_fit <- function(message, dt, model, inits) {
  test_that(message, {
    skip_on_cran()
    fit <- silent_stan_fit(
      data = dt, model = model, init = inits, chains = 2, adapt_delta = 0.95,
      max_treedepth = 15, refresh = 0, show_messages = FALSE,
      iter_warmup = 1000, iter_sampling = 1000
    )
    expect_type(fit, "list")
    expect_data_table(fit)
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
    expect_lt(fit$per_divergent_transitons, 0.1)
    expect_lt(fit$max_treedepth, 15)
    expect_lt(fit$max_rhat, 1.1)
    expect_type(fit$fit_args[[1]], "list")
    expect_type(fit$data[[1]], "list")
  })
}

test_extract_forecast <- function(message, strains, posterior) {
  test_that(message, {
    skip_on_cran()
    forecasts <- extract_forecast(posterior)
    expect_type(forecasts, "list")
    expect_data_table(forecasts)
    expect_named(
      forecasts,
      c(
        "value_type", "type", "date", "horizon", "forecast_start", "mean",
        "median", "sd", "mad", "q5", "q20", "q80", "q95"
      )
    )
    if (strains == 1) {
      types <- "Overall"
      value_types <- c("cases", "growth", "rt")
    } else if (strains == 2) {
      types <- c("Combined", "VOC", "non-VOC")
      value_types <- c("cases", "voc", "growth", "rt")
    }
    expect_equal(unique(forecasts$type), types)
    expect_gt(min(forecasts$horizon), 0)
    expect_gte(max(forecasts$horizon), 4)
    expect_equal(unique(forecasts$value_type), value_types)
  })
}

test_filter_by_availability <- function(dt, message, tar_date = max(dt$date),
                                        case_date = tar_date,
                                        seq_date = tar_date) {
  test_that(message, {
    fdt <- filter_by_availability(dt,
      date = tar_date, seq_date = seq_date,
      case_date = case_date
    )
    # Dates are correctly ordered to avoid downstream issues
    expect_true(
      all(fdt[, ordered := date > shift(date)][!is.na(ordered)]$ordered)
    )
    # No data beyond sequence date is present
    expect_equal(nrow(fdt[seq_available > seq_date & is.na(seq_available)]), 0)
    # No data beyond case date is present
    expect_equal(
      nrow(fdt[cases_available > case_date & is.na(cases_available)]), 0
    )
    if (case_date > seq_date) {
      # If cases are available after sequences they are present
      expect_true(nrow(fdt[cases_available > seq_date]) > 0)
    }
    # If cases were available before sequences they are still present
    if (nrow(dt[date < tar_date & is.na(seq_available)]) > 0) {
      expect_true(nrow(fdt[date < tar_date & is.na(seq_available)]) > 0)
    }
    # Processed data passes observations checks
    expect_error(check_observations(fdt), NA)
  })
}


test_summarise_posterior <- function(message, fit, test_posterior,
                                     strains, equal = TRUE,
                                     probs = c(0.05, 0.2, 0.8, 0.95),
                                     voc_label = "VOC") {
  test_that(message, {
    skip_on_cran()
    posterior <- summarise_posterior(fit, probs, voc_label)
    attributes(test_posterior)$index <- NULL
    attributes(posterior)$index <- NULL
    # check in comparision to default posterior
    if (equal) {
      expect_equal(posterior, test_posterior)
    } else {
      expect_false(isTRUE(all.equal(posterior, test_posterior)))
    }
    # check column names and types
    expect_data_table(posterior)
    quantiles <- paste0("q", probs * 100)
    char_cols <- c("value_type", "variable", "clean_name")
    numeric_cols <- c(
      "mean", "median", "sd", "mad", quantiles, "rhat",
      "ess_bulk", "ess_tail"
    )
    logical_cols <- c("observed", "forecast_start", "exponentiated")
    expect_named(
      posterior,
      c(char_cols, "date", "type", "obs", logical_cols, numeric_cols)
    )
    purrr::walk(
      char_cols,
      ~ expect_type(posterior[[.]], "character")
    )
    purrr::walk(
      numeric_cols,
      ~ expect_type(posterior[[.]], "double")
    )
    purrr::walk(
      logical_cols,
      ~ expect_type(posterior[[.]], "logical")
    )
    expect_s3_class(posterior$date, "Date")
    expect_type(posterior$obs, "double")
    # check quantiles increase in size
    purrr::walk(
      seq_along(quantiles)[-1],
      ~ expect_true(
        all(posterior[[quantiles[.]]] > posterior[[quantiles[. - 1]]])
      )
    )
    # check contents of types
    if (strains == 1) {
      types <- c(NA, "Overall")
      value_types <- c("model", "cases", "growth", "rt", "raw")
    } else if (strains == 2) {
      types <- c(NA, "Combined", voc_label, paste0("non-", voc_label))
      value_types <- c("model", "cases", "voc", "growth", "rt", "raw")
    }
    expect_type(posterior$type, "character")
    expect_equal(unique(posterior$type), types)
    expect_type(posterior$value_type, "character")
    expect_equal(unique(posterior$value_type), value_types)
    # Check dates are increasing only
    cases <- posterior[value_type %in% "cases"]
    cases <- cases[type %in% c("Overall", "Combined")]
    expect_dates_unique(cases)
    # Check contents of fit diagnostics and minimum values for example fit
    expect_gt(min(posterior$ess_bulk), 250)
    expect_gt(min(posterior$ess_tail), 250)
    expect_lte(max(posterior$rhat, na.rm = TRUE), 1.1)
  })
}