# define test data
obs <- filter_by_availability(
  germany_covid19_delta_obs,
  date = "2021-06-19"
)

dt <- stan_data(obs)

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

test_strain_inits(
  "Single strain inits can be sample initial conditions as expected using
  default settings from stan_data",
  strains = 1
)

test_strain_inits(
  "Two strain inits can be sample initial conditions as expected using
  default settings from stan data",
  strains = 2
)
