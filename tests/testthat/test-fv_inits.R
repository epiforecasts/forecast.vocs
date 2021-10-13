# define test data
obs <- filter_by_availability(
  germany_covid19_delta_obs,
  date = "2021-06-19"
)

dt <- fv_as_data_list(obs)

test_strain_inits(
  "Single strain inits can be sample initial conditions as expected using
  default settings from fv_as_data_list",
  strains = 1
)

test_strain_inits(
  "Two strain inits can be sample initial conditions as expected using
  default settings from stan data",
  strains = 2
)
