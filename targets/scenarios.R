# Targets defining the scenarios to evaluate
scenario_targets <- list(
  # define types of variant relationship to initial strain to tests
  tar_target(
    variant_relationship_scenarios,
    c("scaled", "pooled", "independent"),
    deployment = "main"
  ),
  # define overdispersion testing scenarios
  tar_target(
    overdispersion_scenarios,
    c(TRUE, FALSE),
    deployment = "main"
  ),
  # define data availability scenarios for sequence data
  tar_target(
    data_availability_scenarios,
    define_scenarios(),
    deployment = "main"
  )
)
