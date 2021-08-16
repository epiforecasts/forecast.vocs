# Targets defining the scenarios to evaluate
scenario_targets <- list(
  # define types of variant relationship to initial strain to tests
  tar_target(
    variant_relationship,
    c("scaled", "pooled", "independent"),
    deployment = "main"
  ),
  # define overdispersion testing scenarios
  tar_target(
    overdispersion,
    c(TRUE, FALSE),
    deployment = "main"
  ),
  # define data availability scenarios for sequence data
  tar_target(
    scenarios,
    head(define_scenarios(), n = 5),
    deployment = "main"
  )
)
