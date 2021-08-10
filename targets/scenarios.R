# Targets defining the scenarios to evaluate
scenario_targets <- list(
  tar_target(
    variant_relationship,
    c("scaled", "pooled", "independent")
  ),
  tar_target(
    overdispersion,
    c(TRUE, FALSE)
  ),
  tar_target(
    scenarios,
    head(define_scenarios(), n = 5)
  )
)
