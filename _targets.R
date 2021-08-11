# load targets + parallel packages
library(targets)
library(tarchetypes)
library(future)
library(future.callr)
plan(callr)

# load required packages and watch bp.delta for changes
tar_option_set(
  packages = c("bp.delta", "purrr", "data.table"),
  deployment = "main",
  workspace_on_error = TRUE
)

# load target modules
targets <- list.files("targets", full.names = TRUE)
purrr::walk(targets, source)

# datasets of interest
sources <- list(source = "germany")
load_obs <- function(source) {
  source <- match.arg(source,
    choices = "germany"
  )
  if (source %in% "germany") {
    bp.delta::germany_obs
  }
}

# input and control targets
meta_targets <- list(
  tar_target(
    single_model,
    bp.delta::load_model(strains = 1),
    format = "file"
  ),
  tar_target(
    two_model,
    bp.delta::load_model(strains = 2),
    format = "file"
  ),
  tar_target(
    forecast_args,
    list(
      horizon = 8, adapt_delta = 0.9, max_treedepth = 15,
      parallel_chains = 1, plot = FALSE, chains = 2
    )
  )
)
# branch targets across data sources
combined_targets <- tar_map(
  values = sources,
  c(
    obs_targets,
    forecast_targets,
    scenario_forecast_targets,
    summarise_forecast_targets
  )
)
# Combine and evaluate targets
c(
  meta_targets,
  scenario_targets,
  combined_targets
)
