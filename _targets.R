# load targets + parallel packages
library(targets)
library(tarchetypes)
library(future)
library(future.callr)
plan(callr)

# run  this workflow:
# tar_make_future(workers = future::availableCores()) # nolint

# explore this workflow:
# tar_visnetwork(targets_only = TRUE) # nolint

# watch whilst running:
# tar_watch(targets_only = TRUE) # nolint

# load required packages and watch bp.delta for changes
tar_option_set(
  packages = c("bp.delta", "purrr", "data.table", "scoringutils"),
  deployment = "worker",
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
  # Compile models
  tar_target(
    single_model,
    bp.delta::load_model(strains = 1),
    format = "file", deployment = "main",
  ),
  tar_target(
    two_model,
    bp.delta::load_model(strains = 2),
    format = "file", deployment = "main",
  ),
  # Arguments passed to `forecast()` to control forecasting
  tar_target(
    forecast_args,
    list(
      horizon = 8, adapt_delta = 0.9, max_treedepth = 15,
      parallel_chains = 1, plot = FALSE, chains = 2
    ),
    deployment = "main"
  )
)
# branch targets across data sources (see individual targets scripts in
# targets/ for further details of each step)
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
  combined_targets,
  summarise_source_targets
)
