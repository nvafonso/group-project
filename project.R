# High-level entry points. Run from the repository root.
# Each script gets its own environment so simulation objects cannot overwrite
# the observed-data model or the app's scoring definitions.

# Return the analysis script's objects, including model and communication_plot.
run_analysis <- function() {
  e <- new.env(parent = globalenv())
  sys.source("src/data_processing.R", envir = e)
  e
}

# Return simulation objects; the supporting script also exports CSVs and PNGs.
run_simulation <- function() {
  e <- new.env(parent = globalenv())
  sys.source("src/data_simulation.R", envir = e)
  e
}

# Launch the interactive application; stop it with Esc in the RStudio Console.
launch_app <- function() {
  e <- new.env(parent = globalenv())
  app <- source("src/Shiny_App.R", local = e)$value
  shiny::runApp(app, launch.browser = TRUE)
}
