# Review and handoff notes

## Current project structure

The current entry points are in the project root:

- `data_processing.R` — scores the raw questionnaire data and fits the exploratory analysis.
- `data_simulation.R` — runs the standalone simulation using `data/processed_data.csv`.
- `simulation_core.R` — shared simulation calculations; the Shiny app also uses it for team assignments.
- `Shiny_App.R` — launches the app.
- `sanity_checks.R` — runs deterministic checks of team assignments and zero-effect outcomes.
- `main.Rmd` — project report.
- `project.R` — convenience functions for running the analysis, simulation, or app.

The `src/` folder and `data_simulation_copy.R` contain other or older scripts. They are not the current app or standalone simulation entry points.

## Data flow and requirements

- The analysis and app use the raw questionnaire file at `data/HEXACO_TWQ_raw-data.csv`.
- The standalone simulation reads the processed file at `data/processed_data.csv`. It does not create that file automatically.
- The standalone simulation writes its outputs to `simulated_data/`.
- The app's simulated-data option requires `simulated_data/combined_individual_level_results.csv`. Include that file in the GitHub repository or deployment bundle.
- The app's interactive team comparison uses its own controls and simulation loop, with shared calculations from `simulation_core.R`; it is not simply a display of the standalone simulation output.
- `main.Rmd` runs the analysis and sanity checks when knitted. Its simulation-generation chunk is disabled by default.

## Interpretation and review points

- The simulation uses assumptions for trait effects, diversity effects, and outcome generation. These are exploratory settings, not causal estimates or predictions about real teams.
- The variance-ordering sanity check uses a fixed example so it is reproducible. It does not guarantee that every random simulation run will have that ordering.
- The observed dataset is small and participants belong to teams. Interpret individual-level associations with that clustering in mind.
- Check that the questionnaire scoring and reverse-keying match the provided documentation before interpreting scores. The raw dataset is stored as an unchanged copy; the analysis applies its scoring rules to the item responses.
- Record any remaining choices about missing responses and the interpretation of exploratory analyses.


Record the actual results of these checks before submission. Static file and path checks alone do not confirm that the app or report runs successfully.
