# Personality & Teamwork Quality Explorer

This project explores HEXACO personality and teamwork-quality questionnaire data. It contains an exploratory analysis, a standalone data simulation, and a Shiny app. Simulated outcomes reflect assumptions for exploration; they are not estimates or predictions for real teams.

## Project structure

- `data/` — raw questionnaire data and processed scores used by the simulation.
- `simulated_data/` — simulation outputs, including the CSV loaded by the app.
- `figures/` — project figures.
- `src/` — additional analysis scripts and older copies; the current app and simulation entry points are the root-level files listed below.
- `data_processing.R` — score the raw questionnaire data and fit the exploratory model.
- `data_simulation.R` — run the standalone simulation using `data/processed_data.csv`.
- `simulation_core.R` — shared team-effect and team-assignment calculations.
- `sanity_checks.R` — quick deterministic checks for the shared simulation logic.
- `Shiny_App.R` — current interactive app.
- `main.Rmd` — project report.
- `project.R` — convenience functions for running the main components.

## Run the project

Open `group-project.Rproj` in RStudio so the project root is the working directory. The standalone simulation requires R 4.1 or newer and the packages `dplyr`, `tidyr`, `ggplot2`, and `psych`. The analysis and app also use `tidyr`, `ggplot2`, and `shiny`.

To run the analysis, use `source("data_processing.R")`. To regenerate the simulation output used by the app, use `source("data_simulation.R")`. It reads the bundled `data/processed_data.csv` and writes its CSV and PNG outputs under `simulated_data/`. The Shiny app reads `simulated_data/combined_individual_level_results.csv`; regenerate that file before running the app if you want it to reflect a new simulation.

Run `source("sanity_checks.R")` for quick checks of team sizes, expected between-team variance ordering on a fixed example, and zero-effect outcome comparisons.

The same entry points are available through `project.R`:

```r
source("project.R")
analysis <- run_analysis()
# Run only when you want to regenerate the simulation files:
# simulation <- run_simulation()
launch_app()
```

To knit `main.Rmd`, open it in RStudio and select **Knit**. The report runs the sanity checks and analysis; the simulation-generation chunk is disabled by default so knitting does not regenerate all outputs.

## Data provenance

`data/HEXACO_TWQ_raw-data.csv` is the unchanged dataset copy from Zähl (2023). See `main.Rmd` for the source, download, and catalogue links. Review the scoring notes and limitations in the report before interpreting the exploratory analyses or simulation results.
