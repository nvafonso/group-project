# Personality & Teamwork Quality Explorer

This project explores relationships between HEXACO personality traits
and teamwork quality using questionnaire data.

The Shiny app lets users visualize observed relationships and run
hypothetical simulations comparing three team-formation strategies:
random assignment, balancing personality scores across teams, and
grouping people with similar scores.

Simulated outcomes depend on user-selected assumptions. They do not
demonstrate that a grouping strategy improves teamwork in practice.

This project was developed for the Programming in Psychological Science
course at the University of Amsterdam.

## Dataset

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

It contains personality questionnaire responses from students in software teams, along with separate measures of teamwork quality, including cohesion, communication, coordination, commitment, support, and balance.  

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

- Teamwork quality data 

- All related dataset tables 

Why it fits 

You can investigate whether personality traits are associated with teamwork quality, and which dimensions of teamwork show the strongest relationships.  

Important limitation 

The personality table has 54 respondents, with 60 questionnaire items per respondent. The accessible data description doesn't establish that you have enough team-level observations and performance outcomes to train a reliable team-performance predictor.

## Requirements

Use R and RStudio. Install the required packages by running this command
in the RStudio Console:

```r
install.packages(c("shiny", "ggplot2", "tidyr"))
```

An internet connection is required to download the dataset from OSF.
