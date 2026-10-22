
# HEXACO Personality & Teamwork Quality Explorer

## Project Description
This project explores the relationship between HEXACO personality traits and teamwork quality using questionnaire data from student software teams (N = 54). It includes a comprehensive statistical analysis (correlations and mixed-effects models) and an interactive Shiny application that allows users to explore observed relationships and simulate hypothetical team-formation strategies.

## Dependencies
To run this project, you need R (version 4.0 or higher) and the following packages. You can install them using:
```R
install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "lme4", "broom.mixed", "performance", "reshape2", "see", "psych"))
```

## Repository Structure

data/: Contains the raw and processed datasets.

src/: Contains R scripts.
data_processing.R: Cleans data and computes scale scores.
data_analysis.R: Descriptives, correlations, and plots.
mixed_model.R: Linear mixed-effects models and diagnostics.
data_simulation.R: Functions for the Shiny app simulation.
Shiny_App.R: The interactive application.

figures/: Contains all generated plots.

main.Rmd: The main deliverable report combining code, results, and explanations.

README.md: Project instructions and overview.

## How to Run

Clone this repository to your local machine.
Open group-project.Rproj in RStudio to set the working directory correctly.
To view the report: Open main.Rmd and click the "Knit" button to generate the full HTML report.
To run the Shiny App: Open src/Shiny_App.R and click "Run App" (or run shiny::runApp("src/Shiny_App.R") in the console).

## References
Zähl, P., Theis, S., & Wolf, M. R. (2023). HEXACO personality and teamwork dataset.
Ashton, M. C., & Lee, K. (2009). The HEXACO-60: A short measure of the major dimensions of personality. Journal of Personality Assessment.
Hoegl, M., & Gemuenden, H. G. (2001). Teamwork quality and the success of innovative projects. Organization Science.
