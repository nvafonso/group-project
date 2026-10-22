# Review and handoff notes

## Status

Prepared from the five uploaded files and the previously supplied raw CSV.
This is a proposed integration package, not a verified final submission.
R is unavailable in the preparation environment: no Knit or Shiny execution is
claimed. Static checks confirm referenced files exist and the raw copy is unchanged.
The separate simulation is the code from `Pasted text(6).txt`, saved under its own
header's filename `src/data_simulation.R`; its contents are unchanged.

## Changes requiring teammate review

- Supporting R files moved to `src/`; raw data placed in `data/`.
- `src/project.R` supplies three entry functions. Each component is sourced in a
  separate environment so its generic objects such as df do not overwrite another's.
- Observed script reads the bundled raw CSV; summary-table columns use `=` instead
  of assignment expressions; the Agreeableness label spelling is corrected.
- App reads the same bundled CSV and checks required item columns.
- App selectInput vectors now map readable labels to internal score names. Previously
  the label/value direction was reversed, so inputs could refer to absent columns.
- Strategy plot averages now group by BOTH simulation run and strategy. Previously
  all strategies were averaged together within each run.
- App aggregate summary uses the returned matrix directly instead of calling
  do.call(rbind, ...) on that matrix.
- App simulation sets seed 101 each time for reproducibility with identical controls.
- README and main document explain both distinct simulations, provide entry points,
  and use dynamically calculated observed-model estimates rather than fixed results.
- No empirical coefficients, inferential method, scoring keys, missing-item policy,
  or simulation effects have been changed.

## Decisions for the analysis and app authors

1. Check raw item coding and scale keys against the original questionnaire.
2. Agree on missing-item handling: observed script uses available items; app requires
   complete scales. Report exclusions and a minimum answered-item rule if adopted.
3. Review regression dependence: respondents share teams. Ordinary lm intervals do
   not account for clustering. Choose and justify an appropriate final method.
4. Document selection of agreeableness/communication after inspecting correlations;
   do not present its ordinary p-value as a preplanned confirmatory test.
5. App model centers/scales team means and diversity WITHIN each strategy/run.
   Before clipping, average standardized contributions are zero. Consequently it
   cannot use an overall mean increase to demonstrate a balancing benefit. Decide
   whether variance between teams is the intended comparison or revise the model.
6. App results are event-triggered, while labels/metric controls remain live. Rerun
   after every control change, or make the app display the settings of the saved run.
7. Standalone simulation coefficients are illustrative and currently lack theoretical
   effect-size justification. Supply rationale if the simulation is a graded component.
8. Standalone team means are fixed and traits generated separately; this omits real
   between-team mean variation and multivariate personality correlation structure.
9. Generated paired-team intervals quantify the assumed simulation, not evidence
   about real teams. The sensitivity exercise changes all trait SD targets together.

## Manual acceptance check (record actual outcomes in the pull request)

- Open a fresh copy using group-project.Rproj, install dependencies, restart R.
- Knit main.Rmd with run_simulation: true. Verify descriptive table, regression,
  observed plot, simulation table/plot, references, and session information appear.
- Verify both simulation scenarios report 500 teams, 2,000 participants, finite
  outcomes in 1–5, and the expected target/achieved diversity summaries.
- Run the manual app chunk. Check every dropdown selects available columns.
- Run comparison at small settings (e.g. 3 teams, 4 members, 100 repetitions), then
  default settings. Check plots and summary table render without errors.
- Run the same settings twice: results should match. Change effects, rerun, and
  check expected model-dependent changes. Check composition-only view too.
- Stop the app; confirm the main report can still Knit in a fresh R session.
- Compare app plot run averages to the strategy summary; do not require strategies
  to differ when the selected assumptions imply no meaningful difference.
- Have another member run the full instructions on their computer.

## Repository integration

This candidate should be compared with current GitHub files before merging.
It contains no .git directory, so it cannot replace repository history.
Do not submit duplicate old root-level R scripts alongside src versions: after
review, remove/move the obsolete copies using Git so there is one clear entry point.
Keep unrelated teammate changes. Exclude prototype backups from the submitted repo.
After a reviewer accepts the changes, merge the PR into main. Each person must also
complete a substantive review of another person's PR; no generated text substitutes
for actually running and reviewing that contribution.
