# Deterministic, lightweight checks for the shared simulation logic.
# Run from the project root with: source("sanity_checks.R")

source("simulation_core.R")

n_teams <- 4L
team_size <- 6L
trait <- seq_len(n_teams * team_size)

# A fixed, balanced-count random allocation makes the variance comparison
# reproducible while still being less extreme than the two sorted strategies.
fixed_random <- c(
  1, 1, 2, 2, 3, 3, 4, 4,
  2, 2, 3, 3, 4, 4, 1, 1,
  3, 3, 4, 4, 1, 1, 2, 2
)
assignments <- hexaco_assign_teams(
  person_group_trait=trait,
  n_teams=n_teams,
  team_size=team_size,
  random_team=fixed_random
)

# All three strategies must create exactly the requested team size.
for (strategy in names(assignments)) {
  counts <- tabulate(assignments[[strategy]], nbins=n_teams)
  stopifnot(length(assignments[[strategy]]) == n_teams * team_size)
  stopifnot(identical(as.integer(counts), rep.int(team_size, n_teams)))
}
