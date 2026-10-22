# ============================================================
# data_simulation.R
# Individual-level HEXACO -> teamwork-quality simulation
# ============================================================
# IMPORTANT:
# All effects and generated scores are hypothetical assumptions.
# They are not empirical findings, causal estimates, or predictions.
#
# Model:
#   Individual outcome = outcome-specific intercept
#     + within-team individual HEXACO contribution
#     + team-composition contribution from team-average HEXACO scores
#     + within-team diversity contribution
#     + correlated team-level random effects
#     + correlated individual residuals
# Team-level and individual-level outcome covariance are specified
# separately, allowing each outcome pair to have a different relationship.
#
# Teamwork outcomes are generated from latent scores and converted to
# ordinal 1-5 questionnaire responses using response thresholds. Subscale
# scores are the means of their scored items; overall TWQ is the mean of the
# six subscale scores. Thresholds and item noise are placeholders until
# raw questionnaire response data are available.
#
# Requires R >= 4.1 and packages: dplyr, tidyr, ggplot2
# ============================================================

if (getRversion() < "4.1.0") {
  stop("This script requires R >= 4.1.0 because it uses the native pipe |>.",
       call. = FALSE)
}

required_packages <- c("dplyr", "tidyr", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Missing required R package(s): ",
    paste(missing_packages, collapse = ", "),
    ". Install them with install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    ")).",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})
simulation_core_candidates <- c(
  "simulation_core.R",
  file.path("src", "simulation_core.R"),
  file.path("outputs", "simulation_core.R"),
  file.path("..", "src", "simulation_core.R")
)
simulation_core_path <- simulation_core_candidates[file.exists(simulation_core_candidates)]
if (!length(simulation_core_path)) {
  stop("Shared simulation_core.R was not found beside this script, in src/, or in outputs/.", call.=FALSE)
}
source(simulation_core_path[[1]], local=TRUE)
run_started_at <- Sys.time()

# ------------------------------------------------------------
# 1. Shared definitions and default assumptions
# ------------------------------------------------------------

hexaco_traits <- c(
  "honesty_humility", "emotionality", "extraversion",
  "agreeableness", "conscientiousness", "openness"
)

twq_outcomes <- c(
  "communication", "coordination", "balance",
  "support", "commitment", "cohesion"
)

all_outcomes <- c(twq_outcomes, "twq_overall")

# Item codes and reverse-keying follow the supplied TWQ questionnaire.
# Responses are stored as originally answered; *_scored columns apply
# reverse scoring (6 - response) where indicated.
twq_items <- list(
  communication = sprintf("CMNC%02d", 1:10),
  coordination = sprintf("CRDN%d", 1:4),
  balance = sprintf("BLNC%d", 1:3),
  support = sprintf("SPRT%d", 1:6),
  commitment = sprintf("COMT%d", 1:4),
  cohesion = sprintf("COHS%02d", 1:10)
)
twq_reverse_items <- c("CMNC04", "CMNC06", "CMNC07", "CRDN4",
                       "BLNC3", "COMT4", "COHS02", "COHS06")

# Ordinal measurement assumptions (latent scale centered near 3).
# Change thresholds to match observed category frequencies when available.
twq_response_thresholds <- c(-1.20, -0.40, 0.40, 1.20)
twq_item_noise_sd <- 0.70

# Placeholder outcome correlations, expressed through different latent
# common-factor loadings at the team and individual levels. These create
# stronger relationships for conceptually closer domains. Replace with
# estimates from real data when available. Outcome order follows twq_outcomes.
team_outcome_loadings <- c(
  communication = 0.72, coordination = 0.78, balance = 0.55,
  support = 0.70, commitment = 0.62, cohesion = 0.80
)
individual_residual_loadings <- c(
  communication = 0.56, coordination = 0.63, balance = 0.42,
  support = 0.60, commitment = 0.50, cohesion = 0.66
)
make_loading_correlation <- function(loadings) {
  correlation <- tcrossprod(loadings)
  diag(correlation) <- 1
  dimnames(correlation) <- list(names(loadings), names(loadings))
  correlation
}
team_outcome_correlation_matrix <- make_loading_correlation(team_outcome_loadings)
individual_residual_correlation_matrix <- make_loading_correlation(
  individual_residual_loadings
)

trait_labels <- c(
  honesty_humility = "Honesty–Humility",
  emotionality = "Emotionality",
  extraversion = "Extraversion",
  agreeableness = "Agreeableness",
  conscientiousness = "Conscientiousness",
  openness = "Openness"
)

outcome_labels <- c(
  communication = "Communication",
  coordination = "Coordination",
  balance = "Balance of contributions",
  support = "Mutual support",
  commitment = "Commitment",
  cohesion = "Cohesion",
  twq_overall = "Overall teamwork quality"
)

# Population reference values used to standardize individual traits.
# Effects are interpreted as score-point change per 1 reference SD.
trait_reference_means <- setNames(rep(3, length(hexaco_traits)), hexaco_traits)
trait_reference_sds <- setNames(rep(0.7, length(hexaco_traits)), hexaco_traits)

# Temporary placeholder correlations between HEXACO traits. These values are
# taken from the community sample in Ashton & Lee (2009), Table 3 (N = 734),
# not from the real dataset for this project. Replace this matrix when
# project-specific estimates are available. Rows and columns must follow
# hexaco_traits order.
hexaco_correlation_matrix <- matrix(
  c(
    1.00,  0.04, -0.09,  0.25,  0.13, -0.03,
    0.04,  1.00, -0.07, -0.04, -0.06, -0.08,
   -0.09, -0.07,  1.00, -0.11, -0.13,  0.00,
    0.25, -0.04, -0.11,  1.00,  0.05, -0.05,
    0.13, -0.06, -0.13,  0.05,  1.00,  0.09,
   -0.03, -0.08,  0.00, -0.05,  0.09,  1.00
  ),
  nrow = length(hexaco_traits),
  byrow = TRUE,
  dimnames = list(hexaco_traits, hexaco_traits)
)

# Illustrative direct individual-level effects (outcome score points
# per 1 reference SD of the named trait). These are deliberately
# non-zero so changing the assumptions changes the model output.
# Replace these values when a justified theoretical specification exists.
default_trait_effects <- list(
  communication = c(
    honesty_humility = 0.08, emotionality = -0.03, extraversion = 0.12,
    agreeableness = 0.18, conscientiousness = 0.08, openness = 0.05
  ),
  coordination = c(
    honesty_humility = 0.05, emotionality = -0.02, extraversion = 0.05,
    agreeableness = 0.08, conscientiousness = 0.20, openness = 0.07
  ),
  balance = c(
    honesty_humility = 0.08, emotionality = -0.02, extraversion = 0.02,
    agreeableness = 0.12, conscientiousness = 0.12, openness = 0.04
  ),
  support = c(
    honesty_humility = 0.15, emotionality = 0.00, extraversion = 0.04,
    agreeableness = 0.20, conscientiousness = 0.06, openness = 0.03
  ),
  commitment = c(
    honesty_humility = 0.08, emotionality = -0.02, extraversion = 0.03,
    agreeableness = 0.05, conscientiousness = 0.20, openness = 0.04
  ),
  cohesion = c(
    honesty_humility = 0.08, emotionality = -0.02, extraversion = 0.10,
    agreeableness = 0.16, conscientiousness = 0.07, openness = 0.07
  )
)

# Initially match the individual coefficients so the additive split preserves
# the previous total trait contribution. Change these independently to model
# contextual effects of a team's average personality profile.
default_composition_effects <- lapply(default_trait_effects, identity)

# Illustrative diversity effects per 1.0 raw-unit increase in within-team SD.
# Store coefficients in a tidy outcome-by-trait table for easy inspection.
default_diversity_effects_table <- tidyr::expand_grid(
  outcome = twq_outcomes,
  trait = hexaco_traits
) |>
  mutate(
    coefficient = dplyr::case_when(
      outcome == "communication" & trait == "agreeableness" ~ -0.20,
      outcome == "coordination" & trait == "agreeableness" ~ -0.10,
      outcome == "support" & trait == "agreeableness" ~ -0.15,
      outcome == "commitment" & trait == "agreeableness" ~ 0.05,
      outcome == "cohesion" & trait == "agreeableness" ~ -0.10,
      TRUE ~ 0.00
    )
  )

diversity_effect_table_to_list <- function(effect_table) {
  expected_pairs <- tidyr::expand_grid(outcome = twq_outcomes, trait = hexaco_traits)
  if (!all(c("outcome", "trait", "coefficient") %in% names(effect_table)) ||
      !is.numeric(effect_table$coefficient) ||
      anyDuplicated(effect_table[c("outcome", "trait")]) ||
      nrow(effect_table) != nrow(expected_pairs) ||
      !all(expected_pairs$outcome %in% effect_table$outcome) ||
      !all(expected_pairs$trait %in% effect_table$trait) ||
      any(!effect_table$outcome %in% twq_outcomes) ||
      any(!effect_table$trait %in% hexaco_traits) ||
      any(!is.finite(effect_table$coefficient))) {
    stop("Diversity effect table must contain one finite coefficient for every outcome-trait pair.",
         call. = FALSE)
  }
  setNames(lapply(twq_outcomes, function(y) {
    rows <- effect_table[effect_table$outcome == y, , drop = FALSE]
    values <- rows$coefficient[match(hexaco_traits, rows$trait)]
    setNames(values, hexaco_traits)
  }), twq_outcomes)
}
default_diversity_effects <- diversity_effect_table_to_list(
  default_diversity_effects_table
)

outcome_intercepts <- setNames(rep(3, length(twq_outcomes)), twq_outcomes)
default_diversity_reference <- setNames(rep(0.30, length(hexaco_traits)), hexaco_traits)

# Central configuration for the complete run. Edit assumptions here so the
# run metadata and manifest can record the exact settings used.
simulation_config <- list(
  design = list(
    n_teams = 500L,
    team_size = 4L,
    low_target_sd = 0.20,
    high_target_sd = 0.80
  ),
  population = list(
    trait_means = trait_reference_means,
    trait_reference_sds = trait_reference_sds,
    hexaco_correlation = hexaco_correlation_matrix,
    team_mean_sd = 0.35
  ),
  effects = list(
    individual = default_trait_effects,
    composition = default_composition_effects,
    diversity_table = default_diversity_effects_table,
    intercepts = outcome_intercepts,
    diversity_reference = default_diversity_reference
  ),
  outcome = list(
    team_sd = 0.25,
    residual_sd = 0.45,
    team_sds = setNames(rep(0.25, length(twq_outcomes)), twq_outcomes),
    residual_sds = setNames(rep(0.45, length(twq_outcomes)), twq_outcomes),
    team_correlations = team_outcome_correlation_matrix,
    residual_correlations = individual_residual_correlation_matrix,
    item_noise_sd = twq_item_noise_sd,
    response_thresholds = twq_response_thresholds
  ),
  randomness = list(
    team_generation_seed = 101L,
    outcome_seed = 202L,
    sensitivity_team_seed = 606L,
    sensitivity_outcome_seed = 707L,
    dose_response_team_seed = 808L,
    dose_response_outcome_seed = 909L
  ),
  dose_response = list(
    target_sds = seq(0.10, 0.90, by = 0.10),
    teams_per_level = 100L
  ),
  sensitivity = list(teams_per_condition = 100L),
  export = list(directory = "simulated_data")
)

# Named aliases keep the simulation functions readable; simulation_config is
# their single source of truth.
n_teams <- simulation_config$design$n_teams
team_size <- simulation_config$design$team_size
low_target_sd <- simulation_config$design$low_target_sd
high_target_sd <- simulation_config$design$high_target_sd
trait_reference_means <- simulation_config$population$trait_means
trait_reference_sds <- simulation_config$population$trait_reference_sds
hexaco_correlation_matrix <- simulation_config$population$hexaco_correlation
team_mean_sd <- simulation_config$population$team_mean_sd
default_trait_effects <- simulation_config$effects$individual
default_composition_effects <- simulation_config$effects$composition
default_diversity_effects_table <- simulation_config$effects$diversity_table
default_diversity_effects <- diversity_effect_table_to_list(
  default_diversity_effects_table
)
outcome_intercepts <- simulation_config$effects$intercepts
default_diversity_reference <- simulation_config$effects$diversity_reference
scenario_seed <- simulation_config$randomness$team_generation_seed
outcome_seed <- simulation_config$randomness$outcome_seed
team_random_sd <- simulation_config$outcome$team_sd
individual_noise_sd <- simulation_config$outcome$residual_sd
team_outcome_correlation_matrix <- simulation_config$outcome$team_correlations
individual_residual_correlation_matrix <-
  simulation_config$outcome$residual_correlations
team_outcome_sds <- simulation_config$outcome$team_sds
individual_residual_sds <- simulation_config$outcome$residual_sds
output_dir <- simulation_config$export$directory

# ------------------------------------------------------------
# 2. Generate teams with bounded, realistic within-team variation
# ------------------------------------------------------------

generate_diverse_teams <- function(
    n_teams = simulation_config$design$n_teams,
    team_size = simulation_config$design$team_size,
    trait_means = simulation_config$population$trait_means,
    trait_correlation = simulation_config$population$hexaco_correlation,
    target_diversity = setNames(
      rep(simulation_config$design$low_target_sd, length(hexaco_traits)), hexaco_traits
    ),
    team_mean_sd = simulation_config$population$team_mean_sd,
    seed = simulation_config$randomness$team_generation_seed,
    scenario = "Unspecified"
) {
  validate_integer <- function(x, label, min_value = 1) {
    if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
        !is.finite(x) || x < min_value || x != floor(x) ||
        x > .Machine$integer.max) {
      stop(
        label, " must be a whole number between ", min_value,
        " and ", .Machine$integer.max, ".",
        call. = FALSE
      )
    }
  }
  validate_integer(n_teams, "n_teams")
  validate_integer(team_size, "team_size", min_value = 2)
  if (!is.numeric(team_mean_sd) || length(team_mean_sd) != 1L ||
      is.na(team_mean_sd) || !is.finite(team_mean_sd) || team_mean_sd < 0) {
    stop("team_mean_sd must be one finite non-negative numeric value.",
         call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
      !is.finite(seed) || seed < 0 || seed != floor(seed) ||
      seed > .Machine$integer.max) {
    stop(
      "seed must be one non-negative whole number within the supported integer range.",
      call. = FALSE
    )
  }

  if (is.null(names(trait_means)) ||
      !all(hexaco_traits %in% names(trait_means))) {
    stop("trait_means must be named for all six HEXACO traits.", call. = FALSE)
  }
  if (is.null(names(target_diversity)) ||
      !all(hexaco_traits %in% names(target_diversity))) {
    stop("target_diversity must be named for all six HEXACO traits.", call. = FALSE)
  }

  trait_means <- trait_means[hexaco_traits]
  target_diversity <- target_diversity[hexaco_traits]

  if (!is.numeric(trait_means) ||
      any(!is.finite(trait_means)) ||
      any(trait_means < 1 | trait_means > 5)) {
    stop("Every trait mean must be numeric, finite, and within [1, 5].",
         call. = FALSE)
  }
  if (!is.numeric(target_diversity) ||
      any(!is.finite(target_diversity)) ||
      any(target_diversity < 0)) {
    stop("Every target diversity must be numeric, finite, and non-negative.",
         call. = FALSE)
  }
  if (!is.matrix(trait_correlation) ||
      !identical(dim(trait_correlation), c(length(hexaco_traits), length(hexaco_traits))) ||
      !identical(rownames(trait_correlation), hexaco_traits) ||
      !identical(colnames(trait_correlation), hexaco_traits) ||
      !is.numeric(trait_correlation) || any(!is.finite(trait_correlation)) ||
      any(abs(trait_correlation - t(trait_correlation)) > 1e-8) ||
      any(abs(diag(trait_correlation) - 1) > 1e-8)) {
    stop("trait_correlation must be a finite, symmetric 6x6 correlation matrix named in HEXACO trait order.",
         call. = FALSE)
  }
  trait_cholesky <- tryCatch(chol(trait_correlation), error = function(e) NULL)
  if (is.null(trait_cholesky)) {
    stop("trait_correlation must be positive definite.", call. = FALSE)
  }

  set.seed(seed)
  n_people <- n_teams * team_size
  df <- data.frame(
    Scenario = scenario,
    Team = rep(sprintf("T%04d", seq_len(n_teams)), each = team_size),
    member = rep(seq_len(team_size), times = n_teams),
    stringsAsFactors = FALSE
  )

  # Draw correlated team-specific centers and correlated member profiles.
  # Member patterns are centered and standardized within each team; their
  # scales are then set to each trait's requested within-team SD when feasible.
  team_rows <- split(seq_len(n_people), df$Team)

  correlated_normals <- function(n) {
    matrix(stats::rnorm(n * length(hexaco_traits)), nrow = n) %*% trait_cholesky
  }

  team_center_z <- correlated_normals(n_teams)
  team_center_values <- matrix(
    NA_real_, nrow = n_teams, ncol = length(hexaco_traits),
    dimnames = list(NULL, hexaco_traits)
  )
  for (j in seq_along(hexaco_traits)) {
    mu <- unname(trait_means[[hexaco_traits[[j]]]])
    if (team_mean_sd == 0) {
      team_center_values[, j] <- mu
    } else {
      # Convert correlated normal draws to truncated-normal marginals.
      # This avoids the artificial pile-up at 1 and 5 caused by clipping.
      lower_probability <- stats::pnorm((1 - mu) / team_mean_sd)
      upper_probability <- stats::pnorm((5 - mu) / team_mean_sd)
      probabilities <- lower_probability +
        stats::pnorm(team_center_z[, j]) *
        (upper_probability - lower_probability)
      probabilities <- pmin(1 - .Machine$double.eps,
                            pmax(.Machine$double.eps, probabilities))
      team_center_values[, j] <- mu + team_mean_sd * stats::qnorm(probabilities)
      # Protect against tiny floating-point excursions at the endpoints.
      team_center_values[, j] <- pmin(5, pmax(1, team_center_values[, j]))
    }
  }

  within_patterns <- matrix(
    NA_real_, nrow = n_people, ncol = length(hexaco_traits),
    dimnames = list(NULL, hexaco_traits)
  )
  for (team_index in seq_along(team_rows)) {
    ii <- team_rows[[team_index]]
    pattern <- correlated_normals(length(ii))
    pattern <- sweep(pattern, 2, colMeans(pattern), "-")
    for (j in seq_along(hexaco_traits)) {
      pattern_sd <- stats::sd(pattern[, j])
      if (!is.finite(pattern_sd) || pattern_sd < .Machine$double.eps) {
        pattern[, j] <- seq_along(ii) - mean(seq_along(ii))
        pattern_sd <- stats::sd(pattern[, j])
      }
      pattern[, j] <- pattern[, j] / pattern_sd
    }
    within_patterns[ii, ] <- pattern
  }

  for (trait_index in seq_along(hexaco_traits)) {
    tr <- hexaco_traits[[trait_index]]
    scores <- numeric(n_people)
    mu <- unname(trait_means[[tr]])
    target_sd <- unname(target_diversity[[tr]])

    for (team_index in seq_along(team_rows)) {
      ii <- team_rows[[team_index]]
      n <- length(ii)
      team_mu <- team_center_values[team_index, trait_index]

      if (target_sd == 0 || team_mu == 1 || team_mu == 5) {
        scores[ii] <- rep(team_mu, n)
        next
      }

      z <- within_patterns[ii, tr]

      z_hi <- max(z)
      z_lo <- min(z)
      max_scale_upper <- if (z_hi > 0) (5 - team_mu) / z_hi else Inf
      max_scale_lower <- if (z_lo < 0) (team_mu - 1) / abs(z_lo) else Inf
      feasible_sd <- max(0, min(target_sd, max_scale_upper, max_scale_lower))
      # Clamp after arithmetic as well, to absorb floating-point rounding at
      # the boundaries while guaranteeing valid 1-to-5 trait scores.
      scores[ii] <- pmin(5, pmax(1, team_mu + feasible_sd * z))
    }

    df[[tr]] <- scores
  }

  df
}

# ------------------------------------------------------------
# 3. Validate team data and report achieved diversity
# ------------------------------------------------------------

validate_team_data <- function(data, expected_teams = NULL, expected_size = NULL) {
  required <- c("Scenario", "Team", "member", hexaco_traits)
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("Team data missing required columns: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (nrow(data) == 0L || anyNA(data$Scenario) || anyNA(data$Team) ||
      anyNA(data$member) ||
      any(!nzchar(trimws(as.character(data$Scenario)))) ||
      any(!nzchar(trimws(as.character(data$Team)))) ||
      any(!nzchar(trimws(as.character(data$member)))) ||
      (is.numeric(data$Scenario) && any(!is.finite(data$Scenario))) ||
      (is.numeric(data$Team) && any(!is.finite(data$Team))) ||
      (is.numeric(data$member) && any(!is.finite(data$member)))) {
    stop("Team data must have non-empty Scenario, Team, and member identifiers.",
         call. = FALSE)
  }
  if (anyDuplicated(data[c("Scenario", "Team", "member")])) {
    stop("Duplicate Team/member identifiers found.", call. = FALSE)
  }
  x <- as.matrix(data[hexaco_traits])
  if (!all(vapply(as.data.frame(x), is.numeric, logical(1))) ||
      any(!is.finite(x)) || any(x < 1 | x > 5)) {
    stop("All HEXACO scores must be numeric, finite, and within [1, 5].",
         call. = FALSE)
  }

  sizes <- data |> count(Scenario, Team, name = "team_size")
  if (any(sizes$team_size < 2L)) {
    stop("Every team needs at least two members to calculate sample SD.",
         call. = FALSE)
  }
  teams_per_scenario <- sizes |> count(Scenario, name = "n_teams")
  if (length(unique(teams_per_scenario$n_teams)) != 1L) {
    stop("Every scenario must contain the same number of teams.", call. = FALSE)
  }
  if (!is.null(expected_teams) && any(teams_per_scenario$n_teams != expected_teams)) {
    stop("Expected ", expected_teams, " teams in each scenario; found: ",
         paste(teams_per_scenario$n_teams, collapse = ", "), ".", call. = FALSE)
  }
  if (!is.null(expected_size) && any(sizes$team_size != expected_size)) {
    stop("Not every team has the expected size of ", expected_size, ".",
         call. = FALSE)
  }
  invisible(TRUE)
}

calculate_team_diversity <- function(data) {
  validate_team_data(data)
  mean_pairwise_abs_difference <- function(x) {
    differences <- abs(outer(x, x, "-"))
    mean(differences[upper.tri(differences)])
  }

  data |>
    group_by(Scenario, Team) |>
    summarise(
      team_size = dplyr::n(),
      across(
        all_of(hexaco_traits),
        list(
          sd = ~ stats::sd(.x),
          score_range = ~ diff(range(.x)),
          mean_pairwise_abs_difference = ~ mean_pairwise_abs_difference(.x)
        ),
        .names = "{.col}__{.fn}"
      ),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = matches("__(sd|score_range|mean_pairwise_abs_difference)$"),
      names_to = c("trait", ".value"),
      names_pattern = "^(.*)__(sd|score_range|mean_pairwise_abs_difference)$"
    ) |>
    mutate(
      trait = factor(trait, levels = hexaco_traits),
      trait_label = unname(trait_labels[as.character(trait)]),
      within_team_sd = sd,
      standardized_within_team_sd = within_team_sd /
        unname(trait_reference_sds[as.character(trait)])
    )
}

calculate_multivariate_team_diversity <- function(data) {
  validate_team_data(data)
  team_groups <- data |>
    group_by(Scenario, Team) |>
    group_split(.keep = TRUE)

  bind_rows(lapply(team_groups, function(team_data) {
    standardized_traits <- sweep(
      as.matrix(team_data[hexaco_traits]),
      2,
      trait_reference_sds[hexaco_traits],
      "/"
    )
    pairwise_distances <- as.numeric(stats::dist(standardized_traits))

    data.frame(
      Scenario = team_data$Scenario[[1]],
      Team = team_data$Team[[1]],
      team_size = nrow(team_data),
      mean_pairwise_standardized_distance = mean(pairwise_distances),
      median_pairwise_standardized_distance = stats::median(pairwise_distances),
      max_pairwise_standardized_distance = max(pairwise_distances),
      stringsAsFactors = FALSE
    )
  }))
}

# ------------------------------------------------------------
# 4. Simulate individual-level teamwork outcomes
# ------------------------------------------------------------

simulate_teamwork_with_diversity <- function(
    teams_data,
    trait_effects = NULL,
    composition_effects = NULL,
    diversity_effects = NULL,
    intercepts = simulation_config$effects$intercepts,
    diversity_reference = NULL,
    trait_reference_means = NULL,
    trait_reference_sds = NULL,
    team_sd = simulation_config$outcome$team_sd,
    noise_sd = simulation_config$outcome$residual_sd,
    team_correlations = simulation_config$outcome$team_correlations,
    residual_correlations = simulation_config$outcome$residual_correlations,
    team_sds = simulation_config$outcome$team_sds,
    residual_sds = simulation_config$outcome$residual_sds,
    item_noise_sd = simulation_config$outcome$item_noise_sd,
    response_thresholds = simulation_config$outcome$response_thresholds,
    seed = simulation_config$randomness$outcome_seed
) {
  validate_team_data(teams_data)

  if (is.null(trait_effects)) trait_effects <- default_trait_effects
  if (is.null(composition_effects)) composition_effects <- default_composition_effects
  if (is.null(diversity_effects)) diversity_effects <- default_diversity_effects
  if (is.null(diversity_reference)) diversity_reference <- default_diversity_reference
  if (is.null(trait_reference_means)) {
    trait_reference_means <- setNames(rep(3, length(hexaco_traits)), hexaco_traits)
  }
  if (is.null(trait_reference_sds)) {
    trait_reference_sds <- setNames(rep(0.7, length(hexaco_traits)), hexaco_traits)
  }

  scalar_nonnegative <- function(x, label) {
    if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
        !is.finite(x) || x < 0) {
      stop(label, " must be one finite non-negative numeric value.",
           call. = FALSE)
    }
  }
  scalar_nonnegative(team_sd, "team_sd")
  scalar_nonnegative(noise_sd, "noise_sd")
  scalar_nonnegative(item_noise_sd, "item_noise_sd")
  validate_outcome_correlation <- function(x, label) {
    if (!is.matrix(x) ||
        !identical(dim(x), c(length(twq_outcomes), length(twq_outcomes))) ||
        !identical(rownames(x), twq_outcomes) ||
        !identical(colnames(x), twq_outcomes) || !is.numeric(x) ||
        any(!is.finite(x)) || any(abs(x - t(x)) > 1e-8) ||
        any(abs(diag(x) - 1) > 1e-8) || any(abs(x) > 1)) {
      stop(label, " must be a named, finite 6x6 correlation matrix in twq_outcomes order.",
           call. = FALSE)
    }
    if (is.null(tryCatch(chol(x), error = function(e) NULL))) {
      stop(label, " must be positive definite.", call. = FALSE)
    }
    x
  }
  team_correlations <- validate_outcome_correlation(
    team_correlations, "team_correlations"
  )
  residual_correlations <- validate_outcome_correlation(
    residual_correlations, "residual_correlations"
  )
  if (!is.numeric(response_thresholds) || length(response_thresholds) != 4L ||
      any(!is.finite(response_thresholds)) ||
      any(diff(response_thresholds) <= 0)) {
    stop("response_thresholds must contain four finite, strictly increasing values.",
         call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
      !is.finite(seed) || seed < 0 || seed != floor(seed) ||
      seed > .Machine$integer.max) {
    stop(
      "seed must be one non-negative whole number within the supported integer range.",
      call. = FALSE
    )
  }

  validate_named_vector <- function(x, expected, label, nonnegative = FALSE) {
    if (is.null(names(x)) || anyDuplicated(names(x)) ||
        !setequal(names(x), expected)) {
      stop(label, " must be named exactly for: ", paste(expected, collapse = ", "),
           call. = FALSE)
    }
    x <- x[expected]
    if (!is.numeric(x) || any(!is.finite(x)) ||
        (nonnegative && any(x < 0))) {
      stop(label, " must contain numeric, finite values",
           if (nonnegative) " that are non-negative." else ".",
           call. = FALSE)
    }
    x
  }
  if (is.null(team_sds)) {
    team_sds <- setNames(rep(team_sd, length(twq_outcomes)), twq_outcomes)
  }
  if (is.null(residual_sds)) {
    residual_sds <- setNames(rep(noise_sd, length(twq_outcomes)), twq_outcomes)
  }
  team_sds <- validate_named_vector(
    team_sds, twq_outcomes, "team_sds", nonnegative = TRUE
  )
  residual_sds <- validate_named_vector(
    residual_sds, twq_outcomes, "residual_sds", nonnegative = TRUE
  )
  intercepts <- validate_named_vector(intercepts, twq_outcomes, "intercepts")
  diversity_reference <- validate_named_vector(
    diversity_reference, hexaco_traits, "diversity_reference", nonnegative = TRUE
  )
  trait_reference_means <- validate_named_vector(
    trait_reference_means, hexaco_traits, "trait_reference_means"
  )
  trait_reference_sds <- validate_named_vector(
    trait_reference_sds, hexaco_traits, "trait_reference_sds", nonnegative = TRUE
  )
  if (any(trait_reference_sds <= 0)) {
    stop("trait_reference_sds must be strictly positive.", call. = FALSE)
  }

  latent_scores <- setNames(vector("list", length(twq_outcomes)), twq_outcomes)
  for (y in twq_outcomes) {
    if (is.null(trait_effects[[y]]) || is.null(composition_effects[[y]]) ||
        is.null(diversity_effects[[y]])) {
      stop("Missing individual, composition, or diversity effect vector for outcome: ",
           y, call. = FALSE)
    }
    trait_effects[[y]] <- validate_named_vector(
      trait_effects[[y]], hexaco_traits, paste("trait_effects$", y)
    )
    composition_effects[[y]] <- validate_named_vector(
      composition_effects[[y]], hexaco_traits, paste("composition_effects$", y)
    )
    diversity_effects[[y]] <- validate_named_vector(
      diversity_effects[[y]], hexaco_traits, paste("diversity_effects$", y)
    )
  }

  set.seed(seed)
  df <- teams_data
  df$.original_row <- seq_len(nrow(df))
  # Use the shared component engine also used by the Shiny what-if comparison.
  # Group by scenario and team so IDs may safely repeat across scenarios.
  team_group_ids <- interaction(as.character(df$Scenario), as.character(df$Team),
                                drop=TRUE, lex.order=TRUE)
  trait_components <- hexaco_team_trait_components(
    people=df,
    team_ids=team_group_ids,
    traits=hexaco_traits,
    trait_reference_means=trait_reference_means,
    trait_reference_sds=trait_reference_sds,
    diversity_reference_means=diversity_reference,
    diversity_reference_sds=setNames(rep(1, length(hexaco_traits)), hexaco_traits)
  )
  for (tr in hexaco_traits) {
    df[[paste0("team_mean_", tr)]] <- trait_components$person_team_means[, tr]
    df[[paste0("team_sd_", tr)]] <- trait_components$person_team_sds[, tr]
  }
  z_team_means <- trait_components$person_team_mean_z
  z_individual_deviations <- trait_components$individual_z
  team_ids <- trait_components$group_levels
  team_index <- trait_components$group_index

  # Draw separate correlated random-effect vectors at team and individual
  # levels. Their covariance matrices combine the configured correlations
  # with outcome-specific marginal standard deviations.
  team_covariance <- diag(team_sds) %*% team_correlations %*% diag(team_sds)
  residual_covariance <- diag(residual_sds) %*% residual_correlations %*%
    diag(residual_sds)
  team_cholesky <- chol(
    team_covariance + diag(.Machine$double.eps, length(twq_outcomes))
  )
  residual_cholesky <- chol(
    residual_covariance + diag(.Machine$double.eps, length(twq_outcomes))
  )
  team_effect_matrix <- matrix(
    stats::rnorm(length(team_ids) * length(twq_outcomes)),
    nrow = length(team_ids), ncol = length(twq_outcomes)
  ) %*% team_cholesky
  residual_matrix <- matrix(
    stats::rnorm(nrow(df) * length(twq_outcomes)),
    nrow = nrow(df), ncol = length(twq_outcomes)
  ) %*% residual_cholesky
  colnames(team_effect_matrix) <- colnames(residual_matrix) <- twq_outcomes

  for (y in twq_outcomes) {
    beta <- trait_effects[[y]][hexaco_traits]
    beta_composition <- composition_effects[[y]][hexaco_traits]
    beta_div <- diversity_effects[[y]][hexaco_traits]

    individual_component <- as.numeric(z_individual_deviations %*% beta)
    composition_component <- as.numeric(z_team_means %*% beta_composition)
    diversity_component <- as.numeric(
      trait_components$person_diversity_z %*% beta_div
    )

    team_component <- team_effect_matrix[team_index, y]
    residual <- residual_matrix[, y]

    raw_score <- hexaco_compose_latent_score(
      intercept=intercepts[[y]],
      individual_component=individual_component,
      composition_component=composition_component,
      diversity_component=diversity_component,
      team_component=team_component,
      residual=residual
    )

    # Keep the unbounded latent score for diagnostics. It is transformed
    # into item-level ordinal responses below, rather than clipped to [1, 5].
    latent_scores[[y]] <- raw_score
    df[[paste0("latent_", y)]] <- raw_score
    df[[paste0("individual_component_", y)]] <- individual_component
    df[[paste0("composition_component_", y)]] <- composition_component
    df[[paste0("diversity_component_", y)]] <- diversity_component
    df[[paste0("team_component_", y)]] <- team_component
    df[[paste0("residual_", y)]] <- residual
  }

  # Generate Likert item responses from each domain's latent score. The
  # response categories are 1-5 with configurable thresholds and item noise.
  for (y in twq_outcomes) {
    scored_items <- character(length(twq_items[[y]]))
    for (j in seq_along(twq_items[[y]])) {
      item <- twq_items[[y]][[j]]
      # Generate reverse-keyed statements in the opposite raw-response
      # direction, then reverse-score them below. After scoring, every item
      # points in the same direction as its domain's latent teamwork score.
      item_direction <- if (item %in% twq_reverse_items) -1 else 1
      item_latent <- item_direction * (latent_scores[[y]] - 3) +
        stats::rnorm(nrow(df), 0, item_noise_sd)
      response <- 1L + rowSums(outer(item_latent, response_thresholds, ">"))
      df[[item]] <- as.integer(response)
      scored_name <- paste0(item, "_scored")
      df[[scored_name]] <- if (item %in% twq_reverse_items) 6L - response else response
      scored_items[[j]] <- scored_name
    }
    df[[y]] <- rowMeans(df[scored_items])
  }

  df$twq_overall <- rowMeans(df[twq_outcomes])
  df$.original_row <- NULL

  item_columns <- unlist(twq_items, use.names = FALSE)
  scored_item_columns <- paste0(item_columns, "_scored")
  item_matrix <- as.matrix(df[c(item_columns, scored_item_columns)])
  if (any(!is.finite(item_matrix)) || any(item_matrix < 1 | item_matrix > 5) ||
      any(item_matrix != floor(item_matrix))) {
    stop("Generated TWQ item responses must be integer values from 1 to 5.",
         call. = FALSE)
  }
  outcome_matrix <- as.matrix(df[all_outcomes])
  if (any(!is.finite(outcome_matrix)) || any(outcome_matrix < 1 | outcome_matrix > 5)) {
    stop("Outcome validation failed: scores must be finite and within [1, 5].",
         call. = FALSE)
  }

  df
}

# ------------------------------------------------------------
# 5. Create paired low- and high-diversity scenarios
# ------------------------------------------------------------

# Defaults: 500 teams x 4 members = 2,000 participant rows per scenario.
# Paired low/high scenarios intentionally reuse the generation and outcome
# seeds, so the random draws align while the diversity target changes.

low_diversity <- generate_diverse_teams(
  n_teams = n_teams,
  team_size = team_size,
  trait_means = trait_reference_means,
  target_diversity = setNames(rep(low_target_sd, length(hexaco_traits)), hexaco_traits),
  team_mean_sd = team_mean_sd,
  seed = scenario_seed,
  scenario = "Low diversity"
)

high_diversity <- generate_diverse_teams(
  n_teams = n_teams,
  team_size = team_size,
  trait_means = trait_reference_means,
  target_diversity = setNames(rep(high_target_sd, length(hexaco_traits)), hexaco_traits),
  team_mean_sd = team_mean_sd,
  seed = scenario_seed,
  scenario = "High diversity"
)

validate_team_data(low_diversity, expected_teams = n_teams, expected_size = team_size)
validate_team_data(high_diversity, expected_teams = n_teams, expected_size = team_size)
if (!all(low_diversity$Scenario == "Low diversity") ||
    !all(high_diversity$Scenario == "High diversity") ||
    !identical(low_diversity$Team, high_diversity$Team) ||
    !identical(low_diversity$member, high_diversity$member)) {
  stop("Paired scenarios must have matching team/member IDs and the expected scenario labels.",
       call. = FALSE)
}

# Reusing the outcome seed across low/high conditions pairs team effects,
# residuals, and item-noise draws by participant. Coefficients remain hypothetical.
low_results <- simulate_teamwork_with_diversity(
  low_diversity,
  trait_effects = default_trait_effects,
  composition_effects = default_composition_effects,
  diversity_effects = default_diversity_effects,
  intercepts = outcome_intercepts,
  diversity_reference = default_diversity_reference,
  team_sd = team_random_sd,
  noise_sd = individual_noise_sd,
  team_correlations = team_outcome_correlation_matrix,
  residual_correlations = individual_residual_correlation_matrix,
  team_sds = team_outcome_sds,
  residual_sds = individual_residual_sds,
  seed = outcome_seed
)

high_results <- simulate_teamwork_with_diversity(
  high_diversity,
  trait_effects = default_trait_effects,
  composition_effects = default_composition_effects,
  diversity_effects = default_diversity_effects,
  intercepts = outcome_intercepts,
  diversity_reference = default_diversity_reference,
  team_sd = team_random_sd,
  noise_sd = individual_noise_sd,
  team_correlations = team_outcome_correlation_matrix,
  residual_correlations = individual_residual_correlation_matrix,
  team_sds = team_outcome_sds,
  residual_sds = individual_residual_sds,
  seed = outcome_seed
)

combined_results <- bind_rows(low_results, high_results)

# Compare the separate covariance assumptions with realized simulation
# correlations at team, individual-residual, latent-score, and scored-scale levels.
correlation_to_long <- function(correlation, scenario, level) {
  values <- as.data.frame(as.table(correlation))
  names(values) <- c("outcome_1", "outcome_2", "correlation")
  values$Scenario <- scenario
  values$level <- level
  values
}
outcome_correlation_diagnostics <- bind_rows(lapply(
  unique(combined_results$Scenario),
  function(scenario_name) {
    scenario_data <- combined_results |>
      filter(Scenario == scenario_name)
    team_data <- scenario_data |>
      group_by(Team) |>
      summarise(across(starts_with("team_component_"), first), .groups = "drop") |>
      select(all_of(paste0("team_component_", twq_outcomes)))
    residual_data <- scenario_data |>
      select(all_of(paste0("residual_", twq_outcomes)))
    latent_data <- scenario_data |>
      select(all_of(paste0("latent_", twq_outcomes)))
    scored_data <- scenario_data |> select(all_of(twq_outcomes))
    names(team_data) <- names(residual_data) <- names(latent_data) <- twq_outcomes
    bind_rows(
      correlation_to_long(stats::cor(team_data), scenario_name, "Team effects"),
      correlation_to_long(stats::cor(residual_data), scenario_name, "Individual residuals"),
      correlation_to_long(stats::cor(latent_data), scenario_name, "Latent outcomes"),
      correlation_to_long(stats::cor(scored_data), scenario_name, "Ordinal subscale scores")
    )
  }
)) |>
  select(Scenario, level, outcome_1, outcome_2, correlation)

outcome_correlation_assumptions <- bind_rows(
  correlation_to_long(team_outcome_correlation_matrix, "Configured", "Team effects"),
  correlation_to_long(individual_residual_correlation_matrix,
                       "Configured", "Individual residuals")
) |>
  select(level, outcome_1, outcome_2, correlation)

outcome_sd_assumptions <- data.frame(
  outcome = twq_outcomes,
  team_effect_sd = unname(team_outcome_sds[twq_outcomes]),
  individual_residual_sd = unname(individual_residual_sds[twq_outcomes]),
  stringsAsFactors = FALSE
)

# Check generated marginal distributions and quantify any boundary scores.
trait_score_diagnostics <- combined_results |>
  select(Scenario, all_of(hexaco_traits)) |>
  pivot_longer(
    cols = all_of(hexaco_traits),
    names_to = "trait",
    values_to = "score"
  ) |>
  group_by(Scenario, trait) |>
  summarise(
    n_scores = dplyr::n(),
    mean_score = mean(score),
    sd_score = stats::sd(score),
    min_score = min(score),
    max_score = max(score),
    proportion_at_lower_bound = mean(score <= 1 + 1e-8),
    proportion_at_upper_bound = mean(score >= 5 - 1e-8),
    .groups = "drop"
  ) |>
  mutate(
    trait_label = unname(trait_labels[trait]),
    across(where(is.numeric), ~ round(.x, 4))
  )

# Compare the placeholder/input correlation assumptions with the correlations
# actually present in the bounded simulated scores for each scenario.
observed_hexaco_correlations <- bind_rows(lapply(
  unique(combined_results$Scenario),
  function(scenario_name) {
    scenario_rows <- combined_results$Scenario == scenario_name
    correlation_table <- as.data.frame(as.table(stats::cor(
      combined_results[scenario_rows, hexaco_traits, drop = FALSE]
    )))
    names(correlation_table) <- c("trait_1", "trait_2", "observed_correlation")
    correlation_table$Scenario <- scenario_name
    correlation_table
  }
)) |>
  select(Scenario, trait_1, trait_2, observed_correlation)

# ------------------------------------------------------------
# 6. Diagnostics and summaries
# ------------------------------------------------------------

diversity_by_team <- bind_rows(
  calculate_team_diversity(low_diversity),
  calculate_team_diversity(high_diversity)
)

multivariate_diversity_by_team <- bind_rows(
  calculate_multivariate_team_diversity(low_diversity),
  calculate_multivariate_team_diversity(high_diversity)
)

multivariate_diversity_summary <- multivariate_diversity_by_team |>
  group_by(Scenario) |>
  summarise(
    n_teams = dplyr::n(),
    mean_pairwise_distance = mean(mean_pairwise_standardized_distance),
    sd_across_teams = stats::sd(mean_pairwise_standardized_distance),
    min_pairwise_distance = min(mean_pairwise_standardized_distance),
    max_pairwise_distance = max(mean_pairwise_standardized_distance),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

diversity_summary <- diversity_by_team |>
  group_by(Scenario, trait, trait_label) |>
  summarise(
    target_sd = if_else(first(Scenario) == "Low diversity", low_target_sd, high_target_sd),
    mean_achieved_sd = mean(within_team_sd),
    mean_standardized_sd = mean(standardized_within_team_sd),
    mean_score_range = mean(score_range),
    mean_pairwise_abs_difference = mean(mean_pairwise_abs_difference),
    sd_across_teams = stats::sd(within_team_sd),
    min_achieved_sd = min(within_team_sd),
    max_achieved_sd = max(within_team_sd),
    proportion_teams_at_target = mean(abs(within_team_sd - first(target_sd)) < 1e-8),
    .groups = "drop"
  ) |>
  mutate(across(c(target_sd, mean_achieved_sd, mean_standardized_sd,
                  mean_score_range, mean_pairwise_abs_difference, sd_across_teams,
                  min_achieved_sd, max_achieved_sd,
                  proportion_teams_at_target), ~ round(.x, 3)))

achieved_diversity_differences <- diversity_summary |>
  select(Scenario, trait, trait_label, mean_achieved_sd) |>
  pivot_wider(names_from = Scenario, values_from = mean_achieved_sd) |>
  mutate(high_minus_low_achieved_sd = `High diversity` - `Low diversity`)

outcome_long <- combined_results |>
  select(Scenario, Team, member, all_of(all_outcomes)) |>
  pivot_longer(
    cols = all_of(all_outcomes),
    names_to = "outcome",
    values_to = "score"
  ) |>
  mutate(
    outcome = factor(outcome, levels = all_outcomes),
    outcome_label = unname(outcome_labels[as.character(outcome)])
  )

twq_item_columns <- unlist(twq_items, use.names = FALSE)
twq_scored_item_columns <- paste0(twq_item_columns, "_scored")
twq_item_responses <- combined_results |>
  select(Scenario, Team, member, all_of(twq_item_columns),
         all_of(twq_scored_item_columns))

twq_item_response_frequencies <- combined_results |>
  select(Scenario, all_of(twq_item_columns)) |>
  pivot_longer(
    cols = all_of(twq_item_columns),
    names_to = "item",
    values_to = "response"
  ) |>
  group_by(Scenario, item, response) |>
  summarise(n = dplyr::n(), .groups = "drop") |>
  group_by(Scenario, item) |>
  mutate(proportion = n / sum(n)) |>
  ungroup()

boundary_diagnostics <- combined_results |>
  select(Scenario, all_of(c(all_outcomes, twq_item_columns,
                            twq_scored_item_columns))) |>
  pivot_longer(
    cols = -Scenario,
    names_to = "measure",
    values_to = "score"
  ) |>
  group_by(Scenario, measure) |>
  summarise(
    n = dplyr::n(),
    proportion_at_1 = mean(score <= 1 + 1e-8),
    proportion_at_5 = mean(score >= 5 - 1e-8),
    proportion_at_either_boundary = mean(score <= 1 + 1e-8 | score >= 5 - 1e-8),
    .groups = "drop"
  )

teamwork_summary <- outcome_long |>
  group_by(Scenario, outcome, outcome_label) |>
  summarise(
    n_participants = dplyr::n(),
    mean_score = mean(score),
    sd_score = stats::sd(score),
    median_score = median(score),
    min_score = min(score),
    max_score = max(score),
    .groups = "drop"
  ) |>
  mutate(across(c(mean_score, sd_score, median_score, min_score, max_score), ~ round(.x, 3)))

scenario_differences <- teamwork_summary |>
  select(Scenario, outcome, outcome_label, mean_score) |>
  pivot_wider(names_from = Scenario, values_from = mean_score) |>
  mutate(
    high_minus_low = `High diversity` - `Low diversity`
  ) |>
  arrange(outcome)

# Diagnostics are explicit and stop the script if something is invalid.
# The diversity summary also reports the share of teams whose achieved SD
# exactly equals the requested target (within numerical tolerance).
check_scenario <- function(d, label, expected_scenario) {
  validate_team_data(d, expected_teams = n_teams, expected_size = team_size)
  if (n_distinct(d$Scenario) != 1L || !all(d$Scenario == expected_scenario)) {
    stop(label, ": scenario labels are incomplete or unexpected.", call. = FALSE)
  }
  if (nrow(d) != n_teams * team_size) {
    stop(label, ": unexpected participant row count.", call. = FALSE)
  }
  if (!all(vapply(d[all_outcomes], is.numeric, logical(1)))) {
    stop(label, ": one or more outcomes are not numeric.", call. = FALSE)
  }
  scores <- as.matrix(d[all_outcomes])
  if (any(!is.finite(scores)) || any(scores < 1 | scores > 5)) {
    stop(label, ": outcome scores must be finite and within [1, 5].", call. = FALSE)
  }
  if (anyDuplicated(d[c("Scenario", "Team", "member")])) {
    stop(label, ": duplicate Team/member IDs.", call. = FALSE)
  }
  invisible(TRUE)
}
check_scenario(low_results, "Low-diversity results", "Low diversity")
check_scenario(high_results, "High-diversity results", "High diversity")

validation_report <- data.frame(
  scenario = c("Low diversity", "High diversity"),
  teams = c(n_distinct(low_results$Team), n_distinct(high_results$Team)),
  participants = c(nrow(low_results), nrow(high_results)),
  expected_team_size = team_size,
  min_outcome = c(min(as.matrix(low_results[all_outcomes])),
                  min(as.matrix(high_results[all_outcomes]))),
  max_outcome = c(max(as.matrix(low_results[all_outcomes])),
                  max(as.matrix(high_results[all_outcomes]))),
  all_finite = c(all(is.finite(as.matrix(low_results[all_outcomes]))),
                 all(is.finite(as.matrix(high_results[all_outcomes])))),
  all_bounded_1_to_5 = c(
    all(as.matrix(low_results[all_outcomes]) >= 1 &
        as.matrix(low_results[all_outcomes]) <= 5),
    all(as.matrix(high_results[all_outcomes]) >= 1 &
        as.matrix(high_results[all_outcomes]) <= 5)
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 7. Visualizations
# ------------------------------------------------------------

trait_long <- bind_rows(low_diversity, high_diversity) |>
  pivot_longer(
    cols = all_of(hexaco_traits),
    names_to = "trait",
    values_to = "score"
  ) |>
  mutate(trait_label = unname(trait_labels[trait]))

p_hexaco <- ggplot(trait_long, aes(x = score, fill = Scenario)) +
  geom_histogram(binwidth = 0.2, boundary = 1, position = "identity",
                 alpha = 0.50, colour = "white", linewidth = 0.15) +
  facet_wrap(~ trait_label, ncol = 3) +
  scale_fill_manual(values = c("High diversity" = "#F8766D",
                               "Low diversity" = "#00BFC4")) +
  scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(
    title = "HEXACO Score Distributions",
    subtitle = "Randomly generated hypothetical individual scores, bounded from 1 to 5",
    x = "HEXACO score", y = "Participant count", fill = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p_diversity <- ggplot(diversity_by_team,
                      aes(x = trait_label, y = within_team_sd, fill = Scenario)) +
  geom_boxplot(position = position_dodge(width = 0.78),
               width = 0.65, outlier.alpha = 0.15) +
  scale_fill_manual(values = c("High diversity" = "#F8766D",
                               "Low diversity" = "#00BFC4")) +
  labs(
    title = "Observed Within-Team Diversity by HEXACO Trait",
    subtitle = "Each observation is one team's within-team sample SD",
    x = "HEXACO trait", y = "Within-team standard deviation", fill = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 25, hjust = 1))

p_multivariate_diversity <- ggplot(
  multivariate_diversity_by_team,
  aes(x = Scenario, y = mean_pairwise_standardized_distance, fill = Scenario)
) +
  geom_boxplot(width = 0.65, outlier.alpha = 0.15) +
  scale_fill_manual(values = c("High diversity" = "#F8766D",
                               "Low diversity" = "#00BFC4")) +
  labs(
    title = "Multivariate Personality Diversity Within Teams",
    subtitle = "Mean pairwise distance across all six traits, scaled by reference SD",
    x = "Scenario", y = "Mean pairwise standardized distance", fill = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

p_outcomes <- ggplot(outcome_long, aes(x = score, fill = Scenario)) +
  geom_histogram(binwidth = 0.2, boundary = 1, position = "identity",
                 alpha = 0.48, colour = "white", linewidth = 0.12) +
  facet_wrap(~ outcome_label, ncol = 3, scales = "free_y") +
  scale_fill_manual(values = c("High diversity" = "#F8766D",
                               "Low diversity" = "#00BFC4")) +
  scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
  labs(
    title = "Individual-Level Teamwork-Quality Scale Scores",
    subtitle = "Means of simulated ordinal 1–5 item responses; assumptions are illustrative",
    x = "Subscale mean (1–5)", y = "Participant count", fill = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

p_means <- ggplot(teamwork_summary,
                  aes(x = outcome_label, y = mean_score, fill = Scenario)) +
  geom_col(position = position_dodge(width = 0.82), width = 0.75) +
  scale_fill_manual(values = c("High diversity" = "#F8766D",
                               "Low diversity" = "#00BFC4")) +
  scale_y_continuous(limits = c(0, 5), breaks = 0:5,
                     expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "Mean Simulated Teamwork Quality by Diversity Scenario",
    subtitle = "Illustrative assumptions; bars begin at zero to avoid a truncated baseline",
    x = "Teamwork outcome", y = "Mean simulated score (1–5)", fill = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1))

# Show the contribution from each person's own HEXACO profile.
effect_long <- combined_results |>
  select(Scenario, Team, member, starts_with("individual_component_")) |>
  pivot_longer(
    cols = starts_with("individual_component_"),
    names_to = "outcome",
    names_prefix = "individual_component_",
    values_to = "individual_component"
  )

composition_effect_long <- combined_results |>
  select(Scenario, Team, member, starts_with("composition_component_")) |>
  pivot_longer(
    cols = starts_with("composition_component_"),
    names_to = "outcome",
    names_prefix = "composition_component_",
    values_to = "composition_component"
  ) |>
  mutate(outcome_label = unname(outcome_labels[outcome]))

effect_component_summary <- combined_results |>
  select(
    Scenario, Team, member,
    starts_with("individual_component_"),
    starts_with("composition_component_"),
    starts_with("diversity_component_")
  ) |>
  pivot_longer(
    cols = matches("^(individual|composition|diversity)_component_"),
    names_to = c("component", "outcome"),
    names_pattern = "^(individual|composition|diversity)_component_(.*)$",
    values_to = "component_value"
  ) |>
  group_by(Scenario, outcome, component) |>
  summarise(
    mean_component = mean(component_value),
    sd_component = stats::sd(component_value),
    mean_absolute_component = mean(abs(component_value)),
    .groups = "drop"
  ) |>
  mutate(
    outcome_label = unname(outcome_labels[outcome]),
    component_label = recode(
      component,
      individual = "Within-team individual",
      composition = "Team composition",
      diversity = "Within-team diversity"
    ),
    across(c(mean_component, sd_component, mean_absolute_component), ~ round(.x, 3))
  )

score_long_for_effect <- combined_results |>
  select(Scenario, Team, member, all_of(twq_outcomes)) |>
  pivot_longer(cols = all_of(twq_outcomes),
               names_to = "outcome", values_to = "score")

effect_plot_data <- left_join(
  effect_long, score_long_for_effect,
  by = c("Scenario", "Team", "member", "outcome")
) |>
  mutate(outcome_label = unname(outcome_labels[outcome]))

p_individual_effects <- ggplot(
  effect_plot_data,
  aes(x = individual_component, y = score, colour = Scenario)
) +
  geom_point(alpha = 0.10, size = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  facet_wrap(~ outcome_label, ncol = 3) +
  scale_colour_manual(values = c("High diversity" = "#F8766D",
                                "Low diversity" = "#00BFC4")) +
  labs(
    title = "Within-Team Individual Personality Contribution",
    subtitle = "Individual HEXACO deviations from each team's average profile",
    x = "Within-team individual contribution (model component)",
    y = "Simulated individual teamwork score",
    colour = "Scenario"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

composition_plot_data <- left_join(
  composition_effect_long, score_long_for_effect,
  by = c("Scenario", "Team", "member", "outcome")
)

p_composition_effects <- ggplot(
  composition_plot_data,
  aes(x = composition_component, y = score, colour = Scenario)
) +
  geom_point(alpha = 0.10, size = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  facet_wrap(~ outcome_label, ncol = 3) +
  scale_colour_manual(values = c("High diversity" = "#F8766D",
                                "Low diversity" = "#00BFC4")) +
  labs(
    title = "Team Personality Composition Contribution",
    subtitle = "Each member receives the component from their team's average HEXACO profile",
    x = "Team-composition contribution (model component)",
    y = "Simulated individual teamwork score",
    colour = "Scenario"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

# ------------------------------------------------------------
# 8. Additional analyses: paired differences and dose-response
# ------------------------------------------------------------

# Paired team-level high-minus-low differences. Teams are paired by Team ID;
# 95% t intervals describe simulation uncertainty across the paired teams.
team_means_by_scenario <- combined_results |>
  group_by(Scenario, Team) |>
  summarise(
    across(all_of(all_outcomes), ~ mean(.x)),
    .groups = "drop"
  )

paired_differences <- team_means_by_scenario |>
  pivot_longer(
    cols = all_of(all_outcomes),
    names_to = "outcome",
    values_to = "team_mean"
  ) |>
  pivot_wider(names_from = Scenario, values_from = team_mean) |>
  mutate(difference = `High diversity` - `Low diversity`) |>
  group_by(outcome) |>
  summarise(
    mean_difference = mean(difference),
    sd_difference = stats::sd(difference),
    n_teams = dplyr::n(),
    paired_cohens_dz = if_else(sd_difference > 0,
                              mean_difference / sd_difference, NA_real_),
    paired_hedges_gz = paired_cohens_dz * (1 - 3 / (4 * n_teams - 5)),
    se_difference = sd_difference / sqrt(n_teams),
    ci_low = mean_difference + stats::qt(0.025, df = n_teams - 1) * se_difference,
    ci_high = mean_difference + stats::qt(0.975, df = n_teams - 1) * se_difference,
    .groups = "drop"
  ) |>
  mutate(
    outcome_label = unname(outcome_labels[outcome]),
    across(c(mean_difference, sd_difference, paired_cohens_dz,
             paired_hedges_gz, se_difference, ci_low, ci_high), ~ round(.x, 4))
  )

p_paired_differences <- ggplot(
  paired_differences,
  aes(x = mean_difference, y = reorder(outcome_label, mean_difference))
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey45") +
  geom_errorbar(
    aes(xmin = ci_low, xmax = ci_high),
    orientation = "y", width = 0.15
  ) +
  geom_point(size = 2.8) +
  labs(
    title = "High minus Low Diversity: Mean Difference in Simulated Outcomes",
    subtitle = "Paired team-level differences with 95% confidence intervals; hypothetical assumptions only",
    x = "Difference in mean score (High − Low)",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

# A separate dose-response simulation across nine requested SD levels.
# Common random numbers are reused at every level: each target gets the same
# team centers and within-team patterns, and the outcome model gets the same
# team effects, individual residuals, and item-noise draws. Differences across
# levels are therefore easier to attribute to the diversity target. This is
# a hypothetical sensitivity analysis, not empirical data.
dose_targets <- simulation_config$dose_response$target_sds
dose_teams_per_level <- simulation_config$dose_response$teams_per_level

dose_results <- lapply(seq_along(dose_targets), function(i) {
  target <- dose_targets[[i]]
  scenario_name <- sprintf("Target SD %.1f", target)
  generated <- generate_diverse_teams(
    n_teams = dose_teams_per_level,
    team_size = team_size,
    trait_means = trait_reference_means,
    target_diversity = setNames(rep(target, length(hexaco_traits)), hexaco_traits),
    team_mean_sd = team_mean_sd,
    seed = simulation_config$randomness$dose_response_team_seed,
    scenario = scenario_name
  )
  simulate_teamwork_with_diversity(
    generated,
    trait_effects = default_trait_effects,
    composition_effects = default_composition_effects,
    diversity_effects = default_diversity_effects,
    intercepts = outcome_intercepts,
    diversity_reference = default_diversity_reference,
    team_sd = team_random_sd,
    noise_sd = individual_noise_sd,
    seed = simulation_config$randomness$dose_response_outcome_seed
  ) |>
    mutate(requested_target_sd = target)
})

dose_results <- bind_rows(dose_results)
dose_team_diversity <- calculate_team_diversity(
  dose_results |> select(Scenario, Team, member, all_of(hexaco_traits))
) |>
  filter(as.character(trait) == "agreeableness") |>
  select(Scenario, Team, within_team_sd) |>
  left_join(
    dose_results |>
      group_by(Scenario, Team) |>
      summarise(
        across(all_of(all_outcomes), ~ mean(.x)),
        requested_target_sd = first(requested_target_sd),
        .groups = "drop"
      ),
    by = c("Scenario", "Team")
  )

dose_outcome_long <- dose_team_diversity |>
  pivot_longer(
    cols = all_of(all_outcomes),
    names_to = "outcome",
    values_to = "team_mean"
  ) |>
  mutate(outcome_label = unname(outcome_labels[outcome]))

p_dose_response <- ggplot(
  dose_outcome_long,
  aes(x = within_team_sd, y = team_mean)
) +
  geom_point(alpha = 0.20, size = 0.8, colour = "#7CAE00") +
  geom_smooth(method = "lm", se = TRUE, colour = "grey25", linewidth = 0.8) +
  facet_wrap(~ outcome_label, ncol = 3) +
  labs(
    title = "Team Outcome vs. Within-Team Agreeableness Diversity",
    subtitle = "Nine-level hypothetical sensitivity analysis; each point is one team",
    x = "Observed within-team SD of agreeableness",
    y = "Team mean simulated outcome"
  ) +
  theme_minimal(base_size = 11)

# ------------------------------------------------------------
# 9. One-at-a-time sensitivity analysis of key assumptions
# ------------------------------------------------------------

# These scenarios vary one assumption family at a time around the baseline.
# Common seeds are reused across every setting and diversity target, making
# comparisons less affected by unrelated random draws. Values remain
# hypothetical until informed by project-specific data or theory.
sensitivity_identity_correlation <- diag(length(hexaco_traits))
dimnames(sensitivity_identity_correlation) <- list(hexaco_traits, hexaco_traits)
sensitivity_identity_outcome_correlation <- diag(length(twq_outcomes))
dimnames(sensitivity_identity_outcome_correlation) <-
  list(twq_outcomes, twq_outcomes)
weaken_correlation <- function(correlation, retained_strength = 0.5) {
  identity <- diag(nrow(correlation))
  dimnames(identity) <- dimnames(correlation)
  (1 - retained_strength) * identity + retained_strength * correlation
}

new_sensitivity_spec <- function(
    setting, personality_effect_multiplier = 1,
    diversity_effect_multiplier = 1, team_mean_sd_value = team_mean_sd,
    hexaco_correlation = hexaco_correlation_matrix,
    team_correlation = team_outcome_correlation_matrix,
    residual_correlation = individual_residual_correlation_matrix) {
  list(
    setting = setting,
    personality_effect_multiplier = personality_effect_multiplier,
    diversity_effect_multiplier = diversity_effect_multiplier,
    team_mean_sd = team_mean_sd_value,
    hexaco_correlation = hexaco_correlation,
    team_correlation = team_correlation,
    residual_correlation = residual_correlation
  )
}

sensitivity_specs <- list(
  new_sensitivity_spec("Baseline"),
  new_sensitivity_spec("Personality effects x0.5", personality_effect_multiplier = 0.5),
  new_sensitivity_spec("Personality effects x1.5", personality_effect_multiplier = 1.5),
  new_sensitivity_spec("Diversity effects x0", diversity_effect_multiplier = 0),
  new_sensitivity_spec("Diversity effects x2", diversity_effect_multiplier = 2),
  new_sensitivity_spec("Team mean SD 0", team_mean_sd_value = 0),
  new_sensitivity_spec("Team mean SD 0.70", team_mean_sd_value = 0.70),
  new_sensitivity_spec("HEXACO correlations removed",
                       hexaco_correlation = sensitivity_identity_correlation),
  new_sensitivity_spec("HEXACO correlations halved",
                       hexaco_correlation = weaken_correlation(hexaco_correlation_matrix)),
  new_sensitivity_spec("Team outcome correlations removed",
                       team_correlation = sensitivity_identity_outcome_correlation),
  new_sensitivity_spec("Team outcome correlations halved",
                       team_correlation = weaken_correlation(team_outcome_correlation_matrix)),
  new_sensitivity_spec("Residual correlations removed",
                       residual_correlation = sensitivity_identity_outcome_correlation),
  new_sensitivity_spec("Residual correlations halved",
                       residual_correlation = weaken_correlation(
                         individual_residual_correlation_matrix
                       ))
)

scale_effect_list <- function(effects, multiplier) {
  lapply(effects, function(coefficients) coefficients * multiplier)
}

sensitivity_n_teams <- simulation_config$sensitivity$teams_per_condition
sensitivity_team_seed <- simulation_config$randomness$sensitivity_team_seed
sensitivity_outcome_seed <- simulation_config$randomness$sensitivity_outcome_seed
sensitivity_scenario_means <- bind_rows(lapply(sensitivity_specs, function(spec) {
  bind_rows(lapply(c("Low diversity", "High diversity"), function(scenario_name) {
    target_sd <- if (scenario_name == "Low diversity") low_target_sd else high_target_sd
    generated <- generate_diverse_teams(
      n_teams = sensitivity_n_teams,
      team_size = team_size,
      trait_means = trait_reference_means,
      trait_correlation = spec$hexaco_correlation,
      target_diversity = setNames(rep(target_sd, length(hexaco_traits)), hexaco_traits),
      team_mean_sd = spec$team_mean_sd,
      seed = sensitivity_team_seed,
      scenario = scenario_name
    )
    simulated <- simulate_teamwork_with_diversity(
      generated,
      trait_effects = scale_effect_list(
        default_trait_effects, spec$personality_effect_multiplier
      ),
      composition_effects = scale_effect_list(
        default_composition_effects, spec$personality_effect_multiplier
      ),
      diversity_effects = scale_effect_list(
        default_diversity_effects, spec$diversity_effect_multiplier
      ),
      intercepts = outcome_intercepts,
      diversity_reference = default_diversity_reference,
      team_sd = team_random_sd,
      noise_sd = individual_noise_sd,
      team_correlations = spec$team_correlation,
      residual_correlations = spec$residual_correlation,
      team_sds = team_outcome_sds,
      residual_sds = individual_residual_sds,
      seed = sensitivity_outcome_seed
    )
    simulated |>
      summarise(across(all_of(all_outcomes), mean)) |>
      mutate(setting = spec$setting, Scenario = scenario_name)
  }))
}))

sensitivity_results <- sensitivity_scenario_means |>
  pivot_longer(
    cols = all_of(all_outcomes),
    names_to = "outcome",
    values_to = "mean_score"
  ) |>
  pivot_wider(names_from = Scenario, values_from = mean_score) |>
  mutate(high_minus_low = `High diversity` - `Low diversity`) |>
  left_join(
    sensitivity_scenario_means |>
      filter(setting == "Baseline") |>
      pivot_longer(cols = all_of(all_outcomes),
                   names_to = "outcome", values_to = "mean_score") |>
      pivot_wider(names_from = Scenario, values_from = mean_score) |>
      transmute(outcome, baseline_high_minus_low = `High diversity` - `Low diversity`),
    by = "outcome"
  ) |>
  mutate(change_from_baseline = high_minus_low - baseline_high_minus_low) |>
  mutate(outcome_label = unname(outcome_labels[outcome]))

sensitivity_range_summary <- sensitivity_results |>
  filter(setting != "Baseline") |>
  group_by(outcome, outcome_label) |>
  summarise(
    baseline_high_minus_low = first(baseline_high_minus_low),
    minimum_high_minus_low = min(high_minus_low),
    maximum_high_minus_low = max(high_minus_low),
    min_change_from_baseline = min(change_from_baseline),
    max_change_from_baseline = max(change_from_baseline),
    .groups = "drop"
  )

p_sensitivity <- ggplot(
  sensitivity_results,
  aes(x = high_minus_low, y = reorder(setting, high_minus_low), colour = setting == "Baseline")
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2) +
  facet_wrap(~ outcome_label, ncol = 2, scales = "free_y") +
  scale_colour_manual(values = c("FALSE" = "#619CFF", "TRUE" = "#D55E00"),
                      guide = "none") +
  labs(
    title = "Sensitivity of High-minus-Low Diversity Differences",
    subtitle = "One-at-a-time changes to illustrative model assumptions",
    x = "High minus low diversity mean score",
    y = "Assumption setting"
  ) +
  theme_minimal(base_size = 10)

# ------------------------------------------------------------
# 10. Export datasets, summaries, diagnostics, and plots
# ------------------------------------------------------------

plot_dir <- file.path(output_dir, "plots")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# Save the full configuration and runtime versions with every run.
simulation_config$sensitivity$settings <- sensitivity_specs
saveRDS(simulation_config, file.path(output_dir, "simulation_config.rds"))
writeLines(capture.output(dput(simulation_config)),
           file.path(output_dir, "simulation_config.txt"))
package_versions <- data.frame(
  package = c("R", "dplyr", "tidyr", "ggplot2"),
  version = c(
    R.version.string,
    as.character(utils::packageVersion("dplyr")),
    as.character(utils::packageVersion("tidyr")),
    as.character(utils::packageVersion("ggplot2"))
  ),
  stringsAsFactors = FALSE
)
write.csv(package_versions, file.path(output_dir, "package_versions.csv"),
          row.names = FALSE)

run_metadata <- data.frame(
  key = c(
    "run_started_at", "R_version", "operating_system", "main_team_generation_seed",
    "main_outcome_seed", "dose_response_team_seed", "dose_response_outcome_seed",
    "sensitivity_team_seed", "sensitivity_outcome_seed", "pairing_strategy",
    "sensitivity_stream_strategy", "primary_teams_per_scenario", "team_size",
    "low_target_sd", "high_target_sd", "team_mean_sd", "output_directory"
  ),
  value = c(
    format(run_started_at, tz = "UTC", usetz = TRUE), R.version.string,
    paste(Sys.info()[c("sysname", "release", "machine")], collapse = " / "),
    scenario_seed, outcome_seed,
    simulation_config$randomness$dose_response_team_seed,
    simulation_config$randomness$dose_response_outcome_seed,
    sensitivity_team_seed, sensitivity_outcome_seed,
    "Low/high share generation and outcome seeds; paired by team/member ID",
    "Separate seed stream from primary run; common draws across sensitivity settings",
    n_teams, team_size, low_target_sd, high_target_sd, team_mean_sd,
    normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  ),
  stringsAsFactors = FALSE
)
write.csv(run_metadata, file.path(output_dir, "run_metadata.csv"), row.names = FALSE)

write.csv(low_diversity, file.path(output_dir, "low_diversity_teams.csv"),
          row.names = FALSE)
write.csv(high_diversity, file.path(output_dir, "high_diversity_teams.csv"),
          row.names = FALSE)
write.csv(low_results, file.path(output_dir, "low_diversity_teamwork_outcomes.csv"),
          row.names = FALSE)
write.csv(high_results, file.path(output_dir, "high_diversity_teamwork_outcomes.csv"),
          row.names = FALSE)
write.csv(combined_results, file.path(output_dir, "combined_individual_level_results.csv"),
          row.names = FALSE)
write.csv(twq_item_responses,
          file.path(output_dir, "twq_ordinal_item_responses.csv"),
          row.names = FALSE)
write.csv(twq_item_response_frequencies,
          file.path(output_dir, "twq_item_response_frequencies.csv"),
          row.names = FALSE)
write.csv(diversity_by_team, file.path(output_dir, "actual_diversity_by_team.csv"),
          row.names = FALSE)
write.csv(multivariate_diversity_by_team,
          file.path(output_dir, "multivariate_diversity_by_team.csv"),
          row.names = FALSE)
write.csv(diversity_summary, file.path(output_dir, "diversity_summary.csv"),
          row.names = FALSE)
write.csv(multivariate_diversity_summary,
          file.path(output_dir, "multivariate_diversity_summary.csv"),
          row.names = FALSE)
write.csv(teamwork_summary, file.path(output_dir, "teamwork_quality_summary.csv"),
          row.names = FALSE)
write.csv(scenario_differences, file.path(output_dir, "scenario_mean_differences.csv"),
          row.names = FALSE)
write.csv(achieved_diversity_differences,
          file.path(output_dir, "achieved_diversity_differences.csv"),
          row.names = FALSE)
write.csv(validation_report, file.path(output_dir, "validation_report.csv"),
          row.names = FALSE)
write.csv(trait_score_diagnostics,
          file.path(output_dir, "bounded_trait_score_diagnostics.csv"),
          row.names = FALSE)
write.csv(observed_hexaco_correlations,
          file.path(output_dir, "observed_hexaco_trait_correlations.csv"),
          row.names = FALSE)
write.csv(outcome_correlation_assumptions,
          file.path(output_dir, "outcome_correlation_assumptions.csv"),
          row.names = FALSE)
write.csv(outcome_sd_assumptions,
          file.path(output_dir, "outcome_covariance_sd_assumptions.csv"),
          row.names = FALSE)
write.csv(outcome_correlation_diagnostics,
          file.path(output_dir, "outcome_correlation_diagnostics.csv"),
          row.names = FALSE)
write.csv(default_diversity_effects_table,
          file.path(output_dir, "diversity_effect_coefficients.csv"),
          row.names = FALSE)
write.csv(boundary_diagnostics,
          file.path(output_dir, "score_boundary_diagnostics.csv"),
          row.names = FALSE)
write.csv(sensitivity_results,
          file.path(output_dir, "assumption_sensitivity_results.csv"),
          row.names = FALSE)
write.csv(sensitivity_range_summary,
          file.path(output_dir, "assumption_sensitivity_ranges.csv"),
          row.names = FALSE)
write.csv(effect_component_summary,
          file.path(output_dir, "effect_component_summary.csv"),
          row.names = FALSE)
write.csv(paired_differences,
          file.path(output_dir, "paired_team_level_differences.csv"),
          row.names = FALSE)
write.csv(dose_results,
          file.path(output_dir, "continuous_diversity_sensitivity_results.csv"),
          row.names = FALSE)
write.csv(dose_team_diversity,
          file.path(output_dir, "continuous_diversity_team_summary.csv"),
          row.names = FALSE)

ggsave(file.path(plot_dir, "01_hexaco_score_distributions.png"),
       p_hexaco, width = 11, height = 7, dpi = 300)
ggsave(file.path(plot_dir, "02_within_team_diversity.png"),
       p_diversity, width = 11, height = 6.5, dpi = 300)
ggsave(file.path(plot_dir, "08_multivariate_team_diversity.png"),
       p_multivariate_diversity, width = 8, height = 6, dpi = 300)
ggsave(file.path(plot_dir, "03_teamwork_outcome_distributions.png"),
       p_outcomes, width = 11, height = 7, dpi = 300)
ggsave(file.path(plot_dir, "04_teamwork_mean_comparison.png"),
       p_means, width = 11, height = 6.5, dpi = 300)
ggsave(file.path(plot_dir, "05_individual_personality_effects.png"),
       p_individual_effects, width = 11, height = 7, dpi = 300)
ggsave(file.path(plot_dir, "09_team_composition_effects.png"),
       p_composition_effects, width = 11, height = 7, dpi = 300)
ggsave(file.path(plot_dir, "06_paired_team_level_differences.png"),
       p_paired_differences, width = 11, height = 7, dpi = 300)
ggsave(file.path(plot_dir, "07_diversity_dose_response.png"),
       p_dose_response, width = 11, height = 7, dpi = 300)
ggsave(file.path(plot_dir, "10_assumption_sensitivity.png"),
       p_sensitivity, width = 12, height = 10, dpi = 300)

# Print the most important tables and plots when run interactively.
cat("\n=== Validation report ===\n")
print(validation_report)
cat("\n=== Achieved within-team diversity ===\n")
print(diversity_summary)
cat("\n=== Achieved multivariate team diversity ===\n")
print(multivariate_diversity_summary)
cat("\n=== Teamwork-quality summary ===\n")
print(teamwork_summary)
cat("\n=== Bounded trait-score diagnostics ===\n")
print(trait_score_diagnostics)
cat("\n=== Observed HEXACO trait correlations ===\n")
print(observed_hexaco_correlations)
cat("\n=== Individual, composition, and diversity effect components ===\n")
print(effect_component_summary)
cat("\n=== High minus low diversity mean differences ===\n")
print(scenario_differences)
cat("\n=== Paired team-level differences with 95% confidence intervals ===\n")
print(paired_differences)
cat("\n=== Assumption sensitivity: high-minus-low outcome differences ===\n")
print(sensitivity_range_summary)
cat("\n=== Achieved high-minus-low diversity differences ===\n")
print(achieved_diversity_differences)
cat("\n=== Score boundary diagnostics ===\n")
print(boundary_diagnostics)

if (interactive()) {
  print(p_hexaco)
  print(p_diversity)
  print(p_multivariate_diversity)
  print(p_outcomes)
  print(p_means)
  print(p_individual_effects)
  print(p_composition_effects)
  print(p_paired_differences)
  print(p_dose_response)
  print(p_sensitivity)
}

cat(
  "\nSimulation complete.\n",
  "Output folder: ", normalizePath(output_dir, winslash = "/", mustWork = FALSE), "\n",
  "Scenarios: ", n_teams, " teams x ", team_size,
  " members = ", n_teams * team_size, " participants per scenario.\n",
  "All reported effects are hypothetical assumptions.\n",
  sep = ""
)

########### checks

# Team-average personality profiles
team_trait_means <- combined_results |>
  group_by(Scenario, Team) |>
  summarise(
    across(all_of(hexaco_traits), mean),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = all_of(hexaco_traits),
    names_to = "trait",
    values_to = "team_mean"
  ) |>
  group_by(Scenario, trait) |>
  summarise(
    mean_of_team_means = mean(team_mean),
    sd_between_teams = sd(team_mean),
    .groups = "drop"
  )

print(team_trait_means)
write.csv(team_trait_means,
          file.path(output_dir, "team_personality_average_summary.csv"),
          row.names = FALSE)

#####
# Team IDs intentionally repeat between scenarios for paired comparisons.
# Check uniqueness using the full Scenario/Team/member key.
if (anyDuplicated(combined_results[c("Scenario", "Team", "member")])) {
  stop("Duplicate Scenario/Team/member identifiers found.", call. = FALSE)
}

# Final manifest includes the team-average diagnostic written above.
manifest_path <- file.path(output_dir, "run_manifest.csv")
manifest_assumptions <- paste0(
  "n_teams=", n_teams, "; team_size=", team_size,
  "; target_sd_low/high=", low_target_sd, "/", high_target_sd,
  "; team_mean_sd=", team_mean_sd,
  "; team/residual SD=", team_random_sd, "/", individual_noise_sd,
  "; correlation matrices and personality/diversity coefficients are illustrative",
  "; low/high use paired seeds; sensitivity uses an independent stream with ",
  length(sensitivity_specs), " one-at-a-time settings; full assumptions in simulation_config.rds"
)
manifest_files <- list.files(output_dir, recursive = TRUE, full.names = TRUE)
manifest_files <- manifest_files[basename(manifest_files) != "run_manifest.csv"]
manifest_files <- normalizePath(manifest_files, winslash = "/", mustWork = TRUE)
manifest_root <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
manifest_relative_paths <- substring(manifest_files, nchar(manifest_root) + 2L)
manifest_info <- file.info(manifest_files)
run_manifest <- data.frame(
  path = c(manifest_relative_paths, "run_manifest.csv"),
  file_type = c(tools::file_ext(manifest_relative_paths), "csv"),
  size_bytes = c(as.numeric(manifest_info$size), NA_real_),
  run_started_at = format(run_started_at, tz = "UTC", usetz = TRUE),
  assumptions_summary = manifest_assumptions,
  stringsAsFactors = FALSE
)
write.csv(run_manifest, manifest_path, row.names = FALSE)






