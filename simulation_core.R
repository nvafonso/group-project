# Shared HEXACO team-effect calculations used by the standalone generator and
# the interactive Shiny comparison. This root-level module contains
# calculations only; it reads and writes no project files.

hexaco_assign_teams <- function(person_group_trait, n_teams, team_size,
                               random_team=NULL) {
  if (!is.numeric(person_group_trait) || any(!is.finite(person_group_trait))) {
    stop("person_group_trait must contain only finite numeric values.", call.=FALSE)
  }
  if (length(n_teams) != 1L || !is.finite(n_teams) || n_teams < 2 ||
      n_teams != as.integer(n_teams) || length(team_size) != 1L ||
      !is.finite(team_size) || team_size < 2 ||
      team_size != as.integer(team_size)) {
    stop("n_teams and team_size must be whole numbers of at least two.", call.=FALSE)
  }
  n_teams <- as.integer(n_teams)
  team_size <- as.integer(team_size)
  n_people <- n_teams * team_size
  if (length(person_group_trait) != n_people) {
    stop("The trait vector length must equal n_teams * team_size.", call.=FALSE)
  }

  validate_assignment <- function(team_id, label) {
    if (length(team_id) != n_people || anyNA(team_id) ||
        any(!team_id %in% seq_len(n_teams)) ||
        any(tabulate(as.integer(team_id), nbins=n_teams) != team_size)) {
      stop(label, " assignment must give every team exactly team_size members.",
           call.=FALSE)
    }
    as.integer(team_id)
  }

  if (is.null(random_team)) {
    random_team <- sample(rep(seq_len(n_teams), each=team_size))
  }
  random_team <- validate_assignment(random_team, "Random")

  # Similar-score assignment puts adjacent sorted scores together.
  ord <- order(person_group_trait)
  similar_team <- integer(n_people)
  similar_team[ord] <- rep(seq_len(n_teams), each=team_size)

  # Balanced assignment alternates sorted scores across teams in a snake pattern.
  balanced_team <- integer(n_people)
  for (k in seq_len(n_people)) {
    row <- (k - 1L) %/% n_teams
    col <- (k - 1L) %% n_teams
    if (row %% 2L == 1L) col <- n_teams - 1L - col
    balanced_team[ord[k]] <- col + 1L
  }

  result <- list(Random=random_team, Balanced=balanced_team, Similar=similar_team)
  for (label in names(result)) result[[label]] <- validate_assignment(result[[label]], label)
  result
}

hexaco_team_trait_components <- function(
    people,
    team_ids,
    traits,
    trait_reference_means,
    trait_reference_sds,
    diversity_reference_means,
    diversity_reference_sds = setNames(rep(1, length(traits)), traits)
) {
  if (!is.data.frame(people) || !length(traits) ||
      !all(traits %in% names(people))) {
    stop("people must be a data frame containing every requested trait.", call.=FALSE)
  }
  if (length(team_ids) != nrow(people) || anyNA(team_ids) ||
      any(!nzchar(trimws(as.character(team_ids))))) {
    stop("team_ids must provide one non-empty identifier per person.", call.=FALSE)
  }

  validate_reference <- function(x, label, positive=FALSE) {
    if (is.null(names(x)) || anyDuplicated(names(x)) ||
        !setequal(names(x), traits) || !is.numeric(x) ||
        any(!is.finite(x)) || (positive && any(x <= 0))) {
      stop(label, " must be a finite named vector for all selected traits",
           if (positive) " with values greater than zero." else ".", call.=FALSE)
    }
    x[traits]
  }
  trait_reference_means <- validate_reference(
    trait_reference_means, "trait_reference_means"
  )
  trait_reference_sds <- validate_reference(
    trait_reference_sds, "trait_reference_sds", positive=TRUE
  )
  diversity_reference_means <- validate_reference(
    diversity_reference_means, "diversity_reference_means"
  )
  diversity_reference_sds <- validate_reference(
    diversity_reference_sds, "diversity_reference_sds", positive=TRUE
  )

  trait_matrix <- as.matrix(people[, traits, drop=FALSE])
  storage.mode(trait_matrix) <- "double"
  if (any(!is.finite(trait_matrix))) {
    stop("All selected trait values must be numeric and finite.", call.=FALSE)
  }

  group_labels <- as.character(team_ids)
  if (is.numeric(team_ids)) {
    group_levels <- as.character(sort(unique(team_ids)))
  } else if (is.factor(team_ids)) {
    group_levels <- levels(droplevels(team_ids))
  } else {
    group_levels <- sort(unique(group_labels))
  }
  group_index <- match(group_labels, group_levels)
  group_rows <- split(seq_len(nrow(people)), factor(group_index, levels=seq_along(group_levels)))
  if (any(lengths(group_rows) < 2L)) {
    stop("Every team must contain at least two people to calculate within-team SD.", call.=FALSE)
  }

  team_means <- do.call(rbind, lapply(group_rows, function(rows) {
    colMeans(trait_matrix[rows, , drop=FALSE])
  }))
  team_sds <- do.call(rbind, lapply(group_rows, function(rows) {
    apply(trait_matrix[rows, , drop=FALSE], 2, stats::sd)
  }))
  colnames(team_means) <- colnames(team_sds) <- traits
  rownames(team_means) <- rownames(team_sds) <- group_levels

  team_mean_z <- sweep(
    sweep(team_means, 2, trait_reference_means, "-"),
    2, trait_reference_sds, "/"
  )
  diversity_z <- sweep(
    sweep(team_sds, 2, diversity_reference_means, "-"),
    2, diversity_reference_sds, "/"
  )
  person_team_mean_z <- team_mean_z[group_index, , drop=FALSE]
  person_diversity_z <- diversity_z[group_index, , drop=FALSE]
  individual_z <- sweep(
    sweep(trait_matrix, 2, trait_reference_means, "-"),
    2, trait_reference_sds, "/"
  ) - person_team_mean_z

  list(
    group_levels=group_levels,
    group_index=group_index,
    team_means=team_means,
    team_sds=team_sds,
    team_mean_z=team_mean_z,
    diversity_z=diversity_z,
    person_team_means=team_means[group_index, , drop=FALSE],
    person_team_sds=team_sds[group_index, , drop=FALSE],
    person_team_mean_z=person_team_mean_z,
    person_diversity_z=person_diversity_z,
    individual_z=individual_z
  )
}

hexaco_compose_latent_score <- function(
    intercept,
    individual_component=0,
    composition_component=0,
    diversity_component=0,
    team_component=0,
    residual=0
) {
  components <- list(intercept, individual_component, composition_component,
                     diversity_component, team_component, residual)
  if (any(!vapply(components, is.numeric, logical(1))) ||
      any(lengths(components) == 0L) ||
      any(!vapply(components, function(x) all(is.finite(x)), logical(1)))) {
    stop("Score components must be non-empty, numeric, and finite.", call.=FALSE)
  }
  n <- max(lengths(components))
  if (any(!lengths(components) %in% c(1L, n))) {
    stop("Score components must have length one or the same length.", call.=FALSE)
  }
  intercept + individual_component + composition_component +
    diversity_component + team_component + residual
}
