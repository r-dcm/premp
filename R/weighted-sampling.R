#' Weighted Sampling
#'
#' Apply a weighted sampling procedure to sample profiles that will be assigned
#' to raters during a sample setting event.
#'
#' @param possible_profiles A tibble with one row for each attribute mastery
#' profiles that is eligible for assignment to raters.
#' @param observed A tibble with one row for each attribute mastery profile that
#' was observed along with the number of times it was observed.
#' @param observed_count_label A character string for the field name of the
#' observed sample sizes in the observed parameter.
#' @param profiles_per_total_levels_mastered An integer specifying the number of
#' profiles to assign to each rater at each total number of levels mastered.
#' @param raters A character vector containing the raters' ids.
#' @param panelist_configuration A list containing the parameters for
#' configuring a panelist design. The allowable parameters are `num_panelists`
#' indicating the number of panelists and `shared_across`
#' indicating the number of profiles that are common to all panelists. Only one
#' of `panelist_configuration` or `group_configuration` should be specified.
#' @param group_configuration A list containing parameters for configuring a
#' group design. The allowable parameters are `num_assignment_groups` indicating
#' the number of groups of panelists, `panelists_per_group` indicating
#' the number of panelists in each group, `shared_within`
#' indicating the number of profiles that are common to all of the panelists
#' within each group, and `shared_across` indicating the number
#' of profiles that are shared by panelists across groups. Only one of
#' `panelist_configuration` or `group_configuration` should be specified.
#'
#' @return [tibble][tibble::tibble-package] A tibble containing the profiles to
#' be assigned to raters during a standard setting event.
weighted_sampling <- function(
  possible_profiles,
  observed,
  observed_count_label,
  profiles_per_total_levels_mastered,
  raters,
  panelist_configuration = NULL,
  group_configuration = NULL
) {
  # identify attributes
  att_vec <- possible_profiles |>
    dplyr::select(-dplyr::any_of(c("total", "profile_id"))) |>
    names()

  if (!is.null(group_configuration)) {
    shared_across <-
      group_configuration$shared_across
  } else {
    shared_across <-
      panelist_configuration$shared_across
  }

  seen_by_all <- possible_profiles |>
    dplyr::filter(.data$total != 0) |>
    dplyr::left_join(observed, by = c(att_vec, "profile_id")) |>
    dplyr::filter(!is.na(!!rlang::sym(observed_count_label))) |>
    dplyr::mutate(size = shared_across) |>
    slice_stratified(
      by = "total",
      size = "size",
      weight_by = "prop"
    ) |>
    dplyr::select(-dplyr::all_of(observed_count_label))

  possible_profiles <- possible_profiles |>
    dplyr::anti_join(seen_by_all, att_vec)

  possible_profiles <- calculate_hamming(
    possible_profiles |>
      dplyr::select(-"profile_id"),
    seen_by_all |>
      dplyr::select(-"total", -"profile_id"),
    att_vec
  )
  possible_profiles <- refine_possible_profiles(
    possible_profiles,
    filter_function = "median",
    raters = raters,
    profiles_per_total_levels_mastered = profiles_per_total_levels_mastered
  )

  remaining_to_sample <- profiles_per_total_levels_mastered -
    shared_across

  if (!is.null(group_configuration)) {
    panelists_per_group <- group_configuration$panelists_per_group
    shared_within <-
      group_configuration$shared_within
    group_shared_assignments <-
      shared_within - shared_across
  } else {
    panelists_per_group <- NA_integer_
    group_shared_assignments <- 0L
  }

  assignments <- tibble::tibble()

  if (group_shared_assignments > 0) {
    for (ii in seq_len(group_shared_assignments)) {
      for (jj in seq_along(raters)) {
        tmp_assignments <- possible_profiles |>
          dplyr::left_join(observed, by = att_vec) |>
          dplyr::filter(!is.na(!!rlang::sym(observed_count_label))) |>
          dplyr::filter(!!rlang::sym(observed_count_label) > 100) |>
          dplyr::mutate(size = 1L) |>
          slice_stratified(
            by = "total",
            size = "size",
            weight_by = "prop"
          ) |>
          dplyr::select(-dplyr::all_of(observed_count_label))

        assignments <- dplyr::bind_rows(
          assignments,
          tmp_assignments |>
            dplyr::mutate(group = raters[jj])
        )

        possible_profiles <- possible_profiles |>
          dplyr::anti_join(tmp_assignments, att_vec)

        possible_profiles <- calculate_hamming(
          possible_profiles,
          tmp_assignments |>
            dplyr::select(-"total", -"profile_id"),
          att_vec
        )
        possible_profiles <- refine_possible_profiles(
          possible_profiles,
          filter_function = "median",
          raters = raters,
          profiles_per_total_levels_mastered = profiles_per_total_levels_mastered
        )
      }
    }

    assignments <- assignments |>
      tidyr::crossing(panelist = glue::glue("rater{1:panelists_per_group}"))
  }

  remaining_to_sample <- remaining_to_sample - group_shared_assignments

  if (!is.null(group_configuration)) {
    rater_dict <- tibble::tibble(group = raters) |>
      tidyr::crossing(panelist = glue::glue("rater{1:panelists_per_group}")) |>
      tibble::rowid_to_column("rater_num")
  } else {
    rater_dict <- tibble::tibble(group = NA, panelist = raters) |>
      tibble::rowid_to_column("rater_num")
  }

  rater_iterator <- rater_dict |>
    dplyr::pull(.data$rater_num)

  if (remaining_to_sample > 0) {
    for (ii in seq_len(remaining_to_sample)) {
      for (jj in seq_along(rater_iterator)) {
        tmp_assignments <- possible_profiles |>
          dplyr::left_join(observed, by = att_vec) |>
          dplyr::filter(!is.na(!!rlang::sym(observed_count_label))) |>
          dplyr::mutate(size = 1) |>
          slice_stratified(
            by = "total",
            size = "size",
            weight_by = "prop"
          ) |>
          dplyr::select(-dplyr::all_of(observed_count_label))

        tmp_group <- rater_dict |>
          dplyr::filter(.data$rater_num == jj) |>
          dplyr::pull(.data$group)
        tmp_panelist <- rater_dict |>
          dplyr::filter(.data$rater_num == jj) |>
          dplyr::pull(.data$panelist)

        assignments <- dplyr::bind_rows(
          assignments,
          tmp_assignments |>
            dplyr::mutate(group = tmp_group, panelist = tmp_panelist)
        )

        possible_profiles <- possible_profiles |>
          dplyr::anti_join(tmp_assignments, att_vec)

        possible_profiles <- calculate_hamming(
          possible_profiles,
          tmp_assignments |>
            dplyr::select(-"total", -"profile_id"),
          att_vec
        )
        possible_profiles <- refine_possible_profiles(
          possible_profiles,
          filter_function = "median",
          raters = raters,
          profiles_per_total_levels_mastered = profiles_per_total_levels_mastered
        )
      }
    }
  }

  if (!is.null(group_configuration)) {
    profile_sampling <- seen_by_all |>
      tidyr::crossing(
        group = raters,
        panelist = glue::glue("rater{1:panelists_per_group}")
      ) |>
      dplyr::bind_rows(assignments) |>
      dplyr::mutate(assigned = 1L) |>
      tidyr::pivot_wider(
        names_from = "panelist",
        values_from = "assigned",
        values_fill = 0L
      ) |>
      dplyr::arrange(
        .data$total,
        dplyr::across(dplyr::any_of(att_vec), dplyr::desc),
        .data$group
      )
  } else {
    profile_sampling <- seen_by_all |>
      tidyr::crossing(panelist = raters) |>
      dplyr::bind_rows(assignments) |>
      dplyr::mutate(assigned = 1L) |>
      tidyr::pivot_wider(
        names_from = "panelist",
        values_from = "assigned",
        values_fill = 0L
      ) |>
      dplyr::arrange(
        .data$total,
        dplyr::across(dplyr::any_of(att_vec), dplyr::desc)
      ) |>
      dplyr::select(-"group")
  }

  return(profile_sampling) # nolint
}
