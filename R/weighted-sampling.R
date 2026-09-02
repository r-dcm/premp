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
#' @param observed_proportion_label A character string for the field name of the
#' observed proportions in the observed parameter.
#' @param shared_profiles The number of profiles that are shared by all
#' panelists. With a table-based design, this is the number of profiles that are
#' seen by all tables. With a panelist-based design, this is the number of
#' profiles seen by all panelists.
#' @param profiles_per_level An integer specifying the number of profiles to
#' assign to each rater at each level of the total skills mastered.
#' @param raters A character vector containing the raters' ids.
#' @param table_configuration A list containing parameters for configuring a
#' table design. The allowable parameters are `panelists_per_table` indicating
#' the number of panelists at each table and `proportion_of_shared_profiles`
#' indicating the proportion of profiles that are common to all of the panelists
#' at each table.
#'
#' @return [tibble][tibble::tibble-package] A tibble containing the profiles to
#' be assigned to raters during a standard setting event.
weighted_sampling <- function(
  possible_profiles,
  observed,
  observed_count_label,
  observed_proportion_label,
  shared_profiles,
  profiles_per_level,
  raters,
  table_configuration = NULL
) {
  # identify attributes
  att_vec <- possible_profiles |>
    dplyr::select(-"total") |>
    names()

  seen_by_all <- possible_profiles |>
    dplyr::filter(.data$total != 0) |>
    dplyr::left_join(observed, by = att_vec) |>
    dplyr::filter(!is.na(!!rlang::sym(observed_count_label))) |>
    dplyr::mutate(size = shared_profiles) |>
    slice_stratified(
      by = "total",
      size = "size",
      weight_by = observed_proportion_label
    ) |>
    dplyr::select(-dplyr::all_of(observed_count_label))

  possible_profiles <- possible_profiles |>
    dplyr::anti_join(seen_by_all, att_vec)

  possible_profiles <- calculate_hamming(
    possible_profiles,
    seen_by_all |>
      dplyr::select(-"total"),
    att_vec
  )
  possible_profiles <- refine_possible_profiles(
    possible_profiles,
    filter_function = "median",
    raters = raters,
    profiles_per_level = profiles_per_level
  )

  remaining_to_sample <- profiles_per_level - shared_profiles

  if (!is.null(table_configuration)) {
    panelists_per_table <- table_configuration$panelists_per_table
    proportion_of_shared_profiles <-
      table_configuration$proportion_of_shared_profiles
    table_shared_assignments <-
      floor(round(profiles_per_level * proportion_of_shared_profiles, 0)) -
      shared_profiles
  } else {
    panelists_per_table <- NA_integer_
    proportion_of_shared_profiles <- shared_profiles / profiles_per_level
    table_shared_assignments <- 0L
  }

  assignments <- tibble::tibble()

  if (table_shared_assignments > 0) {
    for (ii in seq_len(table_shared_assignments)) {
      for (jj in seq_along(raters)) {
        tmp_assignments <- possible_profiles |>
          dplyr::left_join(observed, by = att_vec) |>
          dplyr::filter(!is.na(!!rlang::sym(observed_count_label))) |>
          dplyr::filter(!!rlang::sym(observed_count_label) > 100) |>
          dplyr::mutate(size = 1L) |>
          slice_stratified(
            by = "total",
            size = "size",
            weight_by = observed_proportion_label
          ) |>
          dplyr::select(-dplyr::all_of(observed_count_label))

        assignments <- dplyr::bind_rows(
          assignments,
          tmp_assignments |>
            dplyr::mutate(table = raters[jj])
        )

        possible_profiles <- possible_profiles |>
          dplyr::anti_join(tmp_assignments, att_vec)

        possible_profiles <- calculate_hamming(
          possible_profiles,
          tmp_assignments |>
            dplyr::select(-"total"),
          att_vec
        )
        possible_profiles <- refine_possible_profiles(
          possible_profiles,
          filter_function = "median",
          raters = raters,
          profiles_per_level = profiles_per_level
        )
      }
    }

    assignments <- assignments |>
      tidyr::crossing(panelist = glue::glue("rater{1:panelists_per_table}"))
  }

  remaining_to_sample <- remaining_to_sample - table_shared_assignments

  if (!is.null(table_configuration)) {
    rater_dict <- tibble::tibble(table = raters) |>
      tidyr::crossing(panelist = glue::glue("rater{1:panelists_per_table}")) |>
      tibble::rowid_to_column("rater_num")
  } else {
    rater_dict <- tibble::tibble(table = NA, panelist = raters) |>
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
            weight_by = observed_proportion_label
          ) |>
          dplyr::select(-dplyr::all_of(observed_count_label))

        tmp_table <- rater_dict |>
          dplyr::filter(.data$rater_num == jj) |>
          dplyr::pull(.data$table)
        tmp_panelist <- rater_dict |>
          dplyr::filter(.data$rater_num == jj) |>
          dplyr::pull(.data$panelist)

        assignments <- dplyr::bind_rows(
          assignments,
          tmp_assignments |>
            dplyr::mutate(table = tmp_table, panelist = tmp_panelist)
        )

        possible_profiles <- possible_profiles |>
          dplyr::anti_join(tmp_assignments, att_vec)

        possible_profiles <- calculate_hamming(
          possible_profiles,
          tmp_assignments |>
            dplyr::select(-"total"),
          att_vec
        )
        possible_profiles <- refine_possible_profiles(
          possible_profiles,
          filter_function = "median",
          raters = raters,
          profiles_per_level = profiles_per_level
        )
      }
    }
  }

  if (!is.null(table_configuration)) {
    profile_sampling <- seen_by_all |>
      tidyr::crossing(
        table = raters,
        panelist = glue::glue("rater{1:panelists_per_table}")
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
        .data$table
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
      dplyr::select(-"table")
  }

  return(profile_sampling) # nolint
}
