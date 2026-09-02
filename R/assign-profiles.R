#' Title
#'
#' @param num_assignment_groups The number of "assignment groups" for the
#' profile assignments. When `table_design` is FALSE, the number of assignment
#' groups is the number of panelists. When `table_design` is TRUE, the number of
#' assignment groups is the number of tables of panelists.
#' @param table_configuration A list containing parameters for configuring a
#' table design. The allowable parameters are `panelists_per_table` indicating
#' the number of panelists at each table and `proportion_of_shared_profiles`
#' indicating the proportion of profiles that are common to all of the panelists
#' at each table.
#' @param observed A tibble with one row for each attribute mastery profile that
#' was observed along with the number of times it was observed.
#' @param observed_count_label A character string for the field name of the
#' observed sample sizes in `observed` (default is 'n').
#' @param observed_proportion_label A character string for the field name of the
#' observed proportions in `observed` (default is 'prop').
#' @param shared_profiles The number of profiles that are shared by all
#' panelists (default is 1). With a table-based design, this is the number of
#' profiles that are seen by all tables. With a panelist-based design, this is
#' the number of profiles seen by all panelists.
#' @param included_totals The increment of the total skills mastered, based on
#' the attribute mastery profiles, that should be retained during profile
#' assignment.
#' @param profiles_per_level An integer specifying the number of profiles to
#' assign to each rater at each level of the total skills mastered.
#' @param assigned_profiles A tibble with the profiles that have already been
#' assigned to panelists, where there is one row for each assigned profile.
#' @param output_dir The output directory for the profile assignments.
#'
#' @returns A [tibble][tibble::tibble-package].
#'
#' @export
assign_profiles <- function(
  num_assignment_groups,
  table_configuration = NULL,
  observed,
  observed_count_label = "n",
  observed_proportion_label = "prop",
  shared_profiles = 1L,
  included_totals,
  profiles_per_level,
  assigned_profiles = NULL,
  output_dir
) {
  # error checks
  if (!is.integer(num_assignment_groups)) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(num_assignment_groups),
      must = cli::format_message(paste(
        "must be an integer."
      ))
    )
  }

  if (!is.null(table_configuration)) {
    if (typeof(table_configuration) != "list") {
      rdcmchecks::abort_bad_argument(
        arg = rlang::caller_arg(table_configuration),
        must = cli::format_message(paste(
          "must be a list."
        ))
      )
    }

    if (
      any(
        !(names(table_configuration) %in%
          c("panelists_per_table", "proportion_of_shared_profiles"))
      )
    ) {
      rdcmchecks::abort_bad_argument(
        arg = rlang::caller_arg(table_configuration),
        must = cli::format_message(paste0(
          "must be a list containing `panelists_per_table` and ",
          "`proportion_of_shared_profiles`."
        ))
      )
    }

    if (length(table_configuration) != 2) {
      rdcmchecks::abort_bad_argument(
        arg = rlang::caller_arg(table_configuration),
        must = cli::format_message(paste(
          "must be a list of length 2."
        ))
      )
    }

    panelists_per_table <- table_configuration$panelists_per_table

    if (!is.integer(panelists_per_table)) {
      rdcmchecks::abort_bad_argument(
        arg = rlang::caller_arg(panelists_per_table),
        must = cli::format_message(paste(
          "must be an integer."
        ))
      )
    }

    proportion_of_shared_profiles <-
      table_configuration$proportion_of_shared_profiles

    if (
      !is.double(proportion_of_shared_profiles) ||
        proportion_of_shared_profiles < 0 ||
        proportion_of_shared_profiles > 1
    ) {
      rdcmchecks::abort_bad_argument(
        arg = rlang::caller_arg(proportion_of_shared_profiles),
        must = cli::format_message(paste(
          "must be a double value between 0 and 1."
        ))
      )
    }
  }

  if (!is.character(observed_count_label)) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(observed_count_label),
      must = cli::format_message(paste(
        "must be a character string."
      ))
    )
  }

  if (typeof(included_totals) != "integer" || !is.vector(included_totals)) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(included_totals),
      must = cli::format_message(paste(
        "must be a vector of integer values."
      ))
    )
  }

  if (!is.integer(profiles_per_level)) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(profiles_per_level),
      must = cli::format_message(paste(
        "must be an integer."
      ))
    )
  }

  unobserved_in_observed <- observed |>
    dplyr::filter(
      !!rlang::sym(observed_count_label) == 0 |
        is.na(!!rlang::sym(observed_count_label))
    ) |>
    nrow() >
    0

  if (unobserved_in_observed) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(profiles_per_level),
      must = cli::format_message(paste(
        "should only include profiles that were observed."
      ))
    )
  }

  if (!is.integer(shared_profiles)) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(shared_profiles),
      must = cli::format_message(paste(
        "must be an integer."
      ))
    )
  }

  table_design <- ifelse(is.null(table_configuration), FALSE, TRUE)

  if (table_design) {
    raters <- glue::glue("table{1:num_assignment_groups}")
  } else {
    raters <- glue::glue("rater{1:num_assignment_groups}")
  }

  if (!is.null(assigned_profiles)) {
    att_vec <- observed |>
      dplyr::select(
        -!!rlang::sym(observed_count_label),
        -!!rlang::sym(observed_proportion_label)
      ) |>
      names()

    possible_profiles <- observed |>
      dplyr::select(
        -!!rlang::sym(observed_count_label),
        -!!rlang::sym(observed_proportion_label)
      ) |>
      dplyr::anti_join(assigned_profiles, by = att_vec)
  } else {
    possible_profiles <- observed |>
      dplyr::select(
        -!!rlang::sym(observed_count_label),
        -!!rlang::sym(observed_proportion_label)
      )
  }

  possible_profiles <- possible_profiles |>
    # calculate total number of mastered attributes/skills
    dplyr::rowwise() |>
    dplyr::mutate(total = sum(dplyr::c_across(dplyr::everything()))) |>
    dplyr::ungroup() |>
    dplyr::arrange(
      .data$total,
      dplyr::across(dplyr::everything(), dplyr::desc)
    ) |>
    # filter down to the total number in increments of the range of profiles
    dplyr::filter(.data$total %in% included_totals)

  # apply weighted sampling design
  profile_sampling <- weighted_sampling(
    possible_profiles,
    observed,
    observed_count_label,
    observed_proportion_label,
    shared_profiles,
    profiles_per_level,
    raters,
    table_configuration
  )

  # save output
  readr::write_csv(
    profile_sampling,
    glue::glue("{output_dir}/profile_assignments.csv")
  )

  profile_sampling
}
