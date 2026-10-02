#' Title
#'
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
#' @param observed A tibble with one row for each attribute mastery profile that
#' was observed along with the number of times it was observed.
#' @param observed_count_label A character string for the field name of the
#' observed sample sizes in `observed` (default is 'n').
#' @param included_total_levels_mastered The increment of the total levels
#' mastered, based on the attribute mastery profiles, that should be retained
#' during profile assignment.
#' @param profiles_per_total_levels_mastered An integer specifying the number of
#' profiles to assign to each rater at each total number of levels mastered.
#' @param previously_rated A tibble with the profiles that have already been
#' assigned to panelists, where there is one row for each assigned profile.
#' @param output_dir The output directory for the profile assignments.
#'
#' @returns A [tibble][tibble::tibble-package].
#'
#' @export
assign_profiles <- function(
  panelist_configuration = NULL,
  group_configuration = NULL,
  observed,
  observed_count_label = "n",
  included_total_levels_mastered,
  profiles_per_total_levels_mastered,
  previously_rated = NULL,
  output_dir
) {
  # error checks
  if (!is.null(group_configuration)) {
    if (typeof(group_configuration) != "list") {
      rdcmchecks::abort_bad_argument(
        arg = rlang::caller_arg(group_configuration),
        must = cli::format_message(paste(
          "must be a list."
        ))
      )
    }
  }

  if (!is.null(panelist_configuration)) {
    num_assignment_groups <- panelist_configuration$num_panelists
  } else {
    num_assignment_groups <- group_configuration$num_assignment_groups
  }

  if (!is.integer(num_assignment_groups)) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(num_assignment_groups),
      must = cli::format_message(paste(
        "must be an integer."
      ))
    )
  }

  if (!is.null(group_configuration)) {
    if (length(group_configuration) != 4) {
      rdcmchecks::abort_bad_argument(
        arg = rlang::caller_arg(group_configuration),
        must = cli::format_message(paste(
          "must be a list of length 4."
        ))
      )
    }

    if (
      any(
        !(names(group_configuration) %in%
          c(
            "num_assignment_groups",
            "panelists_per_group",
            "shared_within",
            "shared_across"
          ))
      )
    ) {
      rdcmchecks::abort_bad_argument(
        arg = rlang::caller_arg(group_configuration),
        must = cli::format_message(paste0(
          "must be a list containing `num_assignment_groups`, ",
          "`panelists_per_group`, ",
          "`shared_within`, and ",
          "`shared_across`."
        ))
      )
    }

    panelists_per_group <- group_configuration$panelists_per_group

    if (!is.integer(panelists_per_group)) {
      rdcmchecks::abort_bad_argument(
        arg = rlang::caller_arg(panelists_per_group),
        must = cli::format_message(paste(
          "must be an integer."
        ))
      )
    }

    shared_within <-
      group_configuration$shared_within

    if (!is.integer(shared_within)) {
      rdcmchecks::abort_bad_argument(
        arg = rlang::caller_arg(shared_within),
        must = cli::format_message(paste(
          "must be an integer."
        ))
      )
    }

    shared_across <-
      group_configuration$shared_across

    if (!is.integer(shared_across)) {
      rdcmchecks::abort_bad_argument(
        arg = rlang::caller_arg(shared_across),
        must = cli::format_message(paste(
          "must be an integer."
        ))
      )
    }
  }

  if (!is.null(panelist_configuration)) {
    if (typeof(panelist_configuration) != "list") {
      rdcmchecks::abort_bad_argument(
        arg = rlang::caller_arg(panelist_configuration),
        must = cli::format_message(paste(
          "must be a list."
        ))
      )
    }

    if (
      any(
        !(names(panelist_configuration) %in%
          c("num_panelists",
            "shared_across"))
      )
    ) {
      rdcmchecks::abort_bad_argument(
        arg = rlang::caller_arg(group_configuration),
        must = cli::format_message(paste0(
          "must be a list containing `num_panelists` and ",
          "`shared_across`."
        ))
      )
    }

    if (length(panelist_configuration) != 2) {
      rdcmchecks::abort_bad_argument(
        arg = rlang::caller_arg(panelist_configuration),
        must = cli::format_message(paste(
          "must be a list of length 2."
        ))
      )
    }

    shared_across <-
      panelist_configuration$shared_across

    if (!is.integer(shared_across)) {
      rdcmchecks::abort_bad_argument(
        arg = rlang::caller_arg(shared_across),
        must = cli::format_message(paste(
          "must be an integer."
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

  if (typeof(included_total_levels_mastered) != "integer" ||
      !is.vector(included_total_levels_mastered)) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(included_total_levels_mastered),
      must = cli::format_message(paste(
        "must be a vector of integer values."
      ))
    )
  }

  if (!is.integer(profiles_per_total_levels_mastered)) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(profiles_per_total_levels_mastered),
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
      arg = rlang::caller_arg(observed),
      must = cli::format_message(paste(
        "should only include profiles that were observed."
      ))
    )
  }

  observed <- observed |>
    dplyr::mutate(prop = !!rlang::sym(observed_count_label) /
                    sum(!!rlang::sym(observed_count_label)))

  group_design <- ifelse(is.null(group_configuration), FALSE, TRUE)

  if (group_design) {
    raters <- glue::glue("group{1:num_assignment_groups}")
  } else {
    raters <- glue::glue("rater{1:num_assignment_groups}")
  }

  if (!is.null(previously_rated)) {
    possible_profiles <- observed |>
      dplyr::select(
        -!!rlang::sym(observed_count_label),
        -"prop"
      ) |>
      dplyr::anti_join(previously_rated, by = "profile_id")
  } else {
    possible_profiles <- observed |>
      dplyr::select(
        -!!rlang::sym(observed_count_label),
        -"prop"
      )
  }

  possible_profiles <- possible_profiles |>
    # calculate total number of mastered attributes/skills
    dplyr::rowwise() |>
    dplyr::mutate(total = sum(dplyr::c_across(-c("profile_id")))) |>
    dplyr::ungroup() |>
    dplyr::arrange(
      .data$total,
      dplyr::across(dplyr::everything(), dplyr::desc)
    ) |>
    # filter down to the total number in increments of the range of profiles
    dplyr::filter(.data$total %in% included_total_levels_mastered)

  # apply weighted sampling design
  profile_sampling <- weighted_sampling(
    possible_profiles,
    observed,
    observed_count_label,
    profiles_per_total_levels_mastered,
    raters,
    panelist_configuration,
    group_configuration
  )

  # filter down to assignments -- profile id, group (when applicable), panelists
  # identify attributes
  att_vec <- possible_profiles |>
    dplyr::select(-"total", -"profile_id") |>
    names()

  profile_sampling <- profile_sampling |>
    dplyr::select(-dplyr::any_of(att_vec), -"total")

  # save output
  readr::write_csv(
    profile_sampling,
    glue::glue("{output_dir}/profile_assignments.csv")
  )

  profile_sampling
}
