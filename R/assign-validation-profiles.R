#' Title
#'
#' @param fitted_model An object with the fitted machine learning model.
#' @param panelist_configuration A list containing the parameters for
#' configuring a panelist design. The allowable parameters are `num_panelists`
#' indicating the number of panelists and
#' `shared_profiles_within_certainty_across_panelists` indicating the number of
#' profiles that are common to all panelists. Only one of
#' `panelist_configuration` or `group_configuration` should be specified.
#' @param group_configuration A list containing parameters for configuring a
#' group design. The allowable parameters are `num_assignment_groups` indicating
#' the number of groups of panelists, `panelists_per_group` indicating
#' the number of panelists in each group, and
#' `shared_profiles_within_certainty_across_groups` indicating the number
#' of profiles within each certainty classification that are shared by panelists
#' across groups. Only one of `panelist_configuration` or `group_configuration`
#' should be specified.
#' @param certainty_assignments A list of integers defining the number of
#' profiles to assign for each performance level and each of the certainty
#' classifications (very_certain, fairly_certain, fairly_uncertain). The
#' default values are `very_certain = 1L`, `fairly_certain = 2L`, and
#' `fairly_uncertain = 3L`.
#' @param certainty_thresholds A list of proportions defining the threshold
#' between very certain and fairly certain (`very_certain`; default value of
#' .20) and the threshold between fairly certain and fairly uncertain
#' (`fairly_uncertain`; default value is .80). The certainty metric is
#' quantified by dividing the probability of the second most likely performance
#' level classification by the probability of the most likely performance level
#' classification. This means the default threshold of .20 separating very
#' certain from fairly certain reflects profiles where the most likely
#' performance level is at least five times the probability of the second most
#' likely performance level. Similarly, the default threshold of .80 separating
#' fairly certain from fairly uncertain reflects profiles where the most likely
#' performance level is no more than 1.25 times the probability of the second
#' most likely performance level.
#' @param observed A tibble with one row for each attribute mastery profile that
#' was observed along with the number of times it was observed.
#' @param observed_count_label A character string for the field name of the
#' observed sample sizes in `observed` (default is 'n').
#' @param included_total_levels_mastered The increment of the total levels
#' mastered, based on the attribute mastery profiles, that should be retained
#' during profile assignment.
#' @param att_levels A numeric value for the number of levels where mastery can
#' be demonstrated. For example, `att_level` is 1 for a dichotomous attribute
#' (i.e., nonmastery or mastery), and `att_level` is 2 for attributes where the
#' possible scores are 0, 1, and 2.
#' @param num_pls The number of performance levels that can be assigned to any
#' profile.
#' @param previously_rated A tibble with the profiles that have already been
#' assigned to panelists, where there is one row for each assigned profile.
#' @param output_dir The output directory for the profile assignments.
#'
#' @returns A [tibble][tibble::tibble-package].
#'
#' @export
#'
assign_validation_profiles <- function(
  fitted_model,
  panelist_configuration = NULL,
  group_configuration = NULL,
  certainty_assignments = list(
    very_certain = 1L,
    fairly_certain = 2L,
    fairly_uncertain = 3L
  ),
  certainty_thresholds = list(very_certain = .20, fairly_uncertain = .80),
  observed,
  observed_count_label = "n",
  included_total_levels_mastered,
  att_levels,
  num_pls,
  previously_rated,
  output_dir
) {
  # error checks
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
    if (typeof(group_configuration) != "list") {
      rdcmchecks::abort_bad_argument(
        arg = rlang::caller_arg(group_configuration),
        must = cli::format_message(paste(
          "must be a list."
        ))
      )
    }

    if (
      any(
        !(names(group_configuration) %in%
          c("num_assignment_groups",
            "panelists_per_group",
            "shared_profiles_within_certainty_across_groups"))
      )
    ) {
      rdcmchecks::abort_bad_argument(
        arg = rlang::caller_arg(group_configuration),
        must = cli::format_message(paste0(
          "must be a list containing `num_assignment_groups`, ",
          "`panelists_per_group`, and ",
          "`shared_profiles_within_certainty_across_groups`."
        ))
      )
    }

    if (length(group_configuration) != 3) {
      rdcmchecks::abort_bad_argument(
        arg = rlang::caller_arg(group_configuration),
        must = cli::format_message(paste(
          "must be a list of length 3."
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

    shared_profiles_within_certainty_across_groups <-
      group_configuration$shared_profiles_within_certainty_across_groups

    if (!is.integer(shared_profiles_within_certainty_across_groups)) {
      rdcmchecks::abort_bad_argument(
        arg = rlang::caller_arg(shared_profiles_within_certainty_across_groups),
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
            "shared_profiles_within_certainty_across_panelists"))
      )
    ) {
      rdcmchecks::abort_bad_argument(
        arg = rlang::caller_arg(panelist_configuration),
        must = cli::format_message(paste0(
          "must be a list containing `num_panelists` and ",
          "`shared_profiles_within_certainty_across_panelists`."
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

    shared_profiles_within_certainty_across_groups <-
      panelist_configuration$shared_profiles_within_certainty_across_panelists

    if (!is.integer(shared_profiles_within_certainty_across_groups)) {
      rdcmchecks::abort_bad_argument(
        arg = "shared_profiles_within_certainty_across_panelists",
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

  if (typeof(certainty_assignments) != "list") {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(certainty_assignments),
      must = cli::format_message(paste(
        "must be a list."
      ))
    )
  }

  if (
    any(
      !(names(certainty_assignments) %in%
        c("very_certain", "fairly_certain", "fairly_uncertain"))
    )
  ) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(certainty_assignments),
      must = cli::format_message(paste0(
        "must be a list containing `very_certain`, `fairly_certain` and ",
        "`fairly_uncertain`."
      ))
    )
  }

  if (length(certainty_assignments) != 3) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(certainty_assignments),
      must = cli::format_message(paste(
        "must be a list of length 3."
      ))
    )
  }

  very_certain_assignments <- certainty_assignments$very_certain
  fairly_certain_assignments <- certainty_assignments$fairly_certain
  fairly_uncertain_assignments <- certainty_assignments$fairly_uncertain

  if (typeof(very_certain_assignments) != "integer") {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(very_certain_assignments),
      must = cli::format_message(paste(
        "must be an integer value."
      ))
    )
  }

  if (typeof(fairly_certain_assignments) != "integer") {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(fairly_certain_assignments),
      must = cli::format_message(paste(
        "must be an integer value."
      ))
    )
  }

  if (typeof(fairly_uncertain_assignments) != "integer") {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(fairly_uncertain_assignments),
      must = cli::format_message(paste(
        "must be an integer value."
      ))
    )
  }

  if (typeof(certainty_thresholds) != "list") {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(certainty_thresholds),
      must = cli::format_message(paste(
        "must be a list."
      ))
    )
  }

  if (
    any(
      !(names(certainty_thresholds) %in%
        c("very_certain", "fairly_uncertain"))
    )
  ) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(certainty_thresholds),
      must = cli::format_message(paste0(
        "must be a list containing `very_certain` and `fairly_uncertain`."
      ))
    )
  }

  if (length(certainty_thresholds) != 2) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(certainty_thresholds),
      must = cli::format_message(paste(
        "must be a list of length 2."
      ))
    )
  }

  very_certain_threshold <- certainty_thresholds$very_certain
  fairly_uncertain_threshold <- certainty_thresholds$fairly_uncertain

  if (typeof(very_certain_threshold) != "double" ||
      very_certain_threshold < 0 ||
      very_certain_threshold > 1) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(very_certain_assignments),
      must = cli::format_message(paste(
        "must be a double value between 0 and 1."
      ))
    )
  }

  if (typeof(fairly_uncertain_threshold) != "double" ||
      fairly_uncertain_threshold < 0 ||
      fairly_uncertain_threshold > 1) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(fairly_uncertain_threshold),
      must = cli::format_message(paste(
        "must be a double value between 0 and 1."
      ))
    )
  }

  if (fairly_uncertain_threshold < very_certain_threshold) {
    rdcmchecks::abort_bad_argument(
      arg = "fairly_uncertain",
      must = cli::format_message(paste(
        "must be greater than `very_certain`"
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

  if (typeof(included_total_levels_mastered) != "integer" ||
      !is.vector(included_total_levels_mastered)) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(included_total_levels_mastered),
      must = cli::format_message(paste(
        "must be a vector of integer values."
      ))
    )
  }

  observed <- observed |>
    dplyr::mutate(prop = !!rlang::sym(observed_count_label) /
                    sum(!!rlang::sym(observed_count_label)))

  group_design <- ifelse(is.null(group_configuration), FALSE, TRUE)

  if (group_design) {
    raters <- glue::glue("group{1:num_assignment_groups}")
    rater_name = "group_id"
  } else {
    raters <- glue::glue("rater{1:num_assignment_groups}")
    rater_name = "rater_id"
  }

  possible_profiles <- observed |>
    dplyr::select(
      -!!rlang::sym(observed_count_label),
      -"prop"
    ) |>
    dplyr::anti_join(previously_rated, by = "profile_id") |>
    dplyr::rowwise() |>
    dplyr::mutate(total = sum(dplyr::c_across(-c("profile_id")))) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$total %in% included_total_levels_mastered)

  model_preds <- assign_pl(
    fitted_model,
    possible_profiles |>
      dplyr::select(-"profile_id"),
    att_levels = att_levels,
    num_pls = num_pls,
    rating_id = "rating",
    output_dir = output_dir
  )

  suppressMessages(
    certainty_classifications <- model_preds |>
      dplyr::left_join(possible_profiles) |>
      dplyr::select("profile_id", "performance_level" = "pred_pl",
                    dplyr::starts_with("prob_pl")) |>
      tidyr::pivot_longer(cols = dplyr::starts_with("prob_pl"),
                          names_to = "pl",
                          values_to = "prob") |>
      dplyr::group_by(.data$profile_id) |>
      dplyr::arrange(.data$profile_id, dplyr::desc(.data$prob)) |>
      dplyr::mutate(profile_num = dplyr::row_number()) |>
      dplyr::ungroup() |>
      dplyr::filter(.data$profile_num <= 2) |>
      dplyr::mutate(profile_num = dplyr::case_when(.data$profile_num == 1 ~
                                                     "most_likely",
                                                   .data$profile_num == 2 ~
                                                     "second_most_likely")) |>
      dplyr::select(-"pl") |>
      tidyr::pivot_wider(names_from = "profile_num", values_from = "prob") |>
      dplyr::mutate(uncertainty = .data$second_most_likely /
                      .data$most_likely) |>
      dplyr::select(-"most_likely", -"second_most_likely") |>
      dplyr::mutate(
        certainty_cat = dplyr::case_when(
          .data$uncertainty <= very_certain_threshold ~ "very certain",
          .data$uncertainty >= fairly_uncertain_threshold ~ "fairly uncertain",
          TRUE ~ "fairly certain"
        )
      ) |>
      dplyr::select(-"uncertainty")
  )

  num_very_certain_assignments <- very_certain_assignments *
    num_assignment_groups
  num_fairly_certain_assignments <- fairly_certain_assignments *
    num_assignment_groups
  num_fairly_uncertain_assignments <- fairly_uncertain_assignments *
    num_assignment_groups

  # very certain assignments
  very_certain_profile_assignments <- certainty_classifications |>
    dplyr::filter(.data$certainty_cat == "very certain") |>
    dplyr::select(-"certainty_cat") |>
    dplyr::group_by(.data$performance_level) |>
    dplyr::slice_sample(n = num_very_certain_assignments)

  short_very_certain_counts <- very_certain_profile_assignments |>
    dplyr::count(.data$performance_level) |>
    dplyr::filter(.data$n < num_very_certain_assignments) |>
    dplyr::mutate(needed = num_very_certain_assignments - .data$n) |>
    dplyr::select(-"n")

  if (nrow(short_very_certain_counts) > 0) {
    replacement_sampling_very_certain <- certainty_classifications |>
      dplyr::filter(.data$certainty_cat == "very certain") |>
      dplyr::semi_join(short_very_certain_counts, by = "performance_level") |>
      dplyr::left_join(short_very_certain_counts, by = "performance_level") |>
      dplyr::select(-"certainty_cat") |>
      dplyr::group_by(.data$performance_level) |>
      dplyr::slice_sample(n = num_very_certain_assignments, replace = TRUE) |>
      tibble::rowid_to_column("row_num") |>
      dplyr::filter(.data$row_num <= .data$needed) |>
      dplyr::select("profile_id", "performance_level")

    very_certain_profile_assignments <- very_certain_profile_assignments |>
      dplyr::bind_rows(replacement_sampling_very_certain) |>
      dplyr::arrange(.data$performance_level)
  }

  very_certain_profile_assignments <- very_certain_profile_assignments |>
    dplyr::ungroup() |>
    dplyr::mutate(!!rlang::sym(rater_name) :=
                    rep(raters,
                        times = num_pls * very_certain_assignments))

  # fairly certain assignments
  fairly_certain_profile_assignments <- certainty_classifications |>
    dplyr::filter(.data$certainty_cat == "fairly certain") |>
    dplyr::select(-"certainty_cat") |>
    dplyr::group_by(.data$performance_level) |>
    dplyr::slice_sample(n = num_fairly_certain_assignments)

  short_fairly_certain_counts <- fairly_certain_profile_assignments |>
    dplyr::count(.data$performance_level) |>
    dplyr::filter(.data$n < num_fairly_certain_assignments) |>
    dplyr::mutate(needed = num_fairly_certain_assignments - .data$n) |>
    dplyr::select(-"n")

  if (nrow(short_fairly_certain_counts) > 0) {
    replacement_sampling_fairly_certain <- certainty_classifications |>
      dplyr::filter(.data$certainty_cat == "fairly certain") |>
      dplyr::semi_join(short_fairly_certain_counts, by = "performance_level") |>
      dplyr::left_join(short_fairly_certain_counts, by = "performance_level") |>
      dplyr::select(-"certainty_cat") |>
      dplyr::group_by(.data$performance_level) |>
      dplyr::slice_sample(n = num_fairly_certain_assignments, replace = TRUE) |>
      tibble::rowid_to_column("row_num") |>
      dplyr::filter(.data$row_num <= .data$needed) |>
      dplyr::select("profile_id", "performance_level")

    fairly_certain_profile_assignments <- fairly_certain_profile_assignments |>
      dplyr::bind_rows(replacement_sampling_fairly_certain) |>
      dplyr::arrange(.data$performance_level)
  }

  fairly_certain_profile_assignments <- fairly_certain_profile_assignments |>
    dplyr::ungroup() |>
    dplyr::mutate(!!rlang::sym(rater_name) :=
                    rep(raters,
                        times = num_pls * fairly_certain_assignments))

  # fairly uncertain assignments
  fairly_uncertain_profile_assignments <- certainty_classifications |>
    dplyr::filter(.data$certainty_cat == "fairly uncertain") |>
    dplyr::select(-"certainty_cat") |>
    dplyr::group_by(.data$performance_level) |>
    dplyr::slice_sample(n = num_fairly_uncertain_assignments)

  short_fairly_uncertain_counts <- fairly_uncertain_profile_assignments |>
    dplyr::count(.data$performance_level) |>
    dplyr::filter(.data$n < num_fairly_uncertain_assignments) |>
    dplyr::mutate(needed = num_fairly_uncertain_assignments - .data$n) |>
    dplyr::select(-"n")

  if (nrow(short_fairly_uncertain_counts) > 0) {
    replacement_sampling_fairly_uncertain <- certainty_classifications |>
      dplyr::filter(.data$certainty_cat == "fairly uncertain") |>
      dplyr::semi_join(short_fairly_uncertain_counts,
                       by = "performance_level") |>
      dplyr::left_join(short_fairly_uncertain_counts,
                       by = "performance_level") |>
      dplyr::select(-"certainty_cat") |>
      dplyr::group_by(.data$performance_level) |>
      dplyr::slice_sample(n = num_fairly_uncertain_assignments,
                          replace = TRUE) |>
      tibble::rowid_to_column("row_num") |>
      dplyr::filter(.data$row_num <= .data$needed) |>
      dplyr::select("profile_id", "performance_level")

    fairly_uncertain_profile_assignments <-
      fairly_uncertain_profile_assignments |>
      dplyr::bind_rows(replacement_sampling_fairly_uncertain) |>
      dplyr::arrange(.data$performance_level)
  }

  fairly_uncertain_profile_assignments <-
    fairly_uncertain_profile_assignments |>
    dplyr::ungroup() |>
    dplyr::mutate(!!rlang::sym(rater_name) :=
                    rep(raters,
                        times = num_pls * fairly_uncertain_assignments))

  # combine assignments
  profile_sampling <- very_certain_profile_assignments |>
    dplyr::bind_rows(fairly_certain_profile_assignments) |>
    dplyr::bind_rows(fairly_uncertain_profile_assignments)

  if (!is.null(group_configuration)) {
    profile_sampling <- profile_sampling |>
      tidyr::crossing(
        rater_id = glue::glue(
          "rater{1:group_configuration$panelists_per_group}"
        )
    ) |>
    dplyr::mutate(assigned = 1L) |>
    tidyr::pivot_wider(names_from = "rater_id", values_from = "assigned")
  }

  # save output
  readr::write_csv(
    profile_sampling,
    glue::glue("{output_dir}/validation_profile_assignments.csv")
  )

  profile_sampling
}
