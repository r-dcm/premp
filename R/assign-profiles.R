#' Title
#'
#' @param raters A vector containing the raters as character strings.
#' @param eligible_profiles A tibble with one row for each attribute mastery
#' profiles that is eligible for assignment to raters.
#' @param observed A tibble with one row for each attribute mastery profile that
#' was observed along with the number of times it was observed.
#' @param observed_id A character string for the field name of the observed
#' sample sizes in the observed parameter (default is 'n').
#' @param profiles_per_rater A numeric value indicating the number of profiles
#' to be rated by each rater.
#' @param raters_per_profile The minimum number of raters who need to rate each
#' #' profile.
#' @param profiles_seen_by_all The number of profiles that should be rated by
#' all raters (default is 1).
#' @param round The round number of the standard setting event.
#' @param num_pls The number of performance levels that can be assigned to any
#' profile.
#' @param range_of_profiles The increment of the total skills mastered, based on
#' the attribute mastery profiles, that should be retained for profile
#' assignment. This should only be provided for Round 1.
#' @param pinpoint_possible_profiles The csv of possible profiles to retain for
#' profile assignment in Round 2. This should only be provided for Round 2.
#' @param output_dir The output directory for the profile assignments.
#'
#' @returns A [tibble][tibble::tibble-package].
#'
#' @export
#' @examples
assign_profiles <- function(
  raters,
  eligible_profiles,
  observed,
  observed_id = "n",
  profiles_per_rater,
  raters_per_profile,
  profiles_seen_by_all = 1L,
  round,
  num_pls,
  range_of_profiles,
  pinpoint_possible_profiles,
  output_dir
) {
  # error checks
  ## range of profiles when round == 1
  ### no pinpoint_possible_profiles
  ## pinpoint_possible_profiles when round == 2
  ### add warning message when user supplies range_of_profiles in round 2

  if (round == 1 && is.null(range_of_profiles)) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(range_of_profiles),
      must = cli::format_message(paste(
        "must be provided for Round 1."
      ))
    )
  }

  if (round == 1 && !is.null(pinpoint_possible_profiles)) {

  }

  if (round == 2 && is.null(pinpoint_possible_profiles)) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(pinpoint_possible_profiles),
      must = cli::format_message(paste(
        "must be provided for Round 2."
      ))
    )
  }

  if (round == 2 && !is.null(range_of_profiles)) {

  }

  profiles_per_rater <- profiles_per_rater - profiles_seen_by_all
  total_ratings <- length(raters) * profiles_per_rater
  num_profiles <- floor(total_ratings / raters_per_profile)

  # remove unobserved profiles
  observed <- observed |>
    dplyr::mutate(!!rlang::sym(observed_id) :=
                    dplyr::case_when(is.na(!!rlang::sym(observed_id)) ~ 0,
                                     TRUE ~ n),
                  pct = !!rlang::sym(observed_id) /
                    sum(!!rlang::sym(observed_id))) |>
    dplyr::filter(!!rlang::sym(observed_id) != 0)

  eligible_profiles <- eligible_profiles |>
    # calculate total number of mastered attributes/skills
    dplyr::rowwise() |>
    dplyr::mutate(total = sum(dplyr::c_across(dplyr::everything()))) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$total, dplyr::across(dplyr::everything(),
                                              dplyr::desc)) |>
    # filter down to the total number in increments of the range of profiles
    dplyr::mutate(keep = .data$total %% range_of_profiles == 0) |>
    dplyr::filter(.data$keep) |>
    dplyr::select(-"keep")

  # apply weighted sampling design
  profile_sampling <- weighted_sampling(eligible_profiles, num_profiles,
                                        observed, round, num_pls)

  # stratified random sampling
  profiles_to_assign <- slice_stratified(profile_sampling,
                                         by = "total",
                                         size = "samples",
                                         weight_by = "pct")

  seen_by_all <- profiles_to_assign |>
    dplyr::slice_sample(n = profiles_seen_by_all)

  # pull vector of attribute names from profiles
  att_vec <- seen_by_all |>
    dplyr::select(-"total") |>
    names()

  # pull profiles assigned to be seen by all raters
  seen_by_all_assigments <- tibble::tibble(raters = raters,
                                           assign = 1) |>
    tidyr::pivot_wider(names_from = "raters", values_from = "assign")

  # add rater assignments to the tibble
  seen_by_all <- seen_by_all |>
    dplyr::bind_cols(seen_by_all_assigments)

  # create output tibble template for other rater assignments
  assignments <- tidyr::expand_grid(
    profile = seq_len(nrow(profiles_to_assign)),
    rater = raters,
    rated = 0L
  ) |>
    tidyr::pivot_wider(names_from = "rater", values_from = "rated") |>
    dplyr::select(-"profile") |>
    dplyr::bind_cols(profiles_to_assign) |>
    dplyr::select(!!!rlang::syms(att_vec), "total", !!!rlang::syms(raters)) |>
    dplyr::anti_join(seen_by_all, by = att_vec) |>
    dplyr::bind_rows(seen_by_all) |>
    dplyr::arrange(.data$total, dplyr::across(dplyr::everything(),
                                              dplyr::desc))

  # if assigning more profiles per rater than available assignments, assign
  # every profile to all raters
  if (profiles_per_rater >= nrow(assignments)) {
    final_assignments <- assignments |>
      tidyr::pivot_longer(cols = dplyr::all_of(raters), names_to = "raters",
                          values_to = "assign") |>
      dplyr::mutate(assign = 1) |>
      tidyr::pivot_wider(names_from = "raters", values_from = "assign")

  } else {
    # otherwise, randomly assign profiles to raters
    fail <- TRUE
    # add safeguards to prevent infinite loop
    attempt_num <- 1
    num_tries <- 1

    while (fail) {
      num_tries <- num_tries + 1

      # number of profiles to assign to each rater
      rater_assignments <- tibble(rater = raters,
                                  num_profiles = profiles_per_rater)

      # flag the profiles that are already being assigned to all raters and
      # remove so that this profile is not re-assigned
      seen_by_all_remove <- assignments |>
        tibble::rowid_to_column("profile") |>
        tidyr::pivot_longer(cols = dplyr::all_of(raters), names_to = "raters",
                            values_to = "assign") |>
        dplyr::filter(.data$assign == 1) |>
        tidyr::pivot_wider(names_from = "raters", values_from = "assign") |>
        dplyr::distinct(.data$profile)

      # calculate number of times to assign each remaining profile
      profile_assignments <- tibble(
        profile = 1:nrow(profiles_to_assign),
        assignments = ceiling(length(raters) * profiles_per_rater /
                                nrow(profiles_to_assign))
      ) |>
        dplyr::anti_join(seen_by_all_remove, by = "profile")

      # output template
      final_assignments = tibble::tibble(rater = NA,
                                         profile = NA)

      # for each profile to be assigned...
      for (ii in seq_len(sum(rater_assignments$num_profiles))) {
        # pull vector of least assigned raters so far
        poss_raters <- rater_assignments |>
          dplyr::filter(.data$num_profiles == max(.data$num_profiles)) |>
          dplyr::pull(.data$rater)
        # calculate even probability for each of the least assigned raters
        rater_probs <- rater_assignments |>
          dplyr::filter(.data$num_profiles == max(.data$num_profiles)) |>
          dplyr::mutate(prob = .data$num_profiles / sum(.data$num_profiles)) |>
          dplyr::pull(.data$prob)
        # pull vector of the least assigned profiles so far
        poss_profiles <- profile_assignments |>
          dplyr::filter(.data$assignments == max(.data$assignments)) |>
          dplyr::pull(.data$profile)
        # calculate even probability for each of the least assigned profiles
        profile_probs <- profile_assignments |>
          dplyr::filter(.data$assignments == max(.data$assignments)) |>
          dplyr::mutate(prob = .data$assignments / sum(.data$assignments)) |>
          dplyr::pull(.data$prob)

        # if more than one possible rater, randomly sample
        if (length(poss_raters) != 1) {
          rand_rater = sample(poss_raters, size = 1, prob = rater_probs)
        } else {
          rand_rater = poss_raters
        }
        # if more than one possible profile, randomly sample
        if (length(poss_profiles) != 1) {
          rand_profile = sample(poss_profiles, size = 1, prob = profile_probs)
        } else {
          rand_profile = poss_profiles
        }
        # save random samples as temporary assignment
        tmp_assign = tibble::tibble(rater = rand_rater, profile = rand_profile)
        # check to make sure this assignment hasn't already been made
        test_assignment <- dplyr::semi_join(final_assignments, tmp_assign,
                                            by = c("rater", "profile"))

        # iterate try count (prevents infinite loop)
        try_count <- 1

        # if the temporary assignment has already been made, start process over
        # after 15 attempts
        while(nrow(test_assignment) != 0 & try_count < 15) {
          # randomly sample another rater
          if (length(poss_raters) != 1) {
            rand_rater = sample(poss_raters, size = 1, prob = rater_probs)
          } else {
            rand_rater = poss_raters
          }
          # randomly sample another profile
          if (length(poss_profiles) != 1) {
            rand_profile = sample(poss_profiles, size = 1, prob = profile_probs)
          } else {
            rand_profile = poss_profiles
          }
          # repeat temporary assignment
          tmp_assign = tibble::tibble(rater = rand_rater,
                                      profile = rand_profile)
          # check to make sure this assignment hasn't already been made
          test_assignment <- dplyr::semi_join(final_assignments, tmp_assign,
                                              by = c("rater", "profile"))

          # iterate try count
          try_count <- try_count + 1
        }

        if (nrow(test_assignment) == 0 | num_tries == 7) {
          fail <- FALSE
        } else {
          # restart while loop if failing
          fail <- TRUE
          attempt_num <- attempt_num + 1
          break
        }

        # add temporary assignment to final assignment tibble
        final_assignments <- dplyr::bind_rows(final_assignments, tmp_assign)

        # decrement rater assignment counts with successful assignment
        rater_assignments <- rater_assignments |>
          dplyr::mutate(num_profiles = dplyr::case_when(.data$rater ==
                                                          rand_rater ~
                                                          num_profiles - 1,
                                                        TRUE ~ num_profiles)) |>
          dplyr::filter(num_profiles != 0)

        # decrement profile assignment counts with successful assignment
        profile_assignments <- profile_assignments |>
          dplyr::mutate(assignments = dplyr::case_when(.data$profile ==
                                                         rand_profile ~
                                                         assignments - 1,
                                                       TRUE ~ assignments)) |>
          dplyr::filter(assignments != 0)
      }
    }

    # remove blank row (stemming from defining original tibble)
    final_assignments <- final_assignments |>
      dplyr::filter(!is.na(.data$rater))

    # if still failing after 7 tries, remove duplicate assignments
    if (num_tries == 7) {
      final_assignments <- final_assignments |>
        dplyr::distinct()
    }

    # format final assignments in wide-format
    final_assignments <- final_assignments |>
      dplyr::mutate(assign = 1) |>
      tidyr::pivot_wider(names_from = "rater", values_from = "assign",
                         values_fill = 0) |>
      dplyr::arrange(.data$profile) |>
      dplyr::select(!!!rlang::syms(raters))

    # add attribute mastery profiles and totals to final assignments tibble
    final_assignments <- dplyr::bind_rows(seen_by_all |>
                                            dplyr::select(
                                              dplyr::all_of(raters)
                                            ),
                                          final_assignments) |>
      dplyr::bind_cols(assignments |>
                         dplyr::select(-dplyr::all_of(raters))) |>
      dplyr::select(dplyr::all_of(att_vec), "total", dplyr::all_of(raters))
  }

  # save output
  readr::write_csv(
    final_assignments,
    glue::glue("{output_dir}/profile_assignments_round_{round}.csv")
  )

  return(final_assignments)
}
