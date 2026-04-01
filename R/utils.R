#' Stratified Sampling
#'
#' Define a function for stratified sampling that allows for specifying the
#' number of samples to take within each stratification while weighting the
#' probability of sampling each case.
#'
#' @param x A tibble with one row for each rater's rating of an assigned
#' profile.
#' @param by A character field that defines the stratifications.
#' @param size A character field that defines the sampling sizes within each
#' stratification.
#' @param weight_by A character field that defines the sampling weights within
#' each stratification.
#'
#' @return [tibble][tibble::tibble-package] A tibble containing the pinpoint
#' ranges to assign profiles during the second round of standard setting.
#'
#' @export
slice_stratified <- function(x, by, size, weight_by = NULL) {
  profiles_to_assign <- tibble::tibble()
  total_levels <- x |>
    dplyr::distinct(!!rlang::sym(by)) |>
    dplyr::pull(!!rlang::sym(by))

  for (ii in total_levels) {
    tmp <- x |>
      dplyr::filter(!!rlang::sym(by) == ii)

    to_sample <- tmp |>
      dplyr::distinct(!!rlang::sym(size)) |>
      dplyr::pull(!!rlang::sym(size))
    tmp <- tmp |>
      ratlas::only_if(!is.null(weight_by))(dplyr::slice_sample)(
        n = to_sample, weight_by = !!rlang::sym(weight_by)
      ) |>
      ratlas::only_if(is.null(weight_by))(dplyr::slice_sample)(n = to_sample) |>
      ratlas::only_if(!is.null(weight_by))(dplyr::select)(
        -!!rlang::sym(size), -!!rlang::sym(weight_by)
      ) |>
      ratlas::only_if(is.null(weight_by))(dplyr::select)(-!!rlang::sym(size))

    profiles_to_assign <- dplyr::bind_rows(profiles_to_assign, tmp)
  }

  return(profiles_to_assign) # nolint
}

#' Fit Standard Setting Logistic Regression Model
#'
#' Estimate a logistic regression to predict the performance level from the
#' total number of linkage levels mastered in each profile.
#'
#' @param dat A data frame with 1 row per profile per rating. There are two
#'   required columns:
#'   1. `y`: A 0/1 indicator representing whether that profile was rated in the
#'      performance level of interest or higher. That is, if calculating the cut
#'      point between levels 1 and 2, was the profile rated in level 2, 3, or 4?
#'   2. `atts_mastered`: The total number of attributes mastered in the
#'      profile that was rated.
#' @param iter The number of iterations in the posterior distribution
#' (default = 4,000).
#' @param warmup The number of warm-up iterations (default = 2,000).
#' @param cores The number of cores (default = 4).
#' @param chains The number of chains (default = 4).
#' @param refresh The interval between refreshing the visual display during
#' model estimation (default = 0, no refresh).
#' @param adapt_delta The likelihood of accepting the next step in traversing
#' the posterior distribution, which is related to step size (default = .95).
#' @param max_treedepth The maximum depth before making a U-turn when traversing
#' the posterior distribution (default = 15).
#'
#' @details
#' This function currently uses [brms::brm()] to estimate the logistic
#' regression, but any logistic function would do (e.g., [stats::glm()]). Note
#' that change the estimation engine would require down-stream adjustments as
#' well, as the post-processing and predictions currently assume that posterior
#' distributions will need to be summarized.
#'
#' @return A tibble with 1 row and 2 columns: `intercept` and `slope`. The one
#'   row contains objects of type [posterior::draws_rvars], which represent the
#'   posterior distribution for each parameter.
#'
#' @export
fit_model <- function(
  dat,
  iter = 4000,
  warmup = 2000,
  cores = 4,
  chains = 4,
  refresh = 0,
  adapt_delta = .95,
  max_treedepth = 15
) {
  suppressMessages(
    mod <- brms::brm(y ~ 1 + atts_mastered, data = dat, family = "bernoulli",
                     prior = c(brms::prior("normal(0, 1.5)",
                                           class = "Intercept"),
                               brms::prior("normal(0, 0.5)",
                                           class = "b")),
                     iter = iter, warmup = warmup, chains = chains,
                     cores = cores, refresh = refresh,
                     control = list(adapt_delta = adapt_delta,
                                    max_treedepth = max_treedepth))
  )

  posterior::as_draws_rvars(mod,
                            variable = c("b_Intercept", "b_atts_mastered")) |>
    tibble::as_tibble() |>
    dplyr::rename(intercept = "b_Intercept", slope = "b_atts_mastered")
}


#' Ensure a maximum value
#'
#' This is a utility function to ensure functions execute correctly. For
#' example, when calculating Z-score, a cumulative percent of 1.0 is undefined.
#' This function can be used to replace all of the 1.0 with some other value,
#' (e.g., 0.9999) to ensure that a Z-score is calculated for each score point.
#'
#' @param x A double that should be capped at a specified value.
#' @param max_val The maximum value that `x` should be allowed to take.
#'
#' @return A double no larger than `max_val`.
max_value <- function(
  x,
  max_val = 0.9999
) {
  min(x, max_val)
}

#' Calculate Hamming distance
#'
#' This is a utility function to calculate the Hamming distance between the last
#' assigned profile and the remaining profiles that are eligible for assignment.
#'
#' @param profiles A tibble containing the profiles that are eligible to be
#' assigned to panelists. This tibble should have fields for each of the
#' attributes and one field (`total`) for the total number of attributes
#' mastered.
#' @param assigned_profiles A tibble containing the assigned profiles. This
#' tibble should only contain fields for each of the attribute.
#' @param att_vec A character vector containing the attribute names.
#'
#' @return A tibble with the eligible profiles and the Hamming distance.
calculate_hamming <- function(
  profiles,
  assigned_profiles,
  att_vec
) {
  att_levels <- profiles |>
    dplyr::filter(.data$total != 0) |>
    dplyr::distinct(.data$total) |>
    dplyr::pull(.data$total)

  hamming_dist <- tibble::tibble()

  for (aa in att_levels) {
    ham_prof <- assigned_profiles |>
      dplyr::mutate(total = rowSums(dplyr::across(dplyr::where(is.numeric)))) |>
      dplyr::filter(.data$total == aa) |>
      dplyr::select(-"total")

    tmp_hamming_dist <- profiles |>
      dplyr::filter(.data$total == aa) |>
      tibble::rowid_to_column("prof_num") |>
      tidyr::pivot_longer(cols = c(-"prof_num", -"total"),
                          names_to = "att",
                          values_to = "score") |>
      dplyr::left_join(ham_prof |>
                         tidyr::pivot_longer(cols = dplyr::everything(),
                                             names_to = "att",
                                             values_to = "score") |>
                         dplyr::rename(orig_score = "score"),
                       by = "att") |>
      dplyr::group_by(.data$prof_num) |>
      dplyr::mutate(ham_distance = abs(.data$score - .data$orig_score),
                    ham_distance = sum(.data$ham_distance)) |>
      dplyr::ungroup() |>
      dplyr::select("prof_num", "att", "score", "total", "ham_distance") |>
      tidyr::pivot_wider(names_from = "att", values_from = "score") |>
      dplyr::select(dplyr::all_of(att_vec), "total",
                    "hamming_distance" = "ham_distance")

    hamming_dist <- dplyr::bind_rows(hamming_dist, tmp_hamming_dist)
  }

  return(hamming_dist) # nolint
}

#' Refine Profiles to Minimize Similarity of the Assigned Profiles
#'
#' This is a utility function to filter out profiles that are most similar to
#' the most recently assigned profile.
#'
#' @param profiles A tibble containing the profiles that are eligible to be
#' assigned to panelists. This tibble should have fields for each of the
#' attributes and one field (`total`) for the total number of attributes
#' mastered.
#' @param filter_function The character string of the function to use for
#' filtering profiles. The supported options are `median` and `mean`.
#' @param filter_percentile A numeric value ranging from 0 to 1 to indicate the
#' percentile used to filter out similar profiles (default is `NULL`). For
#' example, a value of .60 indicates profiles with a Hamming distance below the
#' 60th percentile will be filtered out of the set of eligible profiles.
#' @param raters A character vector containing the rater names.
#' @param profiles_per_level An integer value indicating the number of profiles
#' to sample from each level of the number of attributes mastered.
#'
#' @return A tibble with the eligible profiles and the Hamming distance.
refine_eligible_profiles <- function(
  profiles,
  filter_function = "median",
  filter_percentile = NULL,
  raters,
  profiles_per_level
) {
  if (!is.null(filter_function) && !is.null(filter_percentile)) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(filter_percentile),
      must = cli::format_message(paste(
        "must not be provided in addition to `filter_function`."
      ))
    )
  }

  if (!is.null(filter_percentile) &&
        (filter_percentile < 0 || filter_percentile > 1)) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(filter_percentile),
      must = cli::format_message(paste(
        "must be between 0 and 1."
      ))
    )
  }

  # don't refine eligible profiles if the refinement pushes the number eligible
  # below the number that needs to be sampled
  sx_threshold <- length(raters) * profiles_per_level * 3 # nolint

  if (!is.null(filter_percentile)) {
    profiles <- profiles |>
      dplyr::group_by(.data$total) |>
      dplyr::mutate(num = dplyr::n(),
                    hamming_percentile =
                      dplyr::case_when(
                        .data$num < sx_threshold ~ 1,
                        TRUE ~ dplyr::percent_rank(.data$hamming_distance)
                      )) |>
      dplyr::select(-"num") |>
      dplyr::filter(.data$hamming_percentile >= filter_percentile) |>
      dplyr::ungroup() |>
      dplyr::select(-"hamming_distance", -"hamming_percentile")
  } else if (filter_function == "mean") {
    profiles <- profiles |>
      dplyr::group_by(.data$total) |>
      dplyr::mutate(num = dplyr::n(),
                    hamming_distance =
                      dplyr::case_when(.data$num < sx_threshold ~ 0,
                                       TRUE ~ .data$hamming_distance)) |>
      dplyr::select(-"num") |>
      dplyr::filter(.data$hamming_distance >= mean(.data$hamming_distance)) |>
      dplyr::ungroup() |>
      dplyr::select(-"hamming_distance")
  } else if (filter_function == "median") {
    profiles <- profiles |>
      dplyr::group_by(.data$total) |>
      dplyr::mutate(num = dplyr::n(),
                    hamming_distance =
                      dplyr::case_when(.data$num < sx_threshold ~ 0,
                                       TRUE ~ .data$hamming_distance)) |>
      dplyr::select(-"num") |>
      dplyr::filter(.data$hamming_distance >=
                      stats::median(.data$hamming_distance)) |>
      dplyr::ungroup() |>
      dplyr::select(-"hamming_distance")
  }

  return(profiles) # nolint
}
