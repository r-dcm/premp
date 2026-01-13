#' Weighted Sampling
#'
#' Apply a weighted sampling procedure to sample profiles that will be assigned
#' to raters during a sample setting event.
#'
#' @param eligible_profiles A tibble with one row for each attribute mastery
#' profiles that is eligible for assignment to raters.
#' @param num_profiles The maximum number of profiles that be assigned during
#' this round of the standard setting event.
#' @param observed A tibble with one row for each attribute mastery profile that
#' was observed along with the number of times it was observed.
#' @param round The round number of the standard setting event.
#' @param num_pls The number of performance levels that can be assigned to any
#' profile.
#'
#' @return [tibble][tibble::tibble-package] A tibble containing the profiles to
#' be assigned to raters during a standard setting event.
#'
#' @export
#' @examples
weighted_sampling <- function(
    eligible_profiles,
    num_profiles,
    observed,
    round,
    num_pls
) {
  # calculate profile weights based on number of mastered attributes
  prof_weights <- eligible_profiles |>
    dplyr::count(total) |>
    dplyr::mutate(prob = n / sum(n),
                  min_count = 1,
                  count_prob = prob * num_profiles,
                  count = round(prob * num_profiles, 0),
                  count = dplyr::case_when(count < min_count ~ min_count,
                                    TRUE ~ count)) |>
    dplyr::select(-"n", -"prob", -"count_prob", -"min_count")

  # remove sampled counts from highest frequency totals if necessary
  while (sum(prof_weights$count) > num_profiles) {
    tmp <- prof_weights |>
      dplyr::filter(count == max(count)) |>
      dplyr::slice_sample(n = 1) |>
      dplyr::pull(total)

    prof_weights <- prof_weights |>
      dplyr::mutate(count = dplyr::case_when(total == tmp ~ count - 1,
                                             TRUE ~ count))
  }

  prof_weights <- prof_weights |>
    dplyr::rename(samples = count)

  profile_sampling <- observed |>
    dplyr::select(-"n") |>
    dplyr::rowwise() |>
    dplyr::mutate(total = sum(dplyr::c_across(dplyr::starts_with("EE")))) |>
    dplyr::ungroup() |>
    dplyr::left_join(prof_weights, by = "total") |>
    dplyr::filter(!is.na(samples))

  # oversample to alleviate deficits
  deficits <- profile_sampling |>
    dplyr::group_by(total) |>
    dplyr::mutate(available = n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(deficit = available - samples) |>
    dplyr::distinct(total, samples, available, deficit) |>
    dplyr::filter(deficit != 0)

  surplus <- deficits |>
    dplyr::filter(deficit > 0)

  short <- deficits |>
    dplyr::filter(deficit < 0)

  num_short <- deficits |>
    dplyr::filter(deficit < 0) |>
    dplyr::summarize(deficit = abs(sum(deficit))) |>
    dplyr::pull()

  for (mm in seq_len(num_short)) {
    total_add <- surplus |>
      dplyr::slice_sample(n = 1) |>
      dplyr::select("total") |>
      dplyr::mutate(add = 1)

    surplus <- surplus |>
      dplyr::left_join(total_add, by = "total") |>
      dplyr::mutate(samples = dplyr::case_when(!is.na(add) ~ samples + 1,
                                               TRUE ~ samples),
                    deficit = available - samples) |>
      dplyr::select(-"add") |>
      dplyr::filter(deficit != 0)

    deficits <- deficits |>
      dplyr::left_join(total_add, by = "total") |>
      dplyr::mutate(samples = dplyr::case_when(!is.na(add) ~ samples + 1,
                                               TRUE ~ samples),
                    deficit = available - samples) |>
      dplyr::select(-"add")

    total_short <- short |>
      dplyr::slice_sample(n = 1) |>
      dplyr::mutate(sub = 1) |>
      dplyr::select("total", "sub")

    short <- short |>
      dplyr::left_join(total_short, by = "total") |>
      dplyr::mutate(samples = dplyr::case_when(!is.na(sub) ~ samples - 1,
                                               TRUE ~ samples),
                    deficit = available - samples) |>
      dplyr::select(-"sub") |>
      dplyr::filter(deficit != 0)

    deficits <- deficits |>
      dplyr::left_join(total_short, by = "total") |>
      dplyr::mutate(samples = dplyr::case_when(!is.na(sub) ~ samples - 1,
                                               TRUE ~ samples),
                    deficit = available - samples) |>
      dplyr::select(-"sub")
  }

  num_attributes <- profile_sampling |>
    dplyr::select(-"pct", -"total", -"samples") |>
    names() |>
    length()

  # adjust sample counts; sampling number at each total adjusted by round
  profile_sampling <- profile_sampling |>
    dplyr::left_join(deficits |>
                       dplyr::select("total", "samples"),
                     by = "total") |>
    dplyr::mutate(samples = dplyr::case_when(is.na(samples.y) ~ samples.x,
                                             TRUE ~ samples.y)) |>
    dplyr::select(-"samples.x", -"samples.y") |>
    dplyr::filter(total != 0) |>
    dplyr::filter(total != num_attributes * num_pls) |>
    ratlas::only_if(round == 1)(dplyr::mutate)(samples = 2) |>
    ratlas::only_if(round == 2)(dplyr::mutate)(samples = 3)

  return(profile_sampling)
}
