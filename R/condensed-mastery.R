#' Range Finding Procedure from the Condensed Mastery Standard Setting Method
#'
#' Fit a logistic regression model to conduct the range finding procedure from
#' the condensed mastery standard setting method.
#'
#' @param ratings A tibble with one row for each rater's rating of an assigned
#'   profile, including columns for the profile number, the mastery status for
#'   each attribute, and the raters' ratings.
#' @param profiles A tibble with one row for each attribute mastery
#'   profiles that is eligible for assignment to raters. The rows correspond to
#'.  the profile number in the `ratings` argument.
#' @param pl_labels A character vector containing the ordered performance
#'   levels.
#' @param att_levels An integer describing the number of categorical mastery
#'   classes for each attribute.
#' @param cores The number of cores (default = 4).
#' @param chains The number of chains (default = 4).
#' @param output_dir The output directory for the pinpointing ranges.
#'
#' @return [tibble][tibble::tibble-package] A tibble containing the pinpoint
#' ranges to assign profiles during the second round of standard setting.
#'
#' @export
condensed_mastery <- function(
  ratings,
  profiles,
  pl_labels,
  att_levels,
  cores = 4,
  chains = 4,
  output_dir
) {
  if (length(pl_labels) < 2) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(pl_labels),
      must = cli::format_message(paste(
        "must have a value of at least 2."
      ))
    )
  }

  min_pinpoint_range <- 2

  # set options
  cmdstan_v <- cmdstanr::cmdstan_version(error_on_NA = FALSE)
  options(brms.backend = ifelse(is.null(cmdstan_v), "rstan", "cmdstanr"))

  att_vec <- profiles |>
    names()

  profiles <- profiles |>
    dplyr::rowwise() |>
    dplyr::mutate(total = sum(dplyr::c_across(dplyr::everything()))) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$total, dplyr::across(dplyr::everything(),
                                              dplyr::desc))

  pl_dict <- tibble::tibble(pl = pl_labels) |>
    dplyr::mutate(pl_num = dplyr::row_number())

  # prep data
  rf_dat <- ratings |>
    dplyr::count(.data$profile_num, !!!rlang::syms(att_vec), .data$rating) |>
    dplyr::left_join(pl_dict, by = c("rating" = "pl_num")) |>
    dplyr::mutate(rating = .data$pl) |>
    dplyr::select(-"pl") |>
    dplyr::mutate(rating = factor(.data$rating,
                                  levels = pl_labels)) |>
    tidyr::pivot_wider(names_from = "rating", values_from = "n",
                       values_fill = 0L, names_expand = TRUE) |>
    dplyr::rename(profile_id = "profile_num") |>
    dplyr::rowwise() |>
    dplyr::mutate(atts_mastered =
                    sum(dplyr::c_across(dplyr::any_of(att_vec)))) |>
    dplyr::ungroup() |>
    dplyr::select("profile_id", "atts_mastered", dplyr::any_of(pl_labels))

  # Fit models -----------------------------------------------------------------
  ## Step 1: Calculate how many panelists put each profile in each PLD or higher
  rf_dat <- rf_dat |>
    tidyr::pivot_longer(cols = -c("profile_id", "atts_mastered"),
                        names_to = "pl", values_to = "num") |>
    dplyr::left_join(pl_dict, by = c("pl")) |>
    dplyr::group_by(.data$profile_id) |>
    dplyr::mutate(total_ratings = sum(.data$num)) |>
    dplyr::ungroup()

  for (ii in 2:length(pl_labels)) {
    rf_dat <- rf_dat |>
      dplyr::group_by(.data$profile_id) |>
      dplyr::mutate(tmp = dplyr::case_when(.data$pl_num >= ii ~ num,
                                           TRUE ~ 0),
                    !!rlang::sym(pl_labels[ii]) := sum(.data$tmp)) |>
      dplyr::ungroup() |>
      dplyr::select(-"tmp")
  }

  rf_dat <- rf_dat |>
    dplyr::select("profile_id", "atts_mastered", "total_ratings",
                  dplyr::any_of(pl_labels[2:length(pl_labels)])) |>
    dplyr::distinct()

  ## Step 2: Convert counts to long-format (i.e., 0s and 1s)
  long_rf <- rf_dat |>
    dplyr::select("profile_id", "atts_mastered", "total_ratings") |>
    tidyr::crossing(poss_ratings = 1:max(.data$total_ratings)) |>
    dplyr::filter(.data$poss_ratings <= .data$total_ratings) |>
    dplyr::select(-"total_ratings", -"poss_ratings")

  for (ii in 2:length(pl_labels)) {
    tmp_rf <- rf_dat |>
      dplyr::group_by(.data$profile_id, .data$atts_mastered) |>
      dplyr::reframe(!!rlang::sym(pl_labels[ii]) :=
                       c(rep(0L,
                             .data$total_ratings -
                               max(!!rlang::sym(pl_labels[ii]))),
                         rep(1L, max(!!rlang::sym(pl_labels[ii]))))) |>
      dplyr::ungroup() |>
      dplyr::select(-"profile_id", -"atts_mastered")

    long_rf <- dplyr::bind_cols(long_rf, tmp_rf)
  }

  ## Step 3: Fit a model for each cut point
  model_results <- long_rf |>
    tidyr::pivot_longer(cols = dplyr::any_of(pl_labels[2:length(pl_labels)]),
                        names_to = "model",
                        values_to = "y") |>
    tidyr::nest(model_dat = c("profile_id", "atts_mastered", "y")) |>
    dplyr::mutate(params = purrr::map(.data$model_dat, fit_model,
                                      cores = cores, chains = chains)) |>
    tidyr::unnest("params")

  # Calculate pinpointing ranges -----------------------------------------------
  ## Predicted cut points are defined as the value of attributes mastered at the
  ## inflection point of the logistic curve. The minimum predicted cut point is
  ## the lesser of the x value with a model-predicted .20 probability and 2
  ## below the inflection point. The maximum predicted cut point is the greater
  ## of the x value with a model-predicted .80 probability and 2 above the
  ## inflection point.
  pinpoint_ranges <- model_results |>
    dplyr::mutate(intercept = purrr::map(.data$intercept,
                                         posterior::as_draws_df),
                  slope = purrr::map(.data$slope, posterior::as_draws_df)) |>
    tidyr::unnest("intercept") |>
    dplyr::group_by(.data$model, .data$slope) |>
    dplyr::summarize(intercept = mean(.data$x), .groups = "drop") |>
    tidyr::unnest("slope") |>
    dplyr::group_by(.data$model, .data$intercept) |>
    dplyr::summarize(slope = mean(.data$x), .groups = "drop") |>
    dplyr::mutate(.value = ((-1 * log((1 / .5) - 1)) - .data$intercept) /
                    .data$slope,
                  .lower = ((-1 * log((1 / .2) - 1)) - .data$intercept) /
                    .data$slope,
                  .upper = ((-1 * log((1 / .8) - 1)) - .data$intercept) /
                    .data$slope) |>
    dplyr::select(-"intercept", -"slope") |>
    dplyr::mutate(.value = round(.data$.value, digits = 0),
                  .lower = floor(.data$.lower),
                  .upper = ceiling(.data$.upper)) |>
    dplyr::rowwise() |>
    dplyr::mutate(.lower = min(.data$.value - min_pinpoint_range,
                               .data$.lower),
                  .lower = max(.data$.lower, 0),
                  .upper = max(.data$.value + min_pinpoint_range,
                               .data$.upper),
                  .upper = min(.data$.upper,
                               length(att_vec) * att_levels)) |>
    dplyr::ungroup() |>
    dplyr::select(cut_point = "model", predicted = ".value",
                  pinpoint_min = ".lower", pinpoint_max = ".upper")

  pinpoint_ranges |>
    readr::write_csv(glue::glue("{output_dir}/pp-range.csv"))
}
