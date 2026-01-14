#' Range Finding Procedure from the Condensed Mastery Standard Setting Method
#'
#' Fit a logistic regression model to conduct the range finding procedure from
#' the condensed mastery standard setting method.
#'
#' @param ratings A tibble with one row for each rater's rating of an assigned
#' profile.
#' @param profiles A tibble with one row for each attribute mastery
#' profiles that is eligible for assignment to raters.
#'
#' @return [tibble][tibble::tibble-package] A tibble containing the pinpoint
#' ranges to assign profiles during the second round of standard setting.
#'
#' @export
#' @examples
condensed_mastery <- function(ratings, profiles) {
  # code adapted from https://github.com/atlas-aai/standard-setting/blob/main/01-pinpoint-ranges.R
  pinpoint_interval <- 0.89
  min_pinpoint_range <- 3

  # set options
  cmdstan_v <- cmdstanr::cmdstan_version(error_on_NA = FALSE)
  options(brms.backend = ifelse(is.null(cmdstan_v), "rstan", "cmdstanr"))

  ee_vec <- profiles |>
    names()

  profiles <- profiles |>
    dplyr::rowwise() |>
    dplyr::mutate(total = sum(dplyr::c_across(dplyr::everything()))) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$total, dplyr::across(dplyr::everything(),
                                              dplyr::desc))

  # prep data
  rf_dat <- ratings |>
    tidyr::pivot_longer(cols = dplyr::starts_with("rater"),
                        names_to = "rater_id",
                        values_to = "rating") |>
    dplyr::select(-"rater_id") |>
    dplyr::count(profile_num, !!!rlang::syms(ee_vec), rating) |>
    dplyr::mutate(rating = dplyr::case_when(rating == 1 ~ "emerging",
                                            rating == 2 ~ "approaching",
                                            rating == 3 ~ "target",
                                            rating == 4 ~ "advanced"),
                  rating = factor(rating,
                                  levels = c("emerging", "approaching",
                                             "target", "advanced"))) |>
    tidyr::pivot_wider(names_from = "rating", values_from = "n",
                       values_fill = 0L, names_expand = TRUE) |>
    dplyr::rename(profile_id = profile_num) |>
    dplyr::rowwise() |>
    dplyr::mutate(lls_mastered =
                    sum(dplyr::c_across(dplyr::starts_with("EE")))) |>
    dplyr::ungroup() |>
    dplyr::select("profile_id", "lls_mastered", "emerging", "approaching",
                  "target", "advanced")

  # Fit models -------------------------------------------------------------------
  ## Step 1: Calculate how many panelists put each profile in each PLD or higher
  rf_dat <- rf_dat |>
    dplyr::rowwise() |>
    dplyr::mutate(total_ratings =
                    sum(dplyr::c_across(emerging:advanced), na.rm = TRUE),
                  app =
                    sum(dplyr::c_across(approaching:advanced), na.rm = TRUE),
                  tar =
                    sum(dplyr::c_across(target:advanced), na.rm = TRUE),
                  adv =
                    sum(dplyr::c_across(advanced), na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(-c("emerging", "approaching", "target", "advanced"))

  ## Step 2: Convert counts to long-format (i.e., 0s and 1s)
  long_rf <- rf_dat |>
    dplyr::group_by(profile_id, lls_mastered) |>
    dplyr::reframe(app = c(rep(0L, total_ratings - max(app)),
                           rep(1L, max(app))),
                   tar = c(rep(0L, total_ratings - max(tar)),
                           rep(1L, max(tar))),
                   adv = c(rep(0L, total_ratings - max(adv)),
                           rep(1L, max(adv)))) |>
    dplyr::ungroup()

  ## Step 3: Fit a model for each cut point
  source(here::here("sim-study-4/R/functions.R"))

  model_results <- long_rf |>
    tidyr::pivot_longer(cols = c(app, tar, adv), names_to = "model",
                        values_to = "y") |>
    tidyr::nest(model_dat = c(profile_id, lls_mastered, y)) |>
    dplyr::mutate(params = purrr::map(model_dat, fit_model)) |>
    tidyr::unnest(params)

  # Calculate pinpointing ranges -------------------------------------------------
  ## Predicted cut points are defined as the value of LLs mastered at the
  ## inflection point of the logistic curve. The x value of the inflection point
  ## is given by -B_0 / B_1.
  pinpoint_ranges <- model_results |>
    dplyr::mutate(predicted = -intercept / slope,
                  range = tidybayes::mean_hdci(predicted,
                                               .width =
                                                 pinpoint_interval)) |>
    tidyr::unnest(range) |>
    dplyr::mutate(.value = round(.value, digits = 0),
                  .lower = floor(.lower), .upper = ceiling(.upper)) |>
    dplyr::rowwise() |>
    dplyr::mutate(.lower = min(.value - min_pinpoint_range, .lower),
                  .lower = max(.lower, 0),
                  .upper = max(.value + min_pinpoint_range, .upper),
                  .upper = min(.upper, num_ees * 4)) |>
    dplyr::ungroup() |>
    dplyr::select(cut_point = "model", predicted = ".value",
                  pinpoint_min = ".lower", pinpoint_max = ".upper")

  pinpoint_ranges |>
    readr::write_csv(glue("{output_dir}/pp-range.csv"))

  pinpoint_possible_profiles <- pinpoint_ranges |>
    dplyr::mutate(cut_point = factor(cut_point,
                                     levels = c("app", "tar", "adv"))) |>
    dplyr::group_by(cut_point) |>
    tidyr::complete(predicted = min(pinpoint_min):max(pinpoint_max)) |>
    dplyr::ungroup() |>
    dplyr::filter(predicted >= 0 & predicted <= num_ees * 4) |>
    dplyr::select(lls_mastered = "predicted") |>
    dplyr::distinct() |>
    dplyr::left_join(profiles |>
                       dplyr::select(dplyr::starts_with("EE"), "total") |>
                       dplyr::anti_join(assigned_profiles,
                                        by = c(ee_vec, "total")),
                     by = c("lls_mastered" = "total")) |>
    dplyr::filter(!is.na(EE_1)) |>
    dplyr::mutate(emerging = NA_real_, approaching = NA_real_,
                  target = NA_real_, advanced = NA_real_)

  pinpoint_possible_profiles |>
    readr::write_csv(glue::glue("{output_dir}/pp.csv"))

  return(pinpoint_possible_profiles)
}
