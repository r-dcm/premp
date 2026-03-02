test_that("slice stratified works", {
  profiles_per_rater <- 8
  profiles_seen_by_all <- 1
  raters_per_profile <- 2
  round <- 1
  num_pls <- 4
  observed_id <- "n"
  range_of_profiles <- 5
  raters <- glue::glue("rater{1:5}")

  profiles_per_rater <- profiles_per_rater - profiles_seen_by_all
  total_ratings <- length(raters) * profiles_per_rater
  num_profiles <- floor(total_ratings / raters_per_profile)

  eligible_profiles <- tibble::tibble(tidyr::crossing(att1 = c(0:4),
                                                      att2 = c(0:4),
                                                      att3 = c(0:4),
                                                      att4 = c(0:4),
                                                      att5 = c(0:4),
                                                      att6 = c(0:4),
                                                      att7 = c(0:4)))

  obs <- runif(nrow(eligible_profiles), 1, 10000)

  observed <- eligible_profiles |>
    dplyr::mutate(n = obs,
                  n = dplyr::case_when(n < 1000 ~ NA,
                                       TRUE ~ n)) |>
    dplyr::filter(!is.na(n))

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

  # number sampled is correct
  testthat::expect_equal(nrow(profiles_to_assign),
                         profile_sampling |>
                           dplyr::distinct(.data$total, .data$samples) |>
                           dplyr::summarize(total_sample =
                                              sum(.data$samples)) |>
                           dplyr::pull())
  # check column names
  testthat::expect_equal(colnames(profiles_to_assign),
                         c(glue::glue("att{1:7}"), "total"))
  # profiles assigned at range of profiles increment
  testthat::expect_equal(profiles_to_assign |>
                           dplyr::mutate(increment = .data$total %% 5) |>
                           dplyr::pull(.data$increment),
                         rep(0, nrow(profiles_to_assign)))
  # correct number of profiles sampled at each attribute total
  testthat::expect_equal(profiles_to_assign |>
                           dplyr::count(.data$total),
                         profile_sampling |>
                           dplyr::distinct(.data$total, .data$samples) |>
                           dplyr::rename(n = "samples"))
})

test_that("max values works", {
  testthat::expect_equal(max_value(1.1), .9999)
  testthat::expect_equal(max_value(.1), .1)
})

test_that("fit model works", {
  model_dat <- tibble::tibble(profile_id = rep(1:6, each = 5),
                              atts_mastered = c(rep(5, 10), rep(10, 10),
                                                rep(15, 10)),
                              y = c(rep(1, 10), 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
                                    rep(0, 10)))

  mod_out <- fit_model(model_dat, cores = 1, chains = 1)

  profiles_per_rater <- 8
  profiles_seen_by_all <- 1
  raters_per_profile <- 2
  round <- 1
  num_pls <- 4
  observed_id <- "n"
  range_of_profiles <- 5
  raters <- glue::glue("rater{1:5}")

  profiles_per_rater <- profiles_per_rater - profiles_seen_by_all
  total_ratings <- length(raters) * profiles_per_rater
  num_profiles <- floor(total_ratings / raters_per_profile)

  eligible_profiles <- tibble::tibble(tidyr::crossing(att1 = c(0:4),
                                                      att2 = c(0:4),
                                                      att3 = c(0:4),
                                                      att4 = c(0:4),
                                                      att5 = c(0:4),
                                                      att6 = c(0:4),
                                                      att7 = c(0:4)))

  obs <- runif(nrow(eligible_profiles), 1, 10000)

  observed <- eligible_profiles |>
    dplyr::mutate(n = obs,
                  n = dplyr::case_when(n < 1000 ~ NA,
                                       TRUE ~ n)) |>
    dplyr::filter(!is.na(n))

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

  # number sampled is correct
  testthat::expect_equal(nrow(profiles_to_assign),
                         profile_sampling |>
                           dplyr::distinct(.data$total, .data$samples) |>
                           dplyr::summarize(total_sample =
                                              sum(.data$samples)) |>
                           dplyr::pull())
  # check column names
  testthat::expect_equal(colnames(profiles_to_assign),
                         c(glue::glue("att{1:7}"), "total"))
  # profiles assigned at range of profiles increment
  testthat::expect_equal(profiles_to_assign |>
                           dplyr::mutate(increment = .data$total %% 5) |>
                           dplyr::pull(.data$increment),
                         rep(0, nrow(profiles_to_assign)))
  # correct number of profiles sampled at each attribute total
  testthat::expect_equal(profiles_to_assign |>
                           dplyr::count(.data$total),
                         profile_sampling |>
                           dplyr::distinct(.data$total, .data$samples) |>
                           dplyr::rename(n = "samples"))
})
