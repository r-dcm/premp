test_that("slice stratified works", {
  observed_id <- "n"
  range_of_profiles <- 5

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

  att_vec <- eligible_profiles |>
    dplyr::select(-"total") |>
    names()

  seen_by_all <- eligible_profiles |>
    dplyr::filter(.data$total != 0) |>
    dplyr::left_join(observed, by = att_vec) |>
    dplyr::filter(!is.na(!!rlang::sym(observed_id))) |>
    dplyr::filter(!!rlang::sym(observed_id) > 100) |>
    dplyr::mutate(size = 1)

  test_output <- slice_stratified(seen_by_all, by = "total", size = "size",
                                  weight_by = "pct")

  # output format is correct
  testthat::expect_contains(class(test_output), "tbl_df")
  # check column names
  testthat::expect_equal(names(test_output),
                         c(glue::glue("att{1:7}"), "total", "n"))
  # number sampled is correct
  testthat::expect_equal(nrow(test_output), 5)
  # profiles assigned at range of profiles increment
  testthat::expect_equal(test_output |>
                           dplyr::mutate(increment = .data$total %% 5) |>
                           dplyr::pull(.data$increment),
                         rep(0, nrow(test_output)))
  # correct number of profiles sampled at each attribute total
  testthat::expect_equal(test_output |>
                           dplyr::count(.data$total) |>
                           dplyr::pull(.data$n),
                         rep(1, 5))
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

  # number sampled is correct
  testthat::expect_equal(nrow(mod_out), 1)
  testthat::expect_equal(ncol(mod_out), 2)
  # check column names
  testthat::expect_equal(names(mod_out), c("intercept", "slope"))
})

test_that("Hamming distance calculations work", {
  att_vec <- c("att1", "att2", "att3", "att4")
  profiles <- tibble::tibble(att1 = c(1, 0, 1),
                             att2 = c(1, 1, 0),
                             att3 = c(1, 1, 1),
                             att4 = c(0, 1, 1),
                             total = c(3, 3, 3))
  assigned_profiles <- tibble::tibble(att1 = c(1),
                                      att2 = c(1),
                                      att3 = c(1),
                                      att4 = c(0))

  hamming_dist <- calculate_hamming(profiles, assigned_profiles, att_vec)

  testthat::expect_contains(class(hamming_dist), "tbl_df")
  testthat::expect_equal(nrow(hamming_dist), 3)
  testthat::expect_equal(names(hamming_dist),
                         c(glue::glue("att{1:4}"), "total", "hamming_distance"))
  testthat::expect_equal(hamming_dist$hamming_distance,
                         c(0, 2, 2))
})

test_that("refining based on Hamming distance work", {
  raters <- c("table1", "table2")
  filter_function = "median"
  filter_percentile = NULL

  profiles <- tibble::tibble(att1 = c(1, 0, 1),
                             att2 = c(1, 1, 0),
                             att3 = c(1, 1, 1),
                             att4 = c(0, 1, 1),
                             total = c(3, 3, 3),
                             hamming_distance = c(0, 2, 2))

  refined_output <- refine_eligible_profiles(profiles, filter_function,
                                             filter_percentile, raters,
                                             profiles_per_level = 2)

  exp_output <- profiles |>
    dplyr::select(-"hamming_distance")

  testthat::expect_equal(refined_output, exp_output)

  raters <- c("table1", "table2")
  filter_function = "median"
  filter_percentile = NULL

  profiles <- tibble::tibble(att1 = rep(c(1, 0, 1), times = 5),
                             att2 = rep(c(1, 1, 0), times = 5),
                             att3 = rep(c(1, 1, 1), times = 5),
                             att4 = rep(c(0, 1, 1), times = 5),
                             total = rep(c(3, 3, 3), times = 5),
                             hamming_distance = rep(c(0, 2, 2), times = 5))

  refined_output <- refine_eligible_profiles(profiles, filter_function,
                                             filter_percentile, raters,
                                             profiles_per_level = 2)

  exp_output <- tibble::tibble(att1 = rep(c(0, 1), times = 5),
                               att2 = rep(c(1, 0), times = 5),
                               att3 = rep(c(1, 1), times = 5),
                               att4 = rep(c(1, 1), times = 5),
                               total = rep(c(3, 3), times = 5))

  testthat::expect_equal(refined_output, exp_output)

  raters <- c("table1", "table2")
  filter_function = "mean"
  filter_percentile = NULL

  profiles <- tibble::tibble(att1 = rep(c(1, 0, 1), times = 5),
                             att2 = rep(c(1, 1, 0), times = 5),
                             att3 = rep(c(1, 1, 1), times = 5),
                             att4 = rep(c(0, 1, 1), times = 5),
                             total = rep(c(3, 3, 3), times = 5),
                             hamming_distance = rep(c(0, 2, 2), times = 5))

  refined_output <- refine_eligible_profiles(profiles, filter_function,
                                             filter_percentile, raters,
                                             profiles_per_level = 2)

  exp_output <- tibble::tibble(att1 = rep(c(0, 1), times = 5),
                               att2 = rep(c(1, 0), times = 5),
                               att3 = rep(c(1, 1), times = 5),
                               att4 = rep(c(1, 1), times = 5),
                               total = rep(c(3, 3), times = 5))

  testthat::expect_equal(refined_output, exp_output)

  raters <- c("table1", "table2")
  filter_function = NULL
  filter_percentile = .6

  profiles <- tibble::tibble(att1 = rep(c(1, 0, 1), times = 5),
                             att2 = rep(c(1, 1, 0), times = 5),
                             att3 = rep(c(1, 1, 1), times = 5),
                             att4 = rep(c(0, 1, 1), times = 5),
                             total = rep(c(3, 3, 3), times = 5),
                             hamming_distance = rep(1:5, times = 3))

  refined_output <- refine_eligible_profiles(profiles, filter_function,
                                             filter_percentile, raters,
                                             profiles_per_level = 2)

  exp_output <- tibble::tibble(att1 = rep(c(1, 0, 1), times = 5),
                               att2 = rep(c(1, 1, 0), times = 5),
                               att3 = rep(c(1, 1, 1), times = 5),
                               att4 = rep(c(0, 1, 1), times = 5),
                               total = rep(c(3, 3, 3), times = 5),
                               hamming_distance = rep(1:5, times = 3)) |>
    dplyr::filter(.data$hamming_distance %in% c(4, 5)) |>
    dplyr::select(-"hamming_distance")

  testthat::expect_equal(refined_output, exp_output)

  raters <- c("table1", "table2")
  filter_function = NULL
  filter_percentile = .6

  profiles <- tibble::tibble(att1 = rep(c(1, 0, 1), times = 5),
                             att2 = rep(c(1, 1, 0), times = 5),
                             att3 = rep(c(1, 1, 1), times = 5),
                             att4 = rep(c(0, 1, 1), times = 5),
                             total = rep(c(3, 3, 3), times = 5),
                             hamming_distance = rep(c(0, 2, 2), times = 5))

  refined_output <- refine_eligible_profiles(profiles, filter_function,
                                             filter_percentile, raters,
                                             profiles_per_level = 2)

  exp_output <- profiles |>
    dplyr::filter(is.na(.data$hamming_distance)) |>
    dplyr::select(-"hamming_distance")

  testthat::expect_equal(refined_output, exp_output)
})

test_that("refining based on Hamming distance -- error messages work", {

  raters <- c("table1", "table2")
  filter_function = "median"
  filter_percentile = .6
  profiles <- tibble::tibble()
  profiles_per_level <- 2L

  err <- rlang::catch_cnd(refine_eligible_profiles(profiles, filter_function,
                                                   filter_percentile, raters,
                                                   profiles_per_level))
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "must not be provided in addition to `filter_function`."
  )

  raters <- c("table1", "table2")
  filter_function = NULL
  filter_percentile = -.01
  profiles <- tibble::tibble()
  profiles_per_level <- 2L

  err <- rlang::catch_cnd(refine_eligible_profiles(profiles, filter_function,
                                                   filter_percentile, raters,
                                                   profiles_per_level))
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "must be between 0 and 1."
  )

  filter_percentile = 1.01

  err <- rlang::catch_cnd(refine_eligible_profiles(profiles, filter_function,
                                                   filter_percentile, raters,
                                                   profiles_per_level))
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "must be between 0 and 1."
  )
})
