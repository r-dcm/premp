test_that("assigning profiles in Round 1 works", {
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

  raters <- glue::glue("rater{1:5}")

  final_assignments <- assign_profiles(raters = raters,
                                       eligible_profiles = eligible_profiles,
                                       observed = observed,
                                       profiles_per_rater = 8,
                                       raters_per_profile = 2,
                                       round = 1,
                                       num_pls = 4,
                                       range_of_profiles = 5,
                                       output_dir = testthat::test_path("data"))

  observed <- final_assignments$observed
  final_assignments <- final_assignments$final_assignments

  # did output save correctly
  testthat::expect_equal(
    final_assignments,
    readr::read_csv(testthat::test_path("data/profile_assignments_round_1.csv"))
  )

  # correct number of columns
  testthat::expect_equal(ncol(final_assignments), 13)
  # correct number of rows
  testthat::expect_equal(nrow(final_assignments), 10)
  # total attributes are correct
  testthat::expect_equal(final_assignments |>
                           dplyr::distinct(.data$total) |>
                           dplyr::pull(),
                         c(5, 10, 15, 20, 25))
  # number of each attribute total is correct
  testthat::expect_equal(final_assignments |>
                           dplyr::pull(.data$total),
                         c(5, 5, 10, 10, 15, 15, 20, 20, 25, 25))
  # column names are correct
  testthat::expect_equal(colnames(final_assignments),
                         c(glue::glue("att{1:7}"), "total",
                                      glue::glue("rater{1:5}")))
  # every rater assigned correct number of profiles
  testthat::expect_equal(final_assignments |>
                           dplyr::select(dplyr::starts_with("rater")) |>
                           tidyr::pivot_longer(cols = dplyr::everything(),
                                               names_to = "rater",
                                               values_to = "assignment") |>
                           dplyr::filter(.data$assignment == 1) |>
                           dplyr::count(.data$rater) |>
                           dplyr::pull(.data$n),
                         rep(8, length(raters)))
  # minimum number of profiles seen by all raters
  testthat::expect_gte(final_assignments |>
                         dplyr::select(dplyr::starts_with("rater")) |>
                         tibble::rowid_to_column("prof_num") |>
                         tidyr::pivot_longer(cols = -c("prof_num"),
                                             names_to = "rater",
                                             values_to = "assignment") |>
                         dplyr::filter(.data$assignment == 1) |>
                         dplyr::count(.data$prof_num) |>
                         dplyr::filter(.data$n == length(raters)) |>
                         nrow(),
                       1)
  # all attribute totals are increment of the range of profiles
  testthat::expect_equal(final_assignments |>
                           dplyr::mutate(increment = .data$total %% 5) |>
                           dplyr::pull(.data$increment),
                         rep(0, nrow(final_assignments)))
  # assignments are in correct format
  testthat::expect_contains(c(0, 1),
                            final_assignments |>
                              dplyr::select(dplyr::starts_with("rater")) |>
                              tidyr::pivot_longer(cols = dplyr::everything(),
                                                  names_to = "rater",
                                                  values_to = "assignment") |>
                              dplyr::distinct(.data$assignment) |>
                              dplyr::pull())
  # attribute scores are in correct format
  testthat::expect_contains(c(0:4),
                            final_assignments |>
                              dplyr::select(dplyr::starts_with("att")) |>
                              tidyr::pivot_longer(cols = dplyr::everything(),
                                                  names_to = "att",
                                                  values_to = "att_score") |>
                              dplyr::distinct(.data$att_score) |>
                              dplyr::pull())
})

test_that("assign profiles -- error messages work", {
  # test incorrect arguments
  err <- rlang::catch_cnd(assign_profiles(raters = raters,
                                          eligible_profiles = eligible_profiles,
                                          observed = observed,
                                          profiles_per_rater = 8,
                                          raters_per_profile = 2,
                                          round = 1,
                                          num_pls = 4,
                                          range_of_profiles = NULL,
                                          output_dir =
                                            testthat::test_path("data")))
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "must be provided for Round 1."
  )

  err <- rlang::catch_cnd(assign_profiles(raters = raters,
                                          eligible_profiles = eligible_profiles,
                                          observed = observed,
                                          profiles_per_rater = 8,
                                          raters_per_profile = 2,
                                          round = 2,
                                          num_pls = 4,
                                          range_of_profiles = 5,
                                          output_dir =
                                            testthat::test_path("data")))
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "is not needed for Round 2."
  )
})

test_that("assign profiles -- few attributes work", {
  eligible_profiles <- tibble::tibble(tidyr::crossing(att1 = c(0:4),
                                                      att2 = c(0:4),
                                                      att3 = c(0:4),
                                                      att4 = c(0:4)))

  obs <- runif(nrow(eligible_profiles), 1, 10000)

  observed <- eligible_profiles |>
    dplyr::mutate(n = obs,
                  n = dplyr::case_when(n < 1000 ~ NA,
                                       TRUE ~ n)) |>
    dplyr::filter(!is.na(n))

  raters <- glue::glue("rater{1:5}")

  final_assignments <- assign_profiles(raters = raters,
                                       eligible_profiles = eligible_profiles,
                                       observed = observed,
                                       profiles_per_rater = 8,
                                       raters_per_profile = 2,
                                       round = 1,
                                       num_pls = 4,
                                       range_of_profiles = 5,
                                       output_dir = testthat::test_path("data"))

  observed <- final_assignments$observed
  final_assignments <- final_assignments$final_assignments

  # did output save correctly
  testthat::expect_equal(
    final_assignments,
    readr::read_csv(testthat::test_path("data/profile_assignments_round_1.csv"))
  )

  # correct number of columns
  testthat::expect_equal(ncol(final_assignments), 10)
  # correct number of rows
  testthat::expect_equal(nrow(final_assignments), 6)
  # total attributes are correct
  testthat::expect_equal(final_assignments |>
                           dplyr::distinct(.data$total) |>
                           dplyr::pull(),
                         c(5, 10, 15))
  # number of each attribute total is correct
  testthat::expect_equal(final_assignments |>
                           dplyr::pull(.data$total),
                         c(5, 5, 10, 10, 15, 15))
  # column names are correct
  testthat::expect_equal(colnames(final_assignments),
                         c(glue::glue("att{1:4}"), "total",
                           glue::glue("rater{1:5}")))
  # every rater assigned correct number of profiles
  testthat::expect_equal(final_assignments |>
                           dplyr::select(dplyr::starts_with("rater")) |>
                           tidyr::pivot_longer(cols = dplyr::everything(),
                                               names_to = "rater",
                                               values_to = "assignment") |>
                           dplyr::filter(.data$assignment == 1) |>
                           dplyr::count(.data$rater) |>
                           dplyr::pull(.data$n),
                         rep(6, length(raters)))
  # minimum number of profiles seen by all raters
  testthat::expect_gte(final_assignments |>
                         dplyr::select(dplyr::starts_with("rater")) |>
                         tibble::rowid_to_column("prof_num") |>
                         tidyr::pivot_longer(cols = -c("prof_num"),
                                             names_to = "rater",
                                             values_to = "assignment") |>
                         dplyr::filter(.data$assignment == 1) |>
                         dplyr::count(.data$prof_num) |>
                         dplyr::filter(.data$n == length(raters)) |>
                         nrow(),
                       1)
  # all attribute totals are increment of the range of profiles
  testthat::expect_equal(final_assignments |>
                           dplyr::mutate(increment = .data$total %% 5) |>
                           dplyr::pull(.data$increment),
                         rep(0, nrow(final_assignments)))
  # assignments are in correct format
  testthat::expect_contains(c(0, 1),
                            final_assignments |>
                              dplyr::select(dplyr::starts_with("rater")) |>
                              tidyr::pivot_longer(cols = dplyr::everything(),
                                                  names_to = "rater",
                                                  values_to = "assignment") |>
                              dplyr::distinct(.data$assignment) |>
                              dplyr::pull())
  # attribute scores are in correct format
  testthat::expect_contains(c(0:4),
                            final_assignments |>
                              dplyr::select(dplyr::starts_with("att")) |>
                              tidyr::pivot_longer(cols = dplyr::everything(),
                                                  names_to = "att",
                                                  values_to = "att_score") |>
                              dplyr::distinct(.data$att_score) |>
                              dplyr::pull())
})
