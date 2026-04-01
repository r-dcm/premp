test_that("weighted profile sampling works", {
  set.seed(123)

  observed_count_label <- "n"
  range_of_profiles <- c(5L, 10L, 15L, 20L, 25L)
  profiles_per_level <- 2L
  raters <- glue::glue("table{1:5}")
  table_configuration = list(panelists_per_table = 4L,
                             proportion_of_shared_profiles = .67)

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
    dplyr::mutate(!!rlang::sym(observed_count_label) :=
                    dplyr::case_when(is.na(!!rlang::sym(observed_count_label)) ~
                                       0,
                                     TRUE ~ n),
                  pct = !!rlang::sym(observed_count_label) /
                    sum(!!rlang::sym(observed_count_label))) |>
    dplyr::filter(!!rlang::sym(observed_count_label) != 0)

  eligible_profiles <- eligible_profiles |>
    # calculate total number of mastered attributes/skills
    dplyr::rowwise() |>
    dplyr::mutate(total = sum(dplyr::c_across(dplyr::everything()))) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$total, dplyr::across(dplyr::everything(),
                                              dplyr::desc)) |>
    # filter down to the total number in increments of the range of profiles
    dplyr::filter(.data$total %in% range_of_profiles)

  # apply weighted sampling design
  profile_sampling <- weighted_sampling(eligible_profiles, observed,
                                        observed_count_label,
                                        profiles_per_level, raters,
                                        table_configuration)

  # output format is correct
  testthat::expect_contains(class(profile_sampling), "tbl_df")
  testthat::expect_equal(names(profile_sampling),
                         c(glue::glue("att{1:7}"), "total", "table",
                           glue::glue("rater{1:4}")))
  # number sampled is correct
  testthat::expect_equal(nrow(profile_sampling), 125)
  # profiles assigned at range of profiles increment
  testthat::expect_equal(profile_sampling |>
                           dplyr::mutate(increment = .data$total %% 5) |>
                           dplyr::pull(.data$increment),
                         rep(0, nrow(profile_sampling)))
  # correct number of profiles sampled at each attribute total
  testthat::expect_equal(profile_sampling |>
                           dplyr::count(.data$total) |>
                           dplyr::pull(.data$n),
                         rep(25, 5))
})

# test_that("weighted profile sampling with shortage works", {
#
#   round <- 1
#   num_pls <- 4
#   observed_id <- "n"
#   range_of_profiles <- 5
#   raters <- glue::glue("rater{1:5}")
#
#   profiles_per_rater <- profiles_per_rater - profiles_seen_by_all
#   total_ratings <- length(raters) * profiles_per_rater
#   num_profiles <- floor(total_ratings / raters_per_profile)
#
#   eligible_profiles <- tibble::tibble(tidyr::crossing(att1 = c(0:4),
#                                                       att2 = c(0:4),
#                                                       att3 = c(0:4),
#                                                       att4 = c(0:4)))
#
#   obs <- runif(nrow(eligible_profiles), 1, 10000)
#
#   observed <- eligible_profiles |>
#     dplyr::mutate(n = obs,
#                   n = dplyr::case_when(n < 8000 ~ NA,
#                                        TRUE ~ n)) |>
#     dplyr::filter(!is.na(n))
#
#   # remove unobserved profiles
#   observed <- observed |>
#     dplyr::mutate(!!rlang::sym(observed_id) :=
#                     dplyr::case_when(is.na(!!rlang::sym(observed_id)) ~ 0,
#                                      TRUE ~ n),
#                   pct = !!rlang::sym(observed_id) /
#                     sum(!!rlang::sym(observed_id))) |>
#     dplyr::filter(!!rlang::sym(observed_id) != 0)
#
#   eligible_profiles <- eligible_profiles |>
#     # calculate total number of mastered attributes/skills
#     dplyr::rowwise() |>
#     dplyr::mutate(total = sum(dplyr::c_across(dplyr::everything()))) |>
#     dplyr::ungroup() |>
#     dplyr::arrange(.data$total, dplyr::across(dplyr::everything(),
#                                               dplyr::desc)) |>
#     # filter down to the total number in increments of the range of profiles
#     dplyr::mutate(keep = .data$total %% range_of_profiles == 0) |>
#     dplyr::filter(.data$keep) |>
#     dplyr::select(-"keep") |>
#     dplyr::group_by(.data$total) |>
#     dplyr::mutate(to_keep = dplyr::case_when(.data$total %in% c(5, 15) ~ 1,
#                                              TRUE ~ 999),
#                   num = dplyr::row_number()) |>
#     dplyr::ungroup() |>
#     dplyr::filter(.data$num <= .data$to_keep) |>
#     dplyr::select(-"to_keep", - "num")
#
#   # apply weighted sampling design
#   profile_sampling <- weighted_sampling(eligible_profiles, num_profiles,
#                                         observed, round, num_pls)
#
#   # check column names
#   testthat::expect_equal(colnames(profile_sampling),
#                          c(glue::glue("att{1:4}"), "pct", "total", "samples"))
#   # attribute scores are in correct format
#   testthat::expect_contains(c(0:4),
#                             profile_sampling |>
#                               dplyr::select(dplyr::starts_with("att")) |>
#                               tidyr::pivot_longer(cols = dplyr::everything(),
#                                                   names_to = "att",
#                                                   values_to = "att_score") |>
#                               dplyr::distinct(.data$att_score) |>
#                               dplyr::pull())
#   testthat::expect_equal(all(profile_sampling$pct >= 0), TRUE)
#   testthat::expect_equal(all(profile_sampling$pct <= 1), TRUE)
#   # all attribute totals are increment of the range of profiles
#   testthat::expect_equal(profile_sampling |>
#                            dplyr::mutate(increment = .data$total %% 5) |>
#                            dplyr::pull(.data$increment),
#                          rep(0, nrow(profile_sampling)))
#   # the number to be sampled from each total is correct
#   testthat::expect_equal(profile_sampling |>
#                            dplyr::distinct(.data$samples) |>
#                            dplyr::pull(),
#                          c(2))
#   # profile cample has the correct number of rows
#   testthat::expect_equal(nrow(profile_sampling),
#                          nrow(observed |>
#                                 dplyr::rowwise() |>
#                                 dplyr::mutate(
#                                   total =
#                                     sum(dplyr::c_across(
#                                       dplyr::starts_with("att")
#                                     ))
#                                 ) |>
#                                 dplyr::ungroup() |>
#                                 dplyr::filter(.data$total != 0) |>
#                                 dplyr::mutate(total = .data$total %% 5) |>
#                                 dplyr::filter(.data$total == 0)))
# })
