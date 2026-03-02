test_that("condensed mastery method works", {
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
                                       profiles_per_rater = 10,
                                       raters_per_profile = 2,
                                       round = 1,
                                       num_pls = 4,
                                       range_of_profiles = 5,
                                       output_dir = testthat::test_path())

  observed <- final_assignments$observed
  final_assignments <- final_assignments$final_assignments

  profiles <- final_assignments |>
    dplyr::select(-"total", -dplyr::any_of(raters))

  ratings <- final_assignments |>
    tibble::rowid_to_column("profile_num") |> dplyr::select(-"total") |>
    tidyr::pivot_longer(cols = dplyr::starts_with("rater"),
                        names_to = "rater_id",
                        values_to = "rating") |>
    dplyr::filter(rating == 1) |>
    dplyr::rowwise() |>
    dplyr::mutate(total = sum(dplyr::c_across(dplyr::starts_with("EE")))) |>
    dplyr::ungroup() |>
    dplyr::mutate(bump = runif(30, -.75, .75),
                  bump = dplyr::case_when(.data$bump <= -.5 ~ -1,
                                          .data$bump >= .5 ~ 1,
                                          TRUE ~ 0),
                  base = dplyr::case_when(.data$total <= 8 ~ 1,
                                          .data$total <= 16 ~ 2,
                                          .data$total <= 24 ~ 3,
                                          TRUE ~ 4),
                  rating = .data$base + .data$bump) |>
    dplyr::rowwise() |>
    dplyr::mutate(rating = max(1, .data$rating),
                  rating = min(4, .data$rating)) |>
    dplyr::ungroup() |>
    dplyr::select(-"total", -"bump", -"base", -"rater_id")

  pl_labels <- c("Emerging", "Approaching the Target", "At Target", "Advanced")

  condensed_mastery(ratings, profiles, pl_labels, att_levels = 4,
                    cores = 1, chains = 1,
                    output_dir = testthat::test_path("data"))

  cm_output <-
    readr::read_csv(glue::glue("{testthat::test_path('data')}/pp-range.csv"))

  # check output type
  testthat::expect_contains(class(cm_output), "tbl_df")

  # check column names
  testthat::expect_equal(colnames(cm_output),
                         c("cut_point", "predicted", "pinpoint_min",
                           "pinpoint_max"))

  # check number of cut-points
  testthat::expect_equal(nrow(cm_output), length(pl_labels) - 1)

  # check variable types
  testthat::expect_equal(typeof(cm_output$cut_point), "character")
  testthat::expect_equal(typeof(cm_output$predicted), "double")
  testthat::expect_equal(typeof(cm_output$pinpoint_min), "double")
  testthat::expect_equal(typeof(cm_output$pinpoint_max), "double")

  # check for allowable values for pinpoint min and max
  testthat::expect_gte(min(cm_output$pinpoint_min), 0)
  testthat::expect_lte(max(cm_output$pinpoint_max), 16)

})

test_that("error works", {
  err <- rlang::catch_cnd(condensed_mastery(ratings = NULL,
                                            profiles = NULL,
                                            pl_labels = 1,
                                            att_levels = 4,
                                            output_dir =
                                              testthat::test_path("data")
  ) )
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "must have a value of at least 2."
  )
})

