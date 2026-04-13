test_that("condensed mastery method works", {
  set.seed(123)

  possible_profiles <- tibble::tibble(tidyr::crossing(att1 = c(0:4),
                                                      att2 = c(0:4),
                                                      att3 = c(0:4),
                                                      att4 = c(0:4)))

  obs <- runif(nrow(possible_profiles), 1, 10000)

  observed <- possible_profiles |>
    dplyr::mutate(n = obs,
                  n = dplyr::case_when(n < 1000 ~ NA,
                                       TRUE ~ n)) |>
    dplyr::filter(!is.na(n)) |>
    dplyr::mutate(pct = n / sum(n))

  final_assignments <- assign_profiles(
    num_assignment_groups = 5L,
    table_configuration = list(panelists_per_table = 4L,
                               proportion_of_shared_profiles = .67),
    possible_profiles = possible_profiles,
    observed = observed,
    range_of_profiles = c(5L, 10L, 15L),
    profiles_per_level = 3L,
    output_dir = testthat::test_path("data")
  )

  final_assignments <- final_assignments$profile_sampling

  profiles <- final_assignments |>
    dplyr::select(-"total", -"table", -dplyr::starts_with("rater"))

  ratings <- final_assignments |>
    tibble::rowid_to_column("profile_num") |>
    dplyr::select(-"total") |>
    tidyr::pivot_longer(cols = dplyr::starts_with("rater"),
                        names_to = "rater_id",
                        values_to = "rating") |>
    dplyr::filter(rating == 1) |>
    dplyr::rowwise() |>
    dplyr::mutate(total = sum(dplyr::c_across(dplyr::starts_with("att")))) |>
    dplyr::ungroup() |>
    dplyr::mutate(bump = runif(144, -.75, .75),
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

  condensed_mastery(ratings, pl_labels, att_levels = 4,
                    cores = 1, chains = 1,
                    output_dir = testthat::test_path("data"))

  suppressMessages(
    cm_output <-
      readr::read_csv(glue::glue("{testthat::test_path('data')}/pp-range.csv"))
  )

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
  err <- rlang::catch_cnd(
    condensed_mastery(
      ratings = NULL,
      pl_labels = 1,
      att_levels = 4,
      output_dir =
        testthat::test_path("data")
    )
  )
  testthat::expect_s3_class(err, "rlang_error")
  testthat::expect_match(
    err$message,
    "must have a value of at least 2."
  )
})
