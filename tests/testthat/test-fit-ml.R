test_that("fitting the machine learning model works", {
  workflow <-
    parsnip::rand_forest(mtry = 1,
                         trees = parsnip::tune(),
                         min_n = parsnip::tune()) |>
    parsnip::set_mode("classification") |>
    parsnip::set_engine("ranger")

  ratings_data <- readRDS(testthat::test_path("data/ratings_data_ml.rds"))
  observed <- readRDS(testthat::test_path("data/observed_ml.rds"))

  fit_ml(workflow, ratings_data, observed, att_levels = 5, num_pls = 4,
         output_dir = testthat::test_path("data"))

  mod_in_sample_ca <- readRDS(testthat::test_path("data/mod_in_sample_ca.rds"))
  mod_out_of_sample_ca <-
    readRDS(testthat::test_path("data/mod_out_of_sample_ca.rds"))
  mod_ratings <- readRDS(testthat::test_path("data/profile_predictions.rds"))

  # check output type
  testthat::expect_contains(class(mod_in_sample_ca), "tbl_df")
  testthat::expect_contains(class(mod_out_of_sample_ca), "tbl_df")
  testthat::expect_contains(class(mod_ratings), "tbl_df")

  # check column names
  testthat::expect_equal(colnames(mod_in_sample_ca),
                         c(".metric", ".estimator", ".estimate"))
  testthat::expect_equal(colnames(mod_out_of_sample_ca),
                         c(".metric", ".estimator", ".estimate"))
  testthat::expect_equal(colnames(mod_ratings),
                         c(glue::glue("att{1:8}"), "pred_pl",
                         glue::glue("prob_pl_{1:4}"), "rating"))

  # check for allowable values
  testthat::expect_gte(min(mod_in_sample_ca$.estimate), 0)
  testthat::expect_lte(max(mod_in_sample_ca$.estimate), 1)
  testthat::expect_gte(min(mod_out_of_sample_ca$.estimate), 0)
  testthat::expect_lte(max(mod_out_of_sample_ca$.estimate), 1)
  testthat::expect_contains(c(0:4),
                            mod_ratings |>
                              dplyr::select(dplyr::starts_with("att")) |>
                              tidyr::pivot_longer(cols = dplyr::everything(),
                                                  names_to = "att",
                                                  values_to = "mastered") |>
                              dplyr::pull(.data$mastered))
  testthat::expect_contains(c(1:4),
                            mod_ratings$pred_pl)
  testthat::expect_gte(mod_ratings |>
                         dplyr::select(dplyr::starts_with("prob_pl_")) |>
                         tidyr::pivot_longer(cols = dplyr::everything(),
                                             names_to = "pl",
                                             values_to = "prob") |>
                         dplyr::filter(.data$prob == min(.data$prob)) |>
                         dplyr::distinct(.data$prob) |>
                         dplyr::pull(.data$prob),
                       0)
  testthat::expect_lte(mod_ratings |>
                         dplyr::select(dplyr::starts_with("prob_pl_")) |>
                         tidyr::pivot_longer(cols = dplyr::everything(),
                                             names_to = "pl",
                                             values_to = "prob") |>
                         dplyr::filter(.data$prob == min(.data$prob)) |>
                         dplyr::distinct(.data$prob) |>
                         dplyr::pull(.data$prob),
                       1)
  testthat::expect_contains(c(1:4),
                            mod_ratings$rating)
})
