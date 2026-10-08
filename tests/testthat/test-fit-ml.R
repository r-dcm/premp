test_that("fitting the machine learning model with hyperparameters works", {
  workflow <-
    parsnip::rand_forest(
      mtry = 1,
      trees = parsnip::tune(),
      min_n = parsnip::tune()
    ) |>
    parsnip::set_mode("classification") |>
    parsnip::set_engine("ranger")

  ratings_data <- readRDS(testthat::test_path("data/ratings_data_ml.rds"))
  observed <- readRDS(testthat::test_path("data/observed_ml.rds")) |>
    dplyr::rename(prop = pct)

  possible_profiles <- tibble::tibble(tidyr::crossing(
    att1 = c(0:4),
    att2 = c(0:4),
    att3 = c(0:4),
    att4 = c(0:4),
    att5 = c(0:4),
    att6 = c(0:4),
    att7 = c(0:4),
    att8 = c(0:4)
  )) |>
    tibble::rowid_to_column("profile_id")

  observed <- observed |>
    dplyr::left_join(possible_profiles)

  mod_output <- fit_ml(
    workflow,
    ratings_data,
    observed,
    max_score = 4,
    num_pls = 4,
    metrics = c(
      "accuracy",
      "adjacent",
      "roc_auc",
      "cohens_kappa",
      "gwets_ac2",
      "assignment"
    ),
    output_dir = testthat::test_path("data")
  )

  # check output type
  testthat::expect_contains(class(mod_output@model), "workflow")
  testthat::expect_contains(class(mod_output@in_sample_agreement), "tbl_df")
  testthat::expect_contains(class(mod_output@out_of_sample_agreement), "tbl_df")

  # check column names
  testthat::expect_equal(
    colnames(mod_output@in_sample_agreement),
    c(".metric", ".estimator", ".estimate")
  )
  testthat::expect_equal(
    colnames(mod_output@out_of_sample_agreement),
    c(".metric", ".estimator", ".estimate")
  )

  # check for allowable values
  testthat::expect_equal(
    typeof(mod_output@in_sample_agreement$.estimate),
    "double"
  )
  testthat::expect_equal(
    typeof(mod_output@out_of_sample_agreement$.estimate),
    "double"
  )
  testthat::expect_true(all(
    mod_output@in_sample_agreement |>
      dplyr::filter(!is.na(.data$.estimator)) |>
      dplyr::pull(.data$.estimate) >=
      0
  ))
  testthat::expect_true(all(
    mod_output@out_of_sample_agreement |>
      dplyr::filter(!is.na(.data$.estimator)) |>
      dplyr::pull(.data$.estimate) >=
      0
  ))
  testthat::expect_true(all(
    mod_output@in_sample_agreement |>
      dplyr::filter(!is.na(.data$.estimator)) |>
      dplyr::pull(.data$.estimate) <=
      1
  ))
  testthat::expect_true(all(
    mod_output@out_of_sample_agreement |>
      dplyr::filter(!is.na(.data$.estimator)) |>
      dplyr::pull(.data$.estimate) <=
      1
  ))
})

test_that("fitting the machine learning model without hyperparamters works", {
  workflow <-
    parsnip::rand_forest(
      mtry = 1,
      trees = 100,
      min_n = 20
    ) |>
    parsnip::set_mode("classification") |>
    parsnip::set_engine("ranger")

  ratings_data <- readRDS(testthat::test_path("data/ratings_data_ml.rds"))
  observed <- readRDS(testthat::test_path("data/observed_ml.rds")) |>
    dplyr::rename(prop = pct)

  possible_profiles <- tibble::tibble(tidyr::crossing(
    att1 = c(0:4),
    att2 = c(0:4),
    att3 = c(0:4),
    att4 = c(0:4),
    att5 = c(0:4),
    att6 = c(0:4),
    att7 = c(0:4),
    att8 = c(0:4)
  )) |>
    tibble::rowid_to_column("profile_id")

  observed <- observed |>
    dplyr::left_join(possible_profiles)

  mod_output <- fit_ml(
    workflow,
    ratings_data,
    observed,
    max_score = 4,
    num_pls = 4,
    metrics = c(
      "accuracy",
      "adjacent",
      "cohens_kappa",
      "roc_auc",
      "gwets_ac2",
      "assignment"
    ),
    output_dir = testthat::test_path("data")
  )

  # check output type
  testthat::expect_contains(class(mod_output@model), "workflow")
  testthat::expect_contains(class(mod_output@in_sample_agreement), "tbl_df")
  testthat::expect_contains(class(mod_output@out_of_sample_agreement), "tbl_df")

  # check column names
  testthat::expect_equal(
    colnames(mod_output@in_sample_agreement),
    c(".metric", ".estimator", ".estimate")
  )
  testthat::expect_equal(
    colnames(mod_output@out_of_sample_agreement),
    c(".metric", ".estimator", ".estimate")
  )

  # check for allowable values
  testthat::expect_equal(
    typeof(mod_output@in_sample_agreement$.estimate),
    "double"
  )
  testthat::expect_equal(
    typeof(mod_output@out_of_sample_agreement$.estimate),
    "double"
  )
  testthat::expect_true(all(
    mod_output@in_sample_agreement |>
      dplyr::filter(!is.na(.data$.estimator)) |>
      dplyr::pull(.data$.estimate) >=
      0
  ))
  testthat::expect_true(all(
    mod_output@out_of_sample_agreement |>
      dplyr::filter(!is.na(.data$.estimator)) |>
      dplyr::pull(.data$.estimate) >=
      0
  ))
  testthat::expect_true(all(
    mod_output@in_sample_agreement |>
      dplyr::filter(!is.na(.data$.estimator)) |>
      dplyr::pull(.data$.estimate) <=
      1
  ))
  testthat::expect_true(all(
    mod_output@out_of_sample_agreement |>
      dplyr::filter(!is.na(.data$.estimator)) |>
      dplyr::pull(.data$.estimate) <=
      1
  ))
})
