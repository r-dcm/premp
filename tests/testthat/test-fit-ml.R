test_that("fitting the machine learning model works", {
  workflow <-
    parsnip::rand_forest(mtry = 1,
                         trees = parsnip::tune(),
                         min_n = parsnip::tune()) |>
    parsnip::set_mode("classification") |>
    parsnip::set_engine("ranger")

  ratings_data <- readRDS(testthat::test_path("data/ratings_data_ml.rds"))
  observed <- readRDS(testthat::test_path("data/observed_ml.rds"))

  possible_profiles <- tibble::tibble(tidyr::crossing(att1 = c(0:4),
                                                      att2 = c(0:4),
                                                      att3 = c(0:4),
                                                      att4 = c(0:4),
                                                      att5 = c(0:4),
                                                      att6 = c(0:4),
                                                      att7 = c(0:4),
                                                      att8 = c(0:4)))

  mod_output <- fit_ml(workflow, ratings_data, possible_profiles, observed,
                       att_levels = 4, num_pls = 4,
                       output_dir = testthat::test_path("data"))

  profile_preds <- mod_output$rated_profile_predictions
  poss_preds <- mod_output$all_possible_profile_predictions
  assignment_stats <- mod_output$assignment_stats

  # check output type
  testthat::expect_contains(class(profile_preds), "tbl_df")
  testthat::expect_contains(class(poss_preds), "tbl_df")
  testthat::expect_contains(class(assignment_stats), "tbl_df")

  # check column names
  testthat::expect_equal(colnames(profile_preds),
                         c(glue::glue("att{1:8}"), "pred_pl",
                           glue::glue("prob_pl_{1:4}"), "rating"))
  testthat::expect_equal(colnames(poss_preds),
                         c(glue::glue("att{1:8}"), "pred_pl",
                           glue::glue("prob_pl_{1:4}")))
  testthat::expect_equal(colnames(assignment_stats),
                         c("profiles_assigned",
                           "students_with_assigned_profile",
                           "pct_students_with_assigned_profile",
                           "prediction_accuracy"))

  # check for allowable values
  testthat::expect_gte(assignment_stats$prediction_accuracy, 0)
  testthat::expect_lte(assignment_stats$prediction_accuracy, 1)
  testthat::expect_gte(assignment_stats$pct_students_with_assigned_profile, 0)
  testthat::expect_lte(assignment_stats$pct_students_with_assigned_profile, 1)
  testthat::expect_equal(assignment_stats$profiles_assigned,
                         profile_preds |>
                           dplyr::select(dplyr::starts_with("att")) |>
                           dplyr::distinct() |>
                           nrow())
  testthat::expect_equal(assignment_stats$students_with_assigned_profile,
                         profile_preds |>
                           dplyr::select(dplyr::starts_with("att")) |>
                           dplyr::distinct() |>
                           dplyr::left_join(observed,
                                            by = glue::glue("att{1:8}")) |>
                           dplyr::summarize(n = sum(.data$n)) |>
                           dplyr::pull(.data$n))
  testthat::expect_contains(c(0:4),
                            profile_preds |>
                              dplyr::select(dplyr::starts_with("att")) |>
                              tidyr::pivot_longer(cols = dplyr::everything(),
                                                  names_to = "att",
                                                  values_to = "mastered") |>
                              dplyr::pull(.data$mastered))
  testthat::expect_contains(c(1:4),
                            profile_preds |>
                              dplyr::select("pred_pl", "rating") |>
                              dplyr::mutate(pred_pl =
                                              as.numeric(.data$pred_pl)) |>
                              tidyr::pivot_longer(cols = dplyr::everything(),
                                                  names_to = "att",
                                                  values_to = "mastered") |>
                              dplyr::pull(.data$mastered))
  testthat::expect_gte(profile_preds |>
                         dplyr::select(dplyr::starts_with("prob_pl_")) |>
                         tidyr::pivot_longer(cols = dplyr::everything(),
                                             names_to = "pl",
                                             values_to = "prob") |>
                         dplyr::filter(.data$prob == min(.data$prob)) |>
                         dplyr::distinct(.data$prob) |>
                         dplyr::pull(.data$prob),
                       0)
  testthat::expect_lte(profile_preds |>
                         dplyr::select(dplyr::starts_with("prob_pl_")) |>
                         tidyr::pivot_longer(cols = dplyr::everything(),
                                             names_to = "pl",
                                             values_to = "prob") |>
                         dplyr::filter(.data$prob == min(.data$prob)) |>
                         dplyr::distinct(.data$prob) |>
                         dplyr::pull(.data$prob),
                       1)
  testthat::expect_contains(c(0:4),
                            poss_preds |>
                              dplyr::select(dplyr::starts_with("att")) |>
                              tidyr::pivot_longer(cols = dplyr::everything(),
                                                  names_to = "att",
                                                  values_to = "mastered") |>
                              dplyr::pull(.data$mastered))
  testthat::expect_contains(c(1:4),
                            poss_preds |>
                              dplyr::select("pred_pl") |>
                              dplyr::mutate(pred_pl =
                                              as.numeric(.data$pred_pl)) |>
                              dplyr::pull(.data$pred_pl))
  testthat::expect_gte(poss_preds |>
                         dplyr::select(dplyr::starts_with("prob_pl_")) |>
                         tidyr::pivot_longer(cols = dplyr::everything(),
                                             names_to = "pl",
                                             values_to = "prob") |>
                         dplyr::filter(.data$prob == min(.data$prob)) |>
                         dplyr::distinct(.data$prob) |>
                         dplyr::pull(.data$prob),
                       0)
  testthat::expect_lte(poss_preds |>
                         dplyr::select(dplyr::starts_with("prob_pl_")) |>
                         tidyr::pivot_longer(cols = dplyr::everything(),
                                             names_to = "pl",
                                             values_to = "prob") |>
                         dplyr::filter(.data$prob == min(.data$prob)) |>
                         dplyr::distinct(.data$prob) |>
                         dplyr::pull(.data$prob),
                       1)
})
