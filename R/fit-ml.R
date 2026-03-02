utils::globalVariables(c("case_wts"))

fit_ml <- function(
  user_workflow,
  ratings_data,
  observed,
  att_levels,
  num_pls,
  output_dir
) {
  data_split <- rsample::initial_split(ratings_data, prop = .90)
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)

  att_vec <- observed |>
    dplyr::select(-"n", -"pct") |>
    names()

  train_data <- train_data |>
    dplyr::left_join(observed, by = att_vec) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(att_vec),
                                ~ factor(., levels = 0:att_levels))) |>
    dplyr::mutate(rating = factor(.data$rating, levels = 1:num_pls)) |>
    dplyr::mutate(pct = dplyr::case_when(is.na(pct) ~ .000000001,
                                         TRUE ~ pct)) |>
    dplyr::select(-"n") |>
    dplyr::rename(case_wts = "pct") |>
    dplyr::mutate(case_wts = hardhat::importance_weights(.data$case_wts))

  mod_recipe <-
    recipes::recipe(rating ~ .,
                    data = train_data)

  mod_wf <-
    workflows::workflow() |>
    workflows::add_recipe(mod_recipe) |>
    workflows::add_model(user_workflow) |>
    workflows::add_case_weights(case_wts)

  hyperparameters_present <- workflows::extract_parameter_set_dials(mod_wf) |>
    tibble::as_tibble() |>
    nrow() > 0

  if (hyperparameters_present) {
    mod_folds <- rsample::vfold_cv(train_data)

    doParallel::registerDoParallel()

    all_pl_present <- train_data |>
      dplyr::distinct(.data$rating) |>
      nrow() == num_pls

    if (all_pl_present) {
      tuning_metric <- "roc_auc"
    } else {
      tuning_metric <- "accuracy"
    }

    suppressWarnings(
      suppressMessages(
        mod_tune <-
          tune::tune_grid(mod_wf,
                          resamples = mod_folds,
                          # metrics =
                          #   yardstick::metric_set(!!rlang::sym(tuning_metric)),
                          grid = 10)
      )
    )

    hyperparameters <- tune::select_best(mod_tune, metric = tuning_metric)

    tuned_mod <- tune::finalize_model(user_workflow, hyperparameters)

    final_wf <-
      workflows::workflow() |>
      workflows::add_recipe(mod_recipe) |>
      workflows::add_model(tuned_mod) |>
      workflows::add_case_weights(case_wts)
  } else {
    final_wf <- mod_wf
  }

  mod_fit <-
    final_wf |>
    workflows::fit(data = train_data)

  mod_ratings <- stats::predict(mod_fit,
                                  ratings_data |>
                                    dplyr::select(-"rating") |>
                                    dplyr::mutate(
                                      dplyr::across(dplyr::any_of(att_vec),
                                                    ~ factor(.,
                                                             levels =
                                                               0:att_levels))
                                    ),
                                  type = "class") |>

    dplyr::rename(pred_pl = ".pred_class") |>
    dplyr::bind_cols(ratings_data |>
                       dplyr::select(-"rating")) |>
    dplyr::select(dplyr::any_of(att_vec), "pred_pl")

  mod_probs <- stats::predict(mod_fit,
                                ratings_data |>
                                  dplyr::select(-"rating") |>
                                  dplyr::mutate(
                                    dplyr::across(dplyr::any_of(att_vec),
                                                  ~ factor(.,
                                                           levels =
                                                             0:att_levels))
                                  ),
                                type = "prob")

  prob_labels <- glue::glue("prob_pl_{1:num_pls}")

  mod_ratings <- dplyr::bind_cols(mod_ratings, mod_probs)
  names(mod_ratings) <- c(att_vec, "pred_pl", prob_labels)

  mod_in_sample_ca <- stats::predict(mod_fit, train_data, type = "class") |>
    dplyr::bind_cols(train_data |>
                       dplyr::select("rating")) |>
    dplyr::rename(predicted_pl = ".pred_class",
                  true_rating = "rating") |>
    dplyr::select("predicted_pl", "true_rating") |>
    yardstick::accuracy(truth = "true_rating",
                        "predicted_pl")

  mod_out_of_sample_ca <- stats::predict(mod_fit,
                                           test_data |>
                                             dplyr::mutate(
                                               dplyr::across(
                                                 dplyr::any_of(att_vec),
                                                 ~ factor(.,
                                                          levels = 0:att_levels)
                                               )
                                             ),
                                           type = "class") |>
    dplyr::bind_cols(test_data |>
                       dplyr::select("rating")) |>
    dplyr::rename(predicted_pl = ".pred_class",
                  true_rating = "rating") |>
    dplyr::select("predicted_pl", "true_rating") |>
    dplyr::mutate(true_rating = factor(.data$true_rating,
                                       levels = 1:num_pls)) |>
    yardstick::accuracy(truth = "true_rating",
                        "predicted_pl")

  mod_ratings <- dplyr::bind_cols(mod_ratings,
                                  ratings_data |>
                                    dplyr::select("rating"))

  # Save in-sample outcome measures --------------------------------------
  saveRDS(mod_in_sample_ca, glue::glue("{output_dir}/mod_in_sample_ca.rds"))

  # Save out-of-sample outcome measures --------------------------------------
  saveRDS(mod_out_of_sample_ca,
          glue::glue("{output_dir}/mod_out_of_sample_ca.rds"))

  # Save model predictions
  saveRDS(mod_ratings, glue::glue("{output_dir}/profile_predictions.rds"))
}
