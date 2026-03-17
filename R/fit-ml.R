utils::globalVariables(c("case_wts"))

#' Fitting the Machine Learning Model
#'
#' Fit a machine learning model to the panelists' profile ratings.
#'
#' @param user_workflow A tibble with one row for each attribute mastery
#' profiles that is eligible for assignment to raters.
#' @param ratings_data A tibble with the profiles rated by the panelists and
#' the panelists' ratings in long format.
#' @param possible_profiles A tibble with all of the possible profiles.
#' @param observed A tibble with one row for each attribute mastery profile that
#' was observed along with the number of times it was observed.
#' @param att_levels A numeric value for the number of levels where mastery can
#' be demonstrated. For example, `att_level` is 1 for a dichotomous attribute
#' (i.e., nonmastery or mastery), and `att_level` is 2 for attributes where the
#' possible scores are 0, 1, and 2.
#' @param num_pls The number of performance levels that can be assigned to any
#' profile.
#' @param output_dir The directory path for saving the output.
#'
#' @return A list containing the fitted model, the model predictions to the
#' rated profiles, and the predictions for all possible profiles.
#'
#' @export
fit_ml <- function(
  user_workflow,
  ratings_data,
  possible_profiles,
  observed,
  att_levels,
  num_pls,
  output_dir
) {
  att_vec <- observed |>
    dplyr::select(-"n", -"pct") |>
    names()

  ratings_data <- ratings_data |>
    dplyr::select(dplyr::all_of(att_vec), "rating")

  data_split <- rsample::initial_split(ratings_data, prop = .90)
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)

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

  mod_ratings <- dplyr::bind_cols(mod_ratings,
                                  ratings_data |>
                                    dplyr::select("rating"))

  all_poss_pred <- stats::predict(mod_fit,
                                  possible_profiles |>
                                    dplyr::mutate(
                                      dplyr::across(dplyr::any_of(att_vec),
                                                    ~ factor(.,
                                                             levels =
                                                               0:att_levels))
                                    ),
                                  type = "class") |>

    dplyr::rename(pred_pl = ".pred_class") |>
    dplyr::bind_cols(possible_profiles) |>
    dplyr::select(dplyr::any_of(att_vec), "pred_pl")

  all_poss_probs <- stats::predict(mod_fit,
                                   possible_profiles |>
                                     dplyr::mutate(
                                       dplyr::across(dplyr::any_of(att_vec),
                                                     ~ factor(.,
                                                              levels =
                                                                0:att_levels))
                                     ),
                                   type = "prob")

  prob_labels <- glue::glue("prob_pl_{1:num_pls}")

  all_poss_ratings <- dplyr::bind_cols(all_poss_pred, all_poss_probs)
  names(all_poss_ratings) <- c(att_vec, "pred_pl", prob_labels)

  acc <- mod_ratings |>
    dplyr::select("pred_pl", "rating") |>
    dplyr::mutate(pred_pl = as.numeric(.data$pred_pl),
                  acc = as.numeric(.data$pred_pl == .data$rating)) |>
    dplyr::summarize(acc = mean(.data$acc)) |>
    dplyr::pull(.data$acc)

  assignment_stats <- mod_ratings |>
    dplyr::select(dplyr::all_of(att_vec)) |>
    dplyr::distinct() |>
    dplyr::left_join(observed, by = att_vec) |>
    dplyr::summarize(n = sum(.data$n),
                     pct = sum(.data$pct))

  output_stats <- tibble::tibble(profiles_assigned = mod_ratings |>
                                   dplyr::select(dplyr::any_of(att_vec)) |>
                                   dplyr::distinct() |>
                                   nrow(),
                                 students_with_assigned_profile =
                                   assignment_stats$n,
                                 pct_students_with_assigned_profile =
                                   assignment_stats |>
                                   dplyr::pull(.data$pct),
                                 prediction_accuracy = acc)

  # Save fitted model
  saveRDS(mod_fit, glue::glue("{output_dir}/fitted_model.rds"))

  # Save model predictions of ratings
  saveRDS(mod_ratings, glue::glue("{output_dir}/rated_profile_predictions.rds"))

  # Save model predictions for all possible profiles
  saveRDS(all_poss_ratings,
          glue::glue("{output_dir}/all_possible_profile_predictions.rds"))

  # Save overall stats
  saveRDS(output_stats,
          glue::glue("{output_dir}/assignment_stats.rds"))

  ret_list <- list(fitted_model = mod_fit,
                   rated_profile_predictions = mod_ratings,
                   all_possible_profile_predictions = all_poss_ratings,
                   assignment_stats = output_stats)

  return(ret_list)
}
