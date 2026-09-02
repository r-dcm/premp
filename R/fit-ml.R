utils::globalVariables(c("case_wts"))

#' Fitting the Machine Learning Model
#'
#' Fit a machine learning model to the panelists' profile ratings.
#'
#' @param user_workflow A tibble with one row for each attribute mastery
#' profiles that is eligible for assignment to raters.
#' @param ratings_data A tibble with the profiles rated by the panelists and
#' the panelists' ratings in long format.
#' @param observed A tibble with one row for each attribute mastery profile that
#' was observed along with the number of times it was observed.
#' @param possible_profiles A tibble with all of the possible profiles.
#' @param att_levels A numeric value for the number of levels where mastery can
#' be demonstrated. For example, `att_level` is 1 for a dichotomous attribute
#' (i.e., nonmastery or mastery), and `att_level` is 2 for attributes where the
#' possible scores are 0, 1, and 2.
#' @param num_pls The number of performance levels that can be assigned to any
#' profile.
#' @param rating_id A character string for the field name of the panelists'
#' ratings (default is 'rating').
#' @param observed_count_label A character string for the field name of the
#' observed sample sizes in `observed` (default is 'n').
#' @param observed_proportion_label A character string for the field name of the
#' observed proportions in `observed` (default is 'prop').
#' @param metrics A character vector containing the evaluation metrics that
#' should be included in the output. Can include any of `"accuracy"`,
#' `"adjacent"`, `"kappa"`, `"auc"`, or `"assignment"`. Including `"accuracy"`
#' calculates classification accuracy. Including `"adjacent"` calculates
#' adjacent classification accuracy. Including `"kappa"` calculates Cohen's
#' kappa. Including `"auc"` calculates the area under the receiver operating
#' characteristic curve. Including `"assignment"` calculates assignment
#' statistics from the standard setting procedure -- the number of profiles
#' assigned, the number of students with the assigned profiles, and the
#' proportion of students with the assigned profiles.
#' @param output_dir The directory path for saving the output.
#'
#' @return A list containing the fitted model, the model predictions to the
#' rated profiles, and the predictions for all possible profiles.
#'
#' @export
fit_ml <- function(
  user_workflow,
  ratings_data,
  observed,
  possible_profiles,
  att_levels,
  num_pls,
  rating_id = "rating",
  observed_count_label = "n",
  observed_proportion_label = "prop",
  metrics,
  output_dir
) {
  att_vec <- observed |>
    dplyr::select(
      -!!rlang::sym(observed_count_label),
      -!!rlang::sym(observed_proportion_label)
    ) |>
    names()

  ratings_data <- ratings_data |>
    dplyr::select(dplyr::all_of(att_vec), !!rlang::sym(rating_id))

  data_split <- rsample::initial_split(ratings_data, prop = .90)
  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)

  train_data <- train_data |>
    dplyr::left_join(observed, by = att_vec) |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(att_vec),
      ~ factor(., levels = 0:att_levels)
    )) |>
    dplyr::mutate(rating = factor(.data$rating, levels = 1:num_pls)) |>
    dplyr::mutate(
      !!rlang::sym(observed_proportion_label) := dplyr::case_when(
        is.na(!!rlang::sym(observed_proportion_label)) ~ .000000001,
        TRUE ~ !!rlang::sym(observed_proportion_label)
      )
    ) |>
    dplyr::select(-!!rlang::sym(observed_count_label)) |>
    dplyr::rename(case_wts = !!rlang::sym(observed_proportion_label)) |>
    dplyr::mutate(case_wts = hardhat::importance_weights(.data$case_wts))

  mod_recipe <-
    recipes::recipe(rating ~ ., data = train_data)

  mod_wf <-
    workflows::workflow() |>
    workflows::add_recipe(mod_recipe) |>
    workflows::add_model(user_workflow) |>
    workflows::add_case_weights(case_wts)

  hyperparameters_present <- workflows::extract_parameter_set_dials(mod_wf) |>
    tibble::as_tibble() |>
    nrow() >
    0

  if (hyperparameters_present) {
    mod_folds <- rsample::vfold_cv(train_data)

    doParallel::registerDoParallel()

    all_pl_present <- train_data |>
      dplyr::distinct(!!rlang::sym(rating_id)) |>
      nrow() ==
      num_pls

    if (all_pl_present) {
      tuning_metric <- "roc_auc"
    } else {
      tuning_metric <- "accuracy"
    }

    suppressWarnings(
      suppressMessages(
        mod_tune <-
          tune::tune_grid(mod_wf, resamples = mod_folds, grid = 10)
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

  # Save fitted model
  saveRDS(mod_fit, glue::glue("{output_dir}/fitted_model.rds"))

  # in-sample agreement
  in_sample_preds <- assign_pl(
    mod_fit,
    train_data |>
      dplyr::select(-"case_wts"),
    possible_profiles,
    att_levels = 4,
    num_pls = 4,
    rating_id = "rating",
    output_dir = output_dir
  )

  in_sample_preds$model_ratings <- in_sample_preds$model_ratings |>
    dplyr::mutate(dplyr::across(dplyr::where(is.factor),
                                ~ as.numeric(as.character(.x)))) |>
    dplyr::mutate(pred_pl = factor(.data$pred_pl, levels = 1:num_pls),
                  rating = factor(.data$rating, levels = 1:num_pls))

  in_sample_agreement <- eval_agreement(
    model_ratings = in_sample_preds$model_ratings,
    metrics = c("accuracy", "auc", "kappa", "assignment"),
    observed,
    num_pls = 4,
    rating_id = "rating",
    observed_count_label = observed_count_label,
    observed_proportion_label = observed_proportion_label,
    output_dir = output_dir
  )

  # Save in-sample agreement
  saveRDS(in_sample_agreement,
          glue::glue("{output_dir}/in_sample_agreement.rds"))

  # out-of-sample agreement
  oos_preds <- assign_pl(
    mod_fit,
    test_data,
    possible_profiles,
    att_levels = 4,
    num_pls = 4,
    rating_id = "rating",
    output_dir = output_dir
  )

  oos_preds$model_ratings <- oos_preds$model_ratings |>
    dplyr::mutate(dplyr::across(dplyr::where(is.factor),
                                ~ as.numeric(as.character(.x)))) |>
    dplyr::mutate(pred_pl = factor(.data$pred_pl, levels = 1:num_pls),
                  rating = factor(.data$rating, levels = 1:num_pls))

  oos_agreement <- eval_agreement(
    model_ratings = oos_preds$model_ratings,
    metrics = c("accuracy", "auc", "kappa", "assignment"),
    observed,
    num_pls = 4,
    rating_id = "rating",
    observed_count_label = observed_count_label,
    observed_proportion_label = observed_proportion_label,
    output_dir = output_dir
  ) |>
    dplyr::filter(!stringr::str_detect(.data$.metric, "_assigned"))

  # Save in-sample agreement
  saveRDS(oos_agreement,
          glue::glue("{output_dir}/out_of_sample_agreement.rds"))

  pmp(mod_fit, in_sample_agreement, oos_agreement)
}
