#' Use a Machine Learning Model to Predict Profiles' Performance Levels
#'
#' Predicting panelists' profile ratings with a machine learning model.
#'
#' @param fitted_model A tibble with one row for each attribute mastery
#' profiles that is eligible for assignment to raters.
#' @param ratings_data A tibble with the profiles rated by the panelists and
#' the panelists' ratings in long format.
#' @param possible_profiles A tibble with all of the possible profiles.
#' @param att_levels A numeric value for the number of levels where mastery can
#' be demonstrated. For example, `att_level` is 1 for a dichotomous attribute
#' (i.e., nonmastery or mastery), and `att_level` is 2 for attributes where the
#' possible scores are 0, 1, and 2.
#' @param num_pls The number of performance levels that can be assigned to any
#' profile.
#' @param rating_id A character string for the field name of the panelists'
#' ratings (default is 'rating').
#' @param output_dir The directory path for saving the output.
#'
#' @return A list containing the fitted model, the model predictions to the
#' rated profiles, and the predictions for all possible profiles.
#'
#' @export
assign_pl <- function(
  fitted_model,
  ratings_data,
  possible_profiles,
  att_levels,
  num_pls,
  rating_id = "rating",
  output_dir
) {
  att_vec <- ratings_data |>
    dplyr::select(
      -!!rlang::sym(rating_id),
      -dplyr::starts_with("rater"),
      -dplyr::starts_with("table")
    ) |>
    names()

  mod_ratings <- stats::predict(
    fitted_model,
    ratings_data |>
      dplyr::select(-!!rlang::sym(rating_id)) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(att_vec),
          ~ factor(., levels = 0:att_levels)
        )
      ),
    type = "class"
  ) |>
    dplyr::rename(pred_pl = ".pred_class") |>
    dplyr::bind_cols(
      ratings_data |>
        dplyr::select(-!!rlang::sym(rating_id))
    ) |>
    dplyr::select(dplyr::any_of(att_vec), "pred_pl")

  mod_probs <- stats::predict(
    fitted_model,
    ratings_data |>
      dplyr::select(-!!rlang::sym(rating_id)) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(att_vec),
          ~ factor(., levels = 0:att_levels)
        )
      ),
    type = "prob"
  )

  prob_labels <- glue::glue("prob_pl_{1:num_pls}")

  mod_ratings <- dplyr::bind_cols(mod_ratings, mod_probs)
  names(mod_ratings) <- c(att_vec, "pred_pl", prob_labels)

  mod_ratings <- mod_ratings |>
    dplyr::left_join(ratings_data, by = att_vec, relationship = "many-to-many")

  all_poss_pred <- stats::predict(
    fitted_model,
    possible_profiles |>
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(att_vec),
          ~ factor(., levels = 0:att_levels)
        )
      ),
    type = "class"
  ) |>
    dplyr::rename(pred_pl = ".pred_class") |>
    dplyr::bind_cols(possible_profiles) |>
    dplyr::select(dplyr::any_of(att_vec), "pred_pl")

  all_poss_probs <- stats::predict(
    fitted_model,
    possible_profiles |>
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(att_vec),
          ~ factor(., levels = 0:att_levels)
        )
      ),
    type = "prob"
  )

  prob_labels <- glue::glue("prob_pl_{1:num_pls}")

  all_poss_ratings <- dplyr::bind_cols(all_poss_pred, all_poss_probs)
  names(all_poss_ratings) <- c(att_vec, "pred_pl", prob_labels)

  # Save model predictions of ratings
  saveRDS(mod_ratings, glue::glue("{output_dir}/rated_profile_predictions.rds"))

  # Save model predictions for all possible profiles
  saveRDS(
    all_poss_ratings,
    glue::glue("{output_dir}/all_possible_profile_predictions.rds")
  )

  list(model_ratings = mod_ratings, all_possible_ratings = all_poss_ratings)
}
