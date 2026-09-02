#' Evaluating the Machine Learning Model
#'
#' Evaluating the machine learning model.
#'
#' @param model_ratings A tibble containing the assigned profiles along with
#' the model-predicted performance level, the model-predicted probability for
#' each performance level, and the rating assigned by each panelist. This tibble
#' is the output of `predict_ml()` and is one row per panelist rating.
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
#' @param observed A tibble with one row for each attribute mastery profile that
#' was observed along with the number of times it was observed.
#' @param num_pls The number of performance levels that can be assigned to any
#' profile.
#' @param rating_id A character string for the field name of the panelists'
#' ratings (default is 'rating').
#' @param observed_count_label A character string for the field name of the
#' observed sample sizes in `observed` (default is 'n').
#' @param observed_proportion_label A character string for the field name of the
#' observed proportions in `observed` (default is 'prop').
#' @param output_dir The directory path for saving the output.
#'
#' @return A list containing the fitted model, the model predictions to the
#' rated profiles, and the predictions for all possible profiles.
#'
#' @export
eval_agreement <- function(
  model_ratings,
  metrics,
  observed,
  num_pls,
  rating_id = "rating",
  observed_count_label = "n",
  observed_proportion_label = "prop",
  output_dir
) {
  if (any(!is.character(metrics))) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(metrics),
      must = cli::format_message(paste0(
        "must be a character vector."
      ))
    )
  }

  if (
    any(!(metrics %in% c("accuracy", "adjacent", "kappa", "auc", "assignment")))
  ) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(metrics),
      must = cli::format_message(paste0(
        paste0(
          "must be one of: 'accuracy', 'adjacent', 'kappa', 'auc', or",
          "'assignment'."
        )
      ))
    )
  }

  if (length(metrics) == 0) {
    rdcmchecks::abort_bad_argument(
      arg = rlang::caller_arg(metrics),
      must = cli::format_message(paste0(
        "must be provided."
      ))
    )
  }

  att_vec <- observed |>
    dplyr::select(
      -!!rlang::sym(observed_count_label),
      -!!rlang::sym(observed_proportion_label)
    ) |>
    names()

  model_ratings <- model_ratings |>
    dplyr::mutate(
      !!rlang::sym(rating_id) := factor(
        !!rlang::sym(rating_id),
        levels = 1:num_pls
      )
    )

  res <- tibble::tibble()

  if ("accuracy" %in% metrics) {
    accuracy <- model_ratings |>
      yardstick::accuracy(truth = !!rlang::sym(rating_id), "pred_pl")

    res <- dplyr::bind_rows(res, accuracy)
  }

  if ("adjacent" %in% metrics) {
    adjacent_accuracy <- model_ratings |>
      dplyr::mutate(
        pred_pl = as.numeric(.data$pred_pl),
        !!rlang::sym(rating_id) := as.numeric(!!rlang::sym(rating_id)),
        adj_acc = as.numeric(abs(.data$pred_pl - !!rlang::sym(rating_id)) <= 1)
      ) |>
      dplyr::summarize(adj_acc = mean(.data$adj_acc)) |>
      dplyr::rename(.estimate = "adj_acc") |>
      dplyr::mutate(.metric = "adjacent_accuracy", .estimator = "multiclass") |>
      dplyr::select(".metric", ".estimator", ".estimate")

    res <- dplyr::bind_rows(res, adjacent_accuracy)
  }

  if ("auc" %in% metrics) {
    roc_auc <- model_ratings |>
      dplyr::select(-dplyr::any_of(att_vec)) |>
      yardstick::roc_auc(
        truth = !!rlang::sym(rating_id),
        dplyr::starts_with("prob")
      )

    res <- dplyr::bind_rows(res, roc_auc)
  }

  if ("kappa" %in% metrics) {
    cohens_kappa <- model_ratings |>
      yardstick::kap(truth = !!rlang::sym(rating_id), "pred_pl") |>
      dplyr::mutate(.metric = "cohens_kappa")

    res <- dplyr::bind_rows(res, cohens_kappa)
  }

  if ("assignment" %in% metrics) {
    assignment_stats <- model_ratings |>
      dplyr::select(dplyr::all_of(att_vec)) |>
      dplyr::distinct() |>
      dplyr::left_join(observed, by = att_vec) |>
      dplyr::summarize(
        n = sum(!!rlang::sym(observed_count_label)),
        prop = sum(!!rlang::sym(observed_proportion_label))
      )

    output_stats <- tibble::tibble(
      profiles_assigned = model_ratings |>
        dplyr::select(dplyr::any_of(att_vec)) |>
        dplyr::distinct() |>
        nrow(),
      students_with_assigned_profile = assignment_stats |>
        dplyr::pull(.data$n),
      prop_students_with_assigned_profile = assignment_stats |>
        dplyr::pull(.data$prop)
    )

    tmp_res <- tibble::tibble(
      .metric = c(
        "profiles_assigned",
        "students_with_assigned_profile",
        "prop_students_with_assigned_profile"
      ),
      .estimator = c(NA, NA, NA),
      .estimate = c(
        output_stats$profiles_assigned,
        output_stats$students_with_assigned_profile,
        output_stats$prop_students_with_assigned_profile
      )
    )

    res <- dplyr::bind_rows(res, tmp_res)
  }

  # Save output
  saveRDS(res, glue::glue("{output_dir}/evaluation_metrics.rds"))

  res
}
