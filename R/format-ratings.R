format_ratings <- function(
  raw_ratings
) {

  # format into a long format with one column for profile id, one column for each attribute, and one column for the rating
  # raw_ratings |>
  #   tidyr::pivot_longer(cols = dplyr::starts_with("rater"),
  #                       names_to = "rater_id",
  #                       values_to = "rating") |>
  #   dplyr::select(-"rater_id") |>
}
