#' Title
#'
#' @param raters
#' @param profiles
#' @param profiles_per_rater
#' @param raters_per_profile
#' @param profiles_seen_by_all
#'
#' @returns A [tibble][tibble::tibble-package].
#'
#' @export
#' @examples
assign_profiles <- function(
  raters,
  profiles,
  profiles_per_rater,
  raters_per_profile,
  profiles_seen_by_all = 1L
) {
  profiles_per_rater <- profiles_per_rater - profiles_seen_by_all
  total_ratings <- length(raters) * profiles_per_rater
  num_profiles <- floor(total_ratings / raters_per_profile)

  assignments <- tidyr::expand_grid(
    profile = seq_len(num_profiles),
    rater = raters,
    rated = 0L
  ) |>
    tidyr::pivot_wider(names_from = "rater", values_from = "rated") |>
    dplyr::select(-"profile")

  good_assignment <- FALSE
  while (!good_assignment) {
    new_assign <- assignments
    for (i in seq_along(new_assign)) {
      current_ratings <- rowSums(new_assign)
      ratings_count <- table(current_ratings)
      cum_count <- cumsum(ratings_count)

      min_profiles <- c(
        which(current_ratings == min(current_ratings)),
        which(
          current_ratings %in%
            as.integer(names(cum_count)[which(cum_count <= profiles_per_rater)])
        )
      ) |>
        unique()

      extra_profiles <- if (length(min_profiles) < profiles_per_rater) {
        remain_ratings <- current_ratings[-min_profiles]
        remain_count <- table(remain_ratings)
        cum_remain <- cumsum(remain_count)

        which(
          current_ratings %in%
            as.integer(names(which(
              cum_remain > (profiles_per_rater - length(min_profiles))
            )[1]))
        ) |>
          sample(size = (profiles_per_rater - length(min_profiles)))
      } else {
        NULL
      }
      all_profiles <- c(min_profiles, extra_profiles)

      selections <- sample(
        all_profiles,
        size = profiles_per_rater,
        replace = FALSE
      )
      new_assign[selections, i] <- 1L
    }

    if (
      all(colSums(new_assign) == profiles_per_rater) &&
        all(rowSums(new_assign) >= raters_per_profile)
    ) {
      assignments <- new_assign
      good_assignment <- TRUE
    }
  }
}
