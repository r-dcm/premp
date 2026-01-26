#' Fit Standard Setting Logistic Regression Model
#'
#' Estimate a logistic regression to predict the performance level from the
#' total number of linkage levels mastered in each profile.
#'
#' @param dat A data frame with 1 row per profile per rating. There are two
#'   required columns:
#'   1. `y`: A 0/1 indicator representing whether that profile was rated in the
#'      performance level of interest or higher. That is, if calculating the cut
#'      point between levels 1 and 2, was the profile rated in level 2, 3, or 4?
#'   2. `lls_mastered`: The total number of linkage levels mastered in the
#'      profile that was rated.
#'
#' @details
#' This function currently uses [brms::brm()] to estimate the logistic
#' regression, but any logistic function would do (e.g., [stats::glm()]). Note
#' that change the estimation engine would require down-stream adjustments as
#' well, as the post-processing and predictions currently assume that posterior
#' distributions will need to be summarized.
#'
#' @return A tibble with 1 row and 2 columns: `intercept` and `slope`. The one
#'   row contains objects of type [posterior::draws_rvars], which represent the
#'   posterior distribution for each parameter.
fit_model <- function(dat) {
  out <- capture.output(
    suppressMessages(
      mod <- brm(y ~ 1 + lls_mastered, data = dat, family = "bernoulli",
                 prior = c(prior(normal(0, 1.5), class = Intercept),
                           prior(normal(0, 0.5), class = b)),
                 iter = 4000, warmup = 2000, chains = 4, cores = 4, refresh = 0,
                 control = list(adapt_delta = 0.95, max_treedepth = 15))
    )
  )
  
  as_draws_rvars(mod, variable = c("b_Intercept", "b_lls_mastered")) %>%
    as_tibble() %>%
    rename(intercept = b_Intercept, slope = b_lls_mastered)
}


#' Ensure a maximum value
#'
#' This is a utility function to ensure functions execute correctly. For
#' example, when calculating Z-score, a cumulative percent of 1.0 is undefined.
#' This function can be used to replace all of the 1.0 with some other value,
#' (e.g., 0.9999) to ensure that a Z-score is calculated for each score point.
#'
#' @param x A double that should be capped at a specified value.
#' @param max_val The maximum value that `x` should be allowed to take.
#'
#' @return A double no larger than `max_val`.
max_value <- function(x, max_val = 0.9999) {
  min(x, max_val)
}
