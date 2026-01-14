#' Stratified Sampling
#'
#' Define a function for stratified sampling that allows for specifying the
#' number of samples to take within each stratification while weighting the
#' probability of sampling each case.
#'
#' @param x A tibble with one row for each rater's rating of an assigned
#' profile.
#' @param by A character field that defines the stratifications.
#' @param size A character field that defines the sampling sizes within each
#' stratification.
#' @param weight_by A character field that defines the sampling weights within
#' each stratification.
#'
#' @return [tibble][tibble::tibble-package] A tibble containing the pinpoint
#' ranges to assign profiles during the second round of standard setting.
#'
#' @export
#' @examples
slice_stratified <- function(x, by, size, weight_by = NULL) {
  profiles_to_assign <- tibble::tibble()
  total_levels <- x |>
    dplyr::distinct(!!rlang::sym(by)) |>
    dplyr::pull(!!rlang::sym(by))

  for (ii in total_levels) {
    tmp <- x |>
      dplyr::filter(!!rlang::sym(by) == ii)

    to_sample <- tmp |>
      dplyr::distinct(!!rlang::sym(size)) |>
      dplyr::pull(!!rlang::sym(size))
    tmp <- tmp |>
      ratlas::only_if(!is.null(weight_by))(dplyr::slice_sample)(n = to_sample, weight_by = !!rlang::sym(weight_by)) |>
      ratlas::only_if(is.null(weight_by))(dplyr::slice_sample)(n = to_sample) |>
      ratlas::only_if(!is.null(weight_by))(dplyr::select)(-!!rlang::sym(size), -!!rlang::sym(weight_by)) |>
      ratlas::only_if(is.null(weight_by))(dplyr::select)(-!!rlang::sym(size))

    profiles_to_assign <- dplyr::bind_rows(profiles_to_assign, tmp)
  }

  return(profiles_to_assign)
}
