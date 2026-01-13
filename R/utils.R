slice_stratified <- function(x, by, size, weight_by = NULL) {
  profiles_to_assign <- tibble::tibble()
  total_levels <- x %>%
    dplyr::distinct(!!rlang::sym(by)) %>%
    dplyr::pull(!!rlang::sym(by))

  for (ii in total_levels) {
    tmp <- x %>%
      dplyr::filter(!!rlang::sym(by) == ii)

    to_sample <- tmp %>%
      dplyr::distinct(!!rlang::sym(size)) %>%
      dplyr::pull(!!rlang::sym(size))
    tmp <- tmp %>%
      ratlas::only_if(!is.null(weight_by))(dplyr::slice_sample)(n = to_sample, weight_by = !!rlang::sym(weight_by)) %>%
      ratlas::only_if(is.null(weight_by))(dplyr::slice_sample)(n = to_sample) %>%
      ratlas::only_if(!is.null(weight_by))(dplyr::select)(-!!rlang::sym(size), -!!rlang::sym(weight_by)) %>%
      ratlas::only_if(is.null(weight_by))(dplyr::select)(-!!rlang::sym(size))

    profiles_to_assign <- dplyr::bind_rows(profiles_to_assign, tmp)
  }

  return(profiles_to_assign)
}
