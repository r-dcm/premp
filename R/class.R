#' S7 class for model estimation and evaluation
#'
#' The estimated model, in-sample agreement metrics, and out-of-sample
#' agreement metrics are exported as an S7 class.
#'
#' @param model The workflow for the fitted model.
#' @param in_sample_agreement A tibble containing the in-sample agreement
#' statistics.
#' @param out_of_sample_agreement A tibble containing the out-of-sample
#' agreement statistics.
#'
#' @return An [S7 object][S7::S7_object()] with the corresponding class.
#' @rdname pmp-class
#' @name pmp-class
#'
#' @export
# create S7 class with output
pmp <- S7::new_class(
  name = "pmp",
  properties = list(
    model = S7::new_property(
      class = S7::new_S3_class("workflow"),
      default = quote(.no_constructor())
    ),
    in_sample_agreement = S7::new_property(
      class = S7::new_S3_class("tbl_df"),
      default = quote(.no_constructor())
    ),
    out_of_sample_agreement = S7::new_property(
      class = S7::new_S3_class("tbl_df"),
      default = quote(.no_constructor())
    )
  )
)

.no_constructor <- function(.data) {
  stop(
    sprintf("S3 class <%s> doesn't have a constructor", class(.data)[[1]]),
    call. = FALSE
  )
}
