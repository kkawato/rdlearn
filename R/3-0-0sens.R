#' Sensitivity Analysis for rdlearn Objects
#'
#' This function performs sensitivity analysis for the \code{rdlearn} object
#' under different smoothness multiplier (M) and the cost of treatment (cost).
#'
#' @param object An object of class \code{rdlearn} returned by the
#'   \code{\link{rdlearn}} function.
#' @param M A numeric value or vector specifying the multiplicative smoothness
#'   factor(s) for sensitivity analysis.
#' @param cost A numeric value or vector specifying the cost of treatment for
#'   calculating regret.
#' @param trace A logical value that controls whether to display the progress of
#'   cross-fitting and regret calculation. If set to TRUE, the progress will be
#'   printed. The default value is TRUE.
#' @return An updated \code{rdlearn} object with the new cutoffs based on the
#'   provided values of M and cost.
#' @inherit package_rdlearn examples
#'
#' @export
sens <- function(
    object,
    M = NULL,
    cost = NULL,
    trace = TRUE) {
  # check arguments
  if (missing(object) || !inherits(object, "rdlearn")) {
    stop("'object' must be of class 'rdlearn'.")
  }

  if (missing(M)) {
    stop("M is missing")
  }

  if (missing(cost)) {
    stop("cost is missing")
  }

  if (length(M) > 1 && length(cost) > 1) {
    stop("Both M and cost are vectors.")
  }

  new_result <- safelearn(
    c.vec = object$org_cut,
    n = object$sample,
    q = object$num_group,
    cost = cost,
    M = M,
    group_name_vec = object$group_name,
    dif_lip_output = object$dif_lip_output,
    cross_fit_output = object$cross_fit_output,
    trace = trace
  )

  object$safe_cut <- new_result$safe_cut
  object$dif_cut <- new_result$dif_cut
  object$temp_reg_df <- new_result$temp_reg_df

  object
}
