#' Summary function
#'
#' @param object An object of class \code{rdlearn} returned by the
#'   \code{\link{rdlearn}} function.
#' @param ... additional arguments.
#' @return Displays key outputs from the \code{\link{rdlearn}} function. It
#'   provides basic information and RD causal effect estimates from
#'   \code{\link{rdestimate}}, as well as the safe cutoffs derived by
#'   \code{\link{rdlearn}} and the difference between them and the original
#'   cutoffs.
#' @importFrom cli cli_h1
#' @inherit rdlearn examples
#'
#' @export
summary.rdlearn <- function(object, ...) {
  # rdestimates
  cli_h1("Basic Information")
  print(object$rdestimates)

  # safe and original cutoffs
  cli_h1("Safe Cutoffs and Original Cutoff")
  extended_safe_cut <- cbind(object$org_cut, object$safe_cut)
  colnames(extended_safe_cut)[1] <- "original"
  print(extended_safe_cut)

  # difference of cutoffs
  cli_h1("Numerical Difference of Cutoffs")
  print(object$dif_cut)

  # measures of differences
  cli_h1("Measures of Difference")
  print(object$distance)
}
