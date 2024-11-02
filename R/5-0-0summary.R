#' Summary function
#'
#' @param result An object of class \code{rdlearn} returned by the
#'   \code{\link{rdlearn}} function.
#' @return Displays key outputs from the \code{\link{rdlearn}} function. It
#'   provides basic information and RD causal effect estimates from
#'   \code{\link{rdesimate}}, as well as the safe cutoffs derived by
#'   \code{\link{rdlearn}} and the difference between them and the original
#'   cutoffs.
#'
#' @export
summary.rdlearn <- function(result) {
  # rdestimates
  cat("\n----------\n")
  cat("Basic Information\n")
  print(result$rdestimates)

  # safe and original cutoffs
  cat("\n----------\n")
  cat("Safe Cutoffs and Original Cutoff\n")
  extended_safe_cut <- cbind(result$org_cut, result$safe_cut)
  colnames(extended_safe_cut)[1] <- "original"
  print(extended_safe_cut)

  # difference of cutoffs
  cat("\n----------\n")
  cat("Numerical Difference of Cutoffs\n")
  print(result$dif_cut)

  # measures of differences
  cat("\n----------\n")
  cat("Measures of Difference\n")
  print(result$distance)
}
