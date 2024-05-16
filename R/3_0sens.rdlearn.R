#' @export
sens <- function(x, ...) UseMethod("sens")

#' Sensitivity Analysis for rdlearn Objects
#'
#' This function performs sensitivity analysis for the \code{rdlearn} object under different
#' smoothness multiplier (M) and the cost of treatment (cost).
#'
#' @param result An object of class \code{rdlearn} returned by the \code{\link{rdlearn}} function.
#' @param M A numeric value or vector specifying the multiplicative smoothness factor(s)
#'   for sensitivity analysis.
#' @param cost A numeric value or vector specifying the cost of treatment for calculating
#'   regret.
#'
#' @return An updated \code{rdlearn} object with the new cutoffs based on the provided values of M and cost.
#'
#' @examples
#' result <- rdlearn(y = "elig", x = "saber11", c = "cutoff",
#'                   group_name = "department", data = acces,
#'                   fold = 20, M = c(0, 1), cost = 0)
#' result2 <- sens(result, M = 1, cost = c(0, 0.2, 0.4, 0.6, 0.8, 1))
#' plot(result2)
#'
#' @export
sens.rdlearn <- function (
    result,
    M = NULL,
    cost = NULL
){
  # check M and cost
  if(missing(M))
    stop("M is missing")

  if(missing(cost))
    stop("cost is missing")

  if (length(M) > 1 && length(cost) > 1) {
    stop("Both M and cost are vectors.")
  }

  result$safe_cut <- safelearn(
      c.vec = result$org_cut,
      n = result$sample,
      q = result$num_group,
      cost = cost,
      M = M,
      group_name = result$group_name,
      cross_fit_result = result$cross_fit_result
    )
  result
}
