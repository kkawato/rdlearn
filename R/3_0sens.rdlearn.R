#' @export
sens <- function(x, ...) UseMethod("sens")

#' Sensitivity analysis method for \code{rdlearn} objects
#'
#' \code{sens}
#'
#' @param result An object of class \code{rdlearn} returned by the \code{\link{rdlearn}}.
#' @param M A multiplicative smoothness factor.
#' @param cost A cost for calculating regret.
#' @return an \code{rdlearn} object containing the same contents as the result of function \code{{rdlearn}}.
#'
#' @examples
#' result2 <- sens(result, M = 1, cost=c(0, 0.2, 0.4, 0.6, 0.8, 1))
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
