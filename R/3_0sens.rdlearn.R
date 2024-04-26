#' @export
sens <- function(x, ...) UseMethod("sens")

#' Sensitivity analysis method for \code{rdlearn} objects
#'
#' \code{sens}
#'
#' @param result an object of class \code{rdlearn} returned by the \code{\link{rdlearn}}.
#' @param M multiplicative smoothness factor.
#' @param cost cost for calculating regret.
#'
#' @return an \code{rdlearn} object containing ...
#' @export
#'
#' @examples
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

  result$safecut <- safelearn(
      c.vec = result$basecut,
      n = result$sample,
      q = result$numgroup,
      cost = cost,
      M = M,
      groupname = result$groupname,
      temp_result = result$temp_result
    )
  result
}
