#' @export
sens <- function(x, ...) UseMethod("sens")

#' Sensitivity analysis method for \code{rdlearn} objects
#'
#' \code{sens}
#'
#' @param result An object of class \code{rdlearn} returned by the \code{\link{rdlearn}}.
#' @param M A multiplicative smoothness factor.
#' @param cost A cost for calculating regret.
#' @return an \code{rdlearn} object containing ...
#' @export
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

  result$safecut <- safelearn(
      c.vec = result$orgcut,
      n = result$sample,
      q = result$numgroup,
      cost = cost,
      M = M,
      groupname = result$groupname,
      temp_result = result$temp_result
    )
  result
}
