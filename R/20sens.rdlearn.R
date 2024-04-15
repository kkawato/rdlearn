sens <- function(x, ...) UseMethod("sens")

#' Title
#'
#' @param result
#' @param M
#' @param cost
#' @param xlab
#' @param ylab
#'
#' @return
#' @export
#'
#' @examples
#' result <- rdlearn(y = "acces", x = "saber11", c = "cutoff", groupname = "department", data = colombia_acces, fold = 20, M = c(0, 1), cost = 0)
#' plot.rdlearn(result)
#' sens.rdlearn(result, M = 1, cost = c(0, 0.2, 0.4, 0.6, 0.8, 1))
#' @export
sens.rdlearn <- function (
    result,
    M = NULL,
    cost = NULL,
    xlab="",
    ylab=""
){
  # check M and cost
  if(missing(M))
    stop("M is missing")

  if(missing(cost))
    stop("cost is missing")

  if (length(M) > 1 && length(cost) > 1) {
    stop("Both M and cost are vectors.")
  }

  safecut_new <- safelearn(
    c.vec = result$basecut,
    n = result$sample,
    q = result$numgroup,
    cost = cost,
    M = M,
    groupname = result$groupname,
    temp_result = result$temp_result
  )

  ########### create a new plot #############
  plot.rdlearn(result, safecut = safecut_new)
}
