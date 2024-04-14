# sens <- function(x) {
#   UseMethod("sens", x)
# }

sens <- function(x, ...) UseMethod("sens")

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
