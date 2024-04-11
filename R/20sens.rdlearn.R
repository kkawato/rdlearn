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
    Lip_1 = result$temp_result$Lip_1_temp,
    Lip_0 = result$temp_result$Lip_0_temp,
    dif.1m = result$temp_result$dif.1m_temp,
    dif.0m = result$temp_result$dif.0m_temp,
    data_all = result$temp_result$data_all_temp
  )

  ########### create a new plot #############
  plot.rdlearn(result, safecut = safecut_new)
}
