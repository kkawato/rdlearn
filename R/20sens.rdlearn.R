# sens <- function(x) {
#   UseMethod("sens", x)
# }

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

  safecut_new = safelearn(
    c.vec = result$basecut,
    Y = result$Y,
    X = result$X,
    C = result$C,
    G = result$G,
    D = result$D,
    n = result$sample,
    q = result$numgroup,
    cost = cost,
    M = M,
    groupname = result$groupname,
    Lip_1 = result$psout_ps$Lip_1_temp,
    Lip_0 = result$psout_ps$Lip_0_temp,
    B.1m = result$psout_ps$B.1m_temp,
    B.0m = result$psout_ps$B.0m_temp,
    data_all = result$psout_ps$data_all_temp
  )

  ########### create a new plot #############
  plot.rdlearn(result, safecut = safecut_new)
}
