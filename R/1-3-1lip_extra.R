#' Estimating the bounds
#'
#' Please refer to Section 4.2. (16) of the referenced source
#'
#' @param x.train training data
#' @param treat treatment indicator
#' @param g cutoff group indicater
#' @param g.pr cutoff group indicater
#' @param Lip_1 cross-group smoothness parameter among treatment group
#' @param Lip_0 cross-group smoothenes parameter among control group
#' @param dif_1 cross-group differences among treatment group
#' @param dif_0 cross-group differences among control group
#' @param G group indicator for each individual
#' @param C cutoff value for each individual
#'
#' @return A list containing the empirical upper bound and lower bound of
#'   cross-group difference
#' @keywords internal
#' @noRd
lip_extra <- function(x.train,
                      treat,
                      g,
                      g.pr,
                      Lip_1,
                      Lip_0,
                      dif_1,
                      dif_0,
                      G,
                      C) {
  if (treat == 1) {
    Lip <- Lip_1[g, g.pr]
    dif <- dif_1[g, g.pr]
    eval.main <- unique(C[G == max(g, g.pr)])
  }

  if (treat == 0) {
    Lip <- Lip_0[g, g.pr]
    dif <- dif_0[g, g.pr]
    eval.main <- unique(C[G == min(g, g.pr)])
  }

  upper <- map(x.train, function(x) min(1, min(dif + Lip * abs(x - eval.main))))
  lower <- map(x.train, function(x) max(-1, max(dif - Lip * abs(x - eval.main))))
  return(list(upper = upper, lower = lower))
}
