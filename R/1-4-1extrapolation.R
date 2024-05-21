#' Estimating the bounds of cross-group differences
#'
#' Please refer to the description of 1-4-0crossfit.R.
#'
#' @param x.train A training data.
#' @param treat Treatment indicator.
#' @param g Cutoff group indicator.
#' @param g.pr Another cutoff group indicator.
#' @param Lip_1 The cross-group smoothness parameter among treatment group.
#' @param Lip_0 The cross-group smoothness parameter among control group.
#' @param dif_1 The cross-group differences among treatment group.
#' @param dif_0 The cross-group differences among control group.
#' @param G Cutoff group indicator for each individual.
#' @param C Cutoff value for each individual.
#' @importFrom purrr map
#' @return A list containing the empirical upper bound and lower bound of
#' cross-group difference \code{dif}.
#' @keywords internal
#' @noRd
extrapolation <- function(x.train,
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

  upper <- purrr::map(x.train, function(x) min(1, min(dif + Lip * abs(x - eval.main))))
  lower <- purrr::map(x.train, function(x) max(-1, max(dif - Lip * abs(x - eval.main))))

  return(list(upper = upper, lower = lower))
}
