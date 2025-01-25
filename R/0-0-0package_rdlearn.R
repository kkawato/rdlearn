#' @title Safe Policy Learning for Regression Discontinuity Designs
#'
#' @description The \code{rdlearn} package provides tools for safe policy
#'   learning under regression discontinuity designs with multiple cutoffs.
#'
#' @section Package Functions: The \code{rdlearn} package offers the following
#'   main functions:
#'
#'   \bold{Policy Learning}
#' \itemize{
#'   \item \code{\link{rdlearn}}: Learn new treatment assignment cutoffs
#' }
#'
#'   \bold{Visualization}
#' \itemize{
#'   \item \code{\link{plot}}: Visualize the learned cutoffs
#' }
#'
#'   \bold{Sensitivity Analysis}
#' \itemize{
#'   \item \code{\link{sens}}: Perform sensitivity analysis
#' }
#'
#'   \bold{RD Estimate}
#' \itemize{
#'  \item \code{\link{rdestimate}}: Estimate RD treatment effects
#' }
#'
#'   \bold{Summary}
#' \itemize{
#' \item \code{\link{summary}}: Summarize the result of \code{\link{rdlearn}} and \code{\link{rdestimate}}
#' }
#'
#'   This package also contains the ACCES Program data \code{\link{acces}} for
#'   replication of Section 6 of Zhang et al. (2022). We thank Tatiana Velasco
#'   and her coauthors for sharing the dataset (Melguizo et al. (2016)).
#'
#' @references Zhang, Y., Ben-Michael, E. and Imai, K. (2022) 'Safe Policy
#'   Learning under Regression Discontinuity Designs with Multiple Cutoffs',
#'   arXiv [stat.ME]. Available at: \url{http://arxiv.org/abs/2208.13323}.
#'
#'   Melguizo, F., Sanchez, F., and Velasco, T. (2016) 'Credit for Low Income
#'   Students and Access to and Academic Performance in Higher Education in
#'   Colombia: A Regression Discontinuity Approach', World Development, 80(1):
#'   61-77.
#'
#' @examples
#' # Simulation Data A from Appendix D of Zhang et al. (2022)
#' set.seed(1)
#' n <- 300
#' X <- runif(n, -1000, -1)
#' G <- 2 * as.numeric(
#' I(0.01 * X + 5 + rnorm(n, sd = 10) > 0)
#' ) +
#' as.numeric(
#' I(0.01 * X + 5 + rnorm(n, sd = 10) <= 0)
#' )
#' c1 <- -850
#' c0 <- -571
#' C <- ifelse(G == 1, c1, c0)
#' D <- as.numeric(X >= C)
#' coef0 <- c(-1.992230e+00, -1.004582e-02, -1.203897e-05, -4.587072e-09)
#' coef1 <- c(9.584361e-01, 5.308251e-04, 1.103375e-06, 1.146033e-09)
#' Px <- poly(X - 735.4334 - c1, degree = 3, raw = TRUE) # -735.4334-c1 = 164.43
#' Px <- cbind(rep(1, nrow(Px)), Px)
#' EY0 <- Px %*% coef0
#' EY1 <- Px %*% coef1
#' d <- 0.2 + exp(0.01 * X) * (1 - G) + 0.3 * (1 - D)
#' Y <- EY0 * (1 - D) + EY1 * D - d * as.numeric(I(G == 1)) + rnorm(n, sd = 0.3)
#'
#' simdata_A_demo <- data.frame(Y,X,C)
#'
#' # Learn new treatment assignment cutoffs
#' rdlearn_result <- rdlearn(
#'   y = "Y", x = "X", c = "C", data = simdata_A_demo,
#'   fold = 2, M = 0, cost = 0
#' )
#'
#' # Summarise the learned policies
#' summary(rdlearn_result)
#'
#' # Visualize the learned policies
#' plot(rdlearn_result, opt = "dif")
#' # The learned cutoff for Group 1 is the same as the baseline cutoff, because
#' # the baseline cutoff is set to equal to oracle cutoff in this simulation.
#'
#' # Implement sensitivity analysis
#' sens_result <- sens(rdlearn_result, M = 1, cost = 0)
#' plot(sens_result, opt = "dif")
#' @name package_rdlearn
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom glue glue_collapse
## usethis namespace: end
NULL
