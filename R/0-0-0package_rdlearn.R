#' @title Safe Policy Learning for Regression Discontinuity Designs
#'
#' @description
#' The \code{rdlearn} package provides tools for safe policy learning under
#' regression discontinuity designs with multiple cutoffs.
#'
#' @section Package Functions:
#' The \code{rdlearn} package offers the following main functions:
#'
#' \bold{Policy Learning}
#' \itemize{
#'   \item \code{\link{rdlearn}}: Learn new treatment assignment cutoffs
#' }
#'
#' \bold{Visualization}
#' \itemize{
#'   \item \code{\link{plot}}: Visualize the learned cutoffs
#' }
#'
#' \bold{Sensitivity Analysis}
#' \itemize{
#'   \item \code{\link{sens}}: Perform sensitivity analysis
#' }
#'
#' \bold{RD Estimate}
#' \itemize{
#'  \item \code{\link{rdestimate}}: Estimate RD treatment effects
#' }
#'
#' \bold{Summary}
#' \itemize{
#' \item \code{\link{summary}}: Summarize the result of \code{\link{rdlearn}} and \code{\link{rdestimate}}
#' }
#'
#'
#' @references
#' Zhang, Y., Ben-Michael, E. and Imai, K. (2022) ‘Safe Policy Learning under Regression Discontinuity Designs with Multiple Cutoffs’, arXiv [stat.ME]. Available at: \url{http://arxiv.org/abs/2208.13323}.
#'
#' @examples
#' data(acces)
#'
#' # Learn new treatment assignment cutoffs
#' rdlearn_result <- rdlearn(
#'   y = "elig", x = "saber11", c = "cutoff",
#'   group_name = "department", data = acces,
#'   fold = 20, M = c(0, 1, 2, 4), cost = 0
#' )
#'
#' # Visualize the learned policies
#' plot(rdlearn_result, opt = "dif")
#'
#' # Implement sensitivity analysis
#' sens_result <- sens(rdlearn_result, M = 1, cost = c(0, 0.2, 0.4, 0.6, 0.8, 1))
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
