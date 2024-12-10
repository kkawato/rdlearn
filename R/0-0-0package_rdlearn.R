#' @title Safe Policy Learning for Regression Discontinuity Designs
#'
#' @description
#' The \code{rdlearn} package provides tools for safe policy learning under
#' regression discontinuity designs with multiple cutoffs.
#'
#' @section Package Functions:
#' The \code{rdlearn} package offers three main functions:
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
#' @references
#' Zhang, Yi, Eli Ben-Michael, and Kosuke Imai. 2023. "Safe Policy Learning
#' under Regression Discontinuity Designs with Multiple Cutoffs."
#' \url{http://arxiv.org/abs/2208.13323}.
#'
#' @examples
#' \dontrun{
#' # Load example data
#' data(acces)
#'
#' library(nprobust)
#' library(nnet)
#' library(ggplot2)
#' library(dplyr)
#' library(glue)
#' library(purrr)
#' library(tidyr)
#'
#' # Learn new treatment assignment cutoffs
#' result <- rdlearn(
#'   y = "elig", x = "saber11", c = "cutoff",
#'   group_name = "department", data = acces,
#'   fold = 20, M = c(0, 1), cost = 0
#' )
#'
#' # Visualize the learned policies
#' plot(result)
#'
#' # Perform sensitivity analysis
#' sens_result <- sens(result, M = 1, cost = c(0, 0.2, 0.4, 0.6, 0.8, 1))
#' plot(sens_result)
#' }
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom glue glue_collapse
## usethis namespace: end
NULL
