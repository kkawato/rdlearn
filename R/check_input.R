#' Input check for \code{rdlearn}.
#'
#' @keywords internal
#' @noRd
check_input <- function(
    y,
    x,
    c,
    data,
    M,
    cost,
    var_names){
  # Check argument missingness and type
  if (missing(y) || !is.character(y) || length(y) > 1) {
    stop("'y' must be a character string of length one.")
  }
  if (missing(x) || !is.character(x) || length(x) > 1) {
    stop("'x' must be a character string of length one.")
  }
  if (missing(c) || !is.character(c) || length(c) > 1) {
    stop("'c' must be a character string of length one.")
  }

  # Check if all variables are in 'data'
  if (!all(var_names %in% names(data))) {
    stop("all variables must be in 'data'.")
  }

  # Check NA
  if (anyNA(data[[y]])) {
    stop("the column 'y' contains NA.")
  }
  if (anyNA(data[[x]])) {
    stop("the column 'x' contains NA.")
  }
  if (anyNA(data[[c]])) {
    stop("the column 'c' contains NA.")
  }

  # Check M and cost
  if (length(M) > 1 && length(cost) > 1) {
    stop("Either M or cost must be a scalar.")
  }


}
