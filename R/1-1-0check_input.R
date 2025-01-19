#' Check Input for rdlearn
#'
#' This function checks the input arguments for the \code{rdlearn} function.
#'
#' @param y A character string representing the name of the outcome variable.
#' @param x A character string representing the name of the running variable.
#' @param c A character string representing the name of the cutoff variable.
#' @param data A data frame containing all the required variables.
#' @param M A numeric value or vector representing the multipliers for
#'   sensitivity analysis.
#' @param cost A numeric value or vector representing the cost of treatment for
#'   calculating regret.
#' @param var_names A list containing the names of the outcome, running, and
#'   cutoff variables.
#' @param trace A logical value that controls whether to display the progress of
#'   cross-fitting and regret calculation.
#'
#' @return \value{None}
#' @keywords internal
#' @noRd
check_input <- function(
    y,
    x,
    c,
    data,
    M,
    cost,
    fold,
    var_names,
    trace) {
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

  # Check trace
  if (!is.logical(trace)) {
    stop("trace must be TRUE or FALSE.")
  }

  # Check fold
  if (fold < 2) {
    stop("fold must be an integer of 2 or greater.")
  }
}
