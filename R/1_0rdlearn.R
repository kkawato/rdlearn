#' Safe policy learning under RD design with multiple cutoffs
#'
#' \code{rdlearn} implements policy learning under the RD design with multiple
#' cutoffs. The resulting new treatment cutoffs have a safety guarantee that
#' they will not yield a worse overall outcome than the existing cutoffs.
#'
#' @param data A dataframe containing all following variables.
#' @param y A column name of outcome variable.
#' @param x A column name of running variable.
#' @param c A column name of cutoff.
#' @param groupname A column name of each cutoff group's name (e.g. department
#'   name). If no argument is entered, the names "Group 1", "Group 2", ... are
#'   assigned from the group with smallest cutoff.
#' @param fold The number of folds for cross-fitting. Default is 10.
#' @param M A multiplicative smoothness factor for sensitivity analysis. Default
#'   is 1.
#' @param cost A cost of a treatment for calculating regret. This cost has to be
#'   scaled by the range of the outcome Y. Default is 0.
#'
#' @importFrom dplyr %>% filter ungroup select arrange
#' @importFrom tidyr unnest
#'
#' @return \code{rdlearn} returns an object of \code{rdlearn} class, which is a
#'   list of following items:
#' \describe{
#'   \item{safecut}{A table of obtained new treatment cutoffs in the dataframe format}
#' }
#'
#' @examples
#' result <- rdlearn(y = "elig", x = "saber11", c = "cutoff", groupname = "department", data = acces, fold = 20, M = c(0, 1), cost = 0)
#' plot(result)
#'
#' @export
rdlearn <- function(
    y,
    x,
    c,
    groupname = NULL,
    data,
    fold = 10,
    M = 1,
    cost = 0) {
  # Get function call
  cl <- match.call()

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

  var_names <- list(y, x, c)

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
    stop("M and cost should be a scalar.")
  }

  # --------------------------- Prepare data -------------------------------- #

  # Prepare variables:
  # * Y: outcome variable
  # * X: running variable
  # * C: cutoff
  Y <- data[[y]]
  X <- data[[x]]
  C <- data[[c]]

  # Sort cutoffs from min to max
  c.vec <- sort(unique(C))
  # Sample size
  n <- length(Y)
  # Number of groups
  q <- length(unique(C))
  # Group index, from min cutoff to max cutoff
  G <- match(C, c.vec)
  # Treatment indicator
  D <- as.numeric(X >= C)

  # When groupname is not provided, assign a new name "Group k"
  if (is.null(groupname)) {
    groupname <- character(q)
    for (k in 1:q) {
      groupname[k] <- paste0("Group", k)
    }
  } else {
    grouplist <- data[[groupname]]
    dict <- setNames(grouplist, C)
    groupname <- sapply(c.vec, function(x) dict[[as.character(x)]])
  }

  # Add fold_id to data used for cross-fitting
  tempdata <- data.frame(Y = Y, X = X, C = C, D = D, G = G)

  data_all <- tempdata %>%
    mutate(fold_id = sample(1:fold, size = n, replace = TRUE)) %>%
    arrange(fold_id)

  data_split <- data_all %>%
    group_by(fold_id) %>%
    nest() %>%
    arrange(fold_id)

  # ------------------------- Apply Algorithms ------------------------------- #
  # Apply cross fitting
  cross_fit_output <- crossfit(
    c.vec = c.vec,
    q = q,
    fold = fold,
    data_split = data_split,
    data_all = data_all
  )

  # Apply safe learning
  safecut_all <- safelearn(
    c.vec = c.vec,
    n = n,
    q = q,
    cost = cost,
    M = M,
    groupname = groupname,
    temp_result = cross_fit_output
  )

  # Organize output
  out <- list(
    call = cl,
    variables = var_names,
    orgcut = c.vec,
    sample = n,
    numgroup = q,
    M = M,
    cost = cost,
    groupname = groupname,
    safecut = safecut_all,
    data_all = data_all,
    temp_result = cross_fit_output
  )

  class(out) <- "rdlearn"
  return(out)
}
