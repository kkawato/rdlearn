#' Safe Policy Learning for Regression Discontinuity Design with Multiple
#' Cutoffs
#'
#' The \code{rdlearn} function implements safe policy learning under a
#' regression discontinuity design with multiple cutoffs. The resulting new
#' treatment assignment rules (cutoffs) are guaranteed to yield no worse overall
#' outcomes than the existing cutoffs.
#'
#' @param data A data frame containing all required variables.
#' @param y A character string specifying the name of the outcome variable
#'   column.
#' @param x A character string specifying the name of the running variable
#'   column.
#' @param c A character string specifying the name of the cutoff variable
#'   column.
#' @param group_name A character string specifying the name of the column
#'   containing group names (e.g., department names) for each cutoff. If not
#'   provided, the groups are assigned names "Group 1", "Group 2", ... in
#'   ascending order of cutoff values.
#' @param fold The number of folds for cross-fitting. Default is 10.
#' @param M A numeric value or vector specifying the multiplicative smoothness
#'   factor(s) for sensitivity analysis. Default is 1.
#' @param cost A numeric value or vector specifying the cost of treatment for
#'   calculating regret. This cost should be scaled by the range of the outcome
#'   variable Y. Default is 0.
#' @param trace A logical value that controls whether to display the progress.
#'   If set to TRUE, the progress will be printed. The default value is TRUE.
#' @return An object of class \code{rdlearn}, which is a list containing the
#'   following components:
#'   \describe{
#'     \item{call}{The original function call.}
#'     \item{var_names}{A list of variable names for the outcome, running variable, and cutoff.}
#'     \item{org_cut}{A vector of original cutoff values.}
#'     \item{safe_cut}{A data frame containing the obtained new treatment assignment cutoffs.}
#'     \item{sample}{The total sample size.}
#'     \item{num_group}{The number of groups.}
#'     \item{group_name}{A vector of group names.}
#'     \item{cross_fit_output}{The intermediate output of the cross-fitting procedure.}
#'     \item{dif_lip_output}{The intermediate output of the cross-group differences and the smoothness parameters}
#'   }
#'
#' @examples
#' \dontrun{
#' # Load example data
#' data(acces)
#' library(rdlearn)
#' library(nprobust)
#' library(nnet)
#' library(ggplot2)
#' library(dplyr)
#' library(glue)
#' library(purrr)
#' library(tidyr)
#'
#' result <- rdlearn(y = "elig", x = "saber11", c = "cutoff",
#'                   group_name = "department", data = acces,
#'                   fold = 20, M = c(0, 1), cost = 0)
#' plot(result)
#' }
#'
#' @importFrom stats setNames
#' @importFrom dplyr mutate arrange
#' @importFrom utils globalVariables
#' @export
rdlearn <- function(
    y,
    x,
    c,
    group_name = NULL,
    data,
    fold = 10,
    M = 1,
    cost = 0,
    trace = TRUE) {
  # Get function call
  cl <- match.call()
  var_names <- list(outcome = y,
                    run_var = x,
                    cutoff = c)
  # --------------------------- Check input ---------------------------------- #
  check_input(y = y,
              x = x,
              c = c,
              data = data,
              M = M,
              cost = cost,
              var_names = var_names,
              trace = trace)

  # --------------------------- Prepare data --------------------------------- #

  # Prepare variables:
  # * Y: outcome variable
  # * X: running variable
  # * C: cutoff
  Y <- data[[y]] ; X <- data[[x]] ; C <- data[[c]]

  # Sort cutoffs from min to max
  # Group index, from min cutoff to max cutoff
  # Treatment indicator
  c.vec <- sort(unique(C)) ; G <- match(C, c.vec) ; D <- as.numeric(X >= C)

  # Sample size
  # Number of groups
  n <- length(Y) ; q <- length(unique(C))

  # When group_name is not provided, assign a new name "Group k"
  if (is.null(group_name)) {
    group_name <- character(q)
    for (k in 1:q) {
      group_name[k] <- paste0("Group", k)
    }
  } else {
    grouplist <- data[[group_name]]
    dict <- setNames(grouplist, C)
    group_name <- sapply(c.vec, function(x) dict[[as.character(x)]])
  }

  # Add fold_id to data used for cross-fitting
  data_all <- data.frame(Y = Y, X = X, C = C, D = D, G = G) %>%
    dplyr::mutate(fold_id = sample(1:fold, size = n, replace = TRUE)) %>%
    arrange(fold_id)

  # ------------------------- Apply Algorithms ------------------------------- #
  # Apply cross fitting
  cross_fit_output <- crossfit(
    c.vec = c.vec,
    q = q,
    fold = fold,
    data_all = data_all,
    trace = trace
  )

  # Apply differences and Lipschitz estimation
  dif_lip_output <- estimate_dif_lip(
    cross_fit_output = cross_fit_output,
    q = q,
    c.vec = c.vec,
    trace = trace
  )

  # Apply safe learning
  safecut_all <- safelearn(
    c.vec = c.vec,
    n = n,
    q = q,
    cost = cost,
    M = M,
    group_name = group_name,
    dif_lip_output = dif_lip_output,
    cross_fit_output = cross_fit_output,
    trace = trace
  )

  # Organize output
  out <- list(
    call = cl,
    var_names = var_names,
    org_cut = c.vec,
    safe_cut = safecut_all,
    sample = n,
    num_group = q,
    group_name = group_name,
    cross_fit_output = cross_fit_output,
    dif_lip_output = dif_lip_output
  )

  class(out) <- "rdlearn"
  return(out)
}

# Register global variables to avoid R CMD check notes
utils::globalVariables(c('fold_id', 'D', 'X', 'G', 'Y', 'group', 'type', 'y_axis'))
