#' Safe Policy Learning for Regression Discontinuity Design with Multiple
#' Cutoffs
#'
#' The \code{rdlearn} function implements safe policy learning under a
#' regression discontinuity design with multiple cutoffs. The resulting new
#' treatment assignment rules (cutoffs) are guaranteed to yield no worse overall
#' outcomes than the existing cutoffs.
#'
#' Regarding the detail of the algorithm, please refer to Zhang et al. (2022) "4 Empirical policy
#' learning" and "A.2 A double robust estimator for heterogeneous cross-group
#' differences".
#'
#' @param data A data frame containing all required variables.
#' @param y A character string specifying the name of column containing the outcome variable.
#' @param x A character string specifying the name of column containing the running variable.
#' @param c A character string specifying the name of column containing the cutoff variable.
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
#'     \item{distance}{A numeric vector containing the measures of difference between safe cutoffs and original cutoffs}
#'     \item{rdestimates}{A data frame containing the result of \code{rdesimate} such as causal effect estimates.}
#'     \item{temp_reg_df}{A data frame containing the regrets of every alternative cutoff.}
#'   }
#'
#' @importFrom stats setNames
#' @importFrom dplyr mutate arrange
#' @importFrom utils globalVariables
#'
#' @inherit package_rdlearn examples
#'
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
  call <- match.call()

  var_names <- list(
    outcome = y,
    run_var = x,
    cutoff = c
  )

  # --------------------------- Check input ---------------------------------- #
  check_input(
    y = y,
    x = x,
    c = c,
    data = data,
    M = M,
    cost = cost,
    fold = fold,
    var_names = var_names,
    trace = trace
  )

  # --------------------------- RDD esimation -------------------------------- #
  rdestimates <- rdestimate(
    y = y,
    x = x,
    c = c,
    group_name = group_name,
    data = data
  )

  # --------------------------- Prepare data --------------------------------- #
  # Prepare variables:
  # * Y: outcome variable
  # * X: running variable
  # * C: cutoff
  # Sort cutoffs from min to max
  # Group index, from min cutoff to max cutoff
  # Treatment indicator
  # Sample size
  # Number of groups

  Y <- data[[y]]
  X <- data[[x]]
  C <- data[[c]]
  c.vec <- sort(unique(C))
  G <- match(C, c.vec)
  D <- as.numeric(X >= C)
  n <- length(Y)
  q <- length(unique(C))

  # When group_name is not provided, assign a new name "Group k"
  if (is.null(group_name)) {
    group_name_vec <- character(q)
    for (k in 1:q) {
      group_name_vec[k] <- paste0("Group", k)
    }
  } else {
    group_name_df <- data[[group_name]]
    dict <- setNames(group_name_df, C)
    group_name_vec <- sapply(c.vec, function(x) dict[[as.character(x)]])
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
    group_name_vec = group_name_vec,
    dif_lip_output = dif_lip_output,
    cross_fit_output = cross_fit_output,
    trace = trace
  )

  # Calculate the distance between original cutoffs and safe cutoffs
  distance <- calculate_distance(
    org_cut = c.vec,
    safe_cut = safecut_all$safe_cut
  )

  # Organize output
  out <- list(
    call = call,
    var_names = var_names,
    org_cut = c.vec,
    safe_cut = safecut_all$safe_cut,
    dif_cut = safecut_all$dif_cut,
    sample = length(Y),
    num_group = q,
    group_name = group_name_vec,
    cross_fit_output = cross_fit_output,
    dif_lip_output = dif_lip_output,
    distance = distance,
    rdestimates = rdestimates,
    temp_reg_df = safecut_all$temp_reg_df
  )

  class(out) <- "rdlearn"
  return(out)
}

# Register global variables to avoid R CMD check notes
utils::globalVariables(c("fold_id", "D", "X", "G", "Y", "group", "type", "y_axis"))
