#' RD Estimate Function
#'
#' This function estimates local causal effect of treatment under standard
#' regression discontinuity (RD) setting.
#'
#' @param data A data frame containing all required variables.
#' @param y A character string specifying the name of column containing the
#'   outcome variable.
#' @param x A character string specifying the name of column containing the
#'   running variable.
#' @param c A character string specifying the name of column containing the
#'   cutoff variable.
#' @param group_name A character ctring specifying the name of the column
#'   containing group names (e.g., department names) for each cutoff. If not
#'   provided, the groups are assigned names "Group 1", "Group 2", ... in
#'   ascending order of cutoff values.
#'
#' @return A data frame with the RD estimates for each group, including the
#'   sample size of each group, baseline cutoff, RD estimate, standard error,
#'   and p-value.
#' @importFrom rdrobust rdrobust
#' @examples
#' rdestimate_result <- rdestimate(
#'   y = "elig", x = "saber11", c = "cutoff",
#'   group_name = "department", data = acces
#' )
#' print(rdestimate_result)
#' @export
rdestimate <- function(
    y,
    x,
    c,
    group_name = NULL,
    data) {
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

  var_names <- list(
    outcome = y,
    run_var = x,
    cutoff = c
  )

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

  results <- data.frame(
    Group = character(),
    Sample_size = integer(),
    Baseline_cutoff = numeric(),
    RD_Estimate = numeric(),
    se = numeric(),
    p_value = numeric(),
    stringsAsFactors = FALSE
  )

  Y <- data[[y]]
  X <- data[[x]]
  C <- data[[c]]
  c.vec <- sort(unique(C))
  G <- match(C, c.vec)
  q <- length(unique(C))

  if (is.null(group_name)) {
    group_names <- character(q)
    for (k in 1:q) {
      group_names[k] <- paste0("Group", k)
    }
  } else {
    grouplist <- data[[group_name]]
    dict <- setNames(grouplist, C)
    group_names <- sapply(c.vec, function(x) dict[[as.character(x)]])
  }

  data_all <- data.frame(Y = Y, X = X, C = C, G = G)

  for (g in 1:q) {
    subdata <- subset(data_all, G == g)
    n <- nrow(subdata)
    cutoff_value <- unique(subdata$C)
    y_value <- subdata$Y
    x_value <- subdata$X

    result <- rdrobust(y = y_value, x = x_value, c = cutoff_value)
    coef_conventional <- round(result$coef["Conventional", "Coeff"], 2)
    se_conventional <- round(result$se["Conventional", "Std. Err."], 2)
    pv_conventional <- round(result$pv["Conventional", "P>|z|"], 3)

    results <- rbind(results, data.frame(
      Group = group_names[g],
      Sample_size = n,
      Baseline_cutoff = cutoff_value,
      RD_Estimate = coef_conventional,
      se = se_conventional,
      p_value = pv_conventional
    ))
  }
  return(results)
}
