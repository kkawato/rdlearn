#' Calculate distance measure between original cutoffs and safe cutoffs
#'
#' @param org_cut Numeric vector of original cutoffs.
#' @param safe_cut Data frame of safe cutoffs. The first column should be the group column, and subsequent columns the cutoffs.
#'
#' @return A numeric vector containing the L2 norms for each column of safe cutoffs.
#'
#' @importFrom dplyr %>%
#' @export
calculate_distance <- function(org_cut, safe_cut) {
  # Removing the group column
  safe_cut_value <- safe_cut %>% select(-group)

  # Function to calculate L2 norm
  calculate_l2norm <- function(safe_col, org_cut) {
    sqrt(sum((safe_col - org_cut)^2))
  }

  # Calculating L2 norms for each column
  l2norm_vector <- sapply(safe_cut_value, calculate_l2norm, org_cut = org_cut)

  return(l2norm_vector)
}
