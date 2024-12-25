#' Calculate distance measure between original cutoffs and safe cutoffs
#'
#' @param org_cut Numeric vector of original cutoffs.
#' @param safe_cut Data frame of safe cutoffs. The first column should be the
#'   group column, and subsequent columns the cutoffs.
#' @return A numeric vector containing the distance between safe cutoffs and
#'   original cutoffs, measured in terms of the L1 norm, L2 norm, and uniform
#'   norm.
#' @keywords internal
#' @noRd
calculate_distance <- function(org_cut, safe_cut) {
  calculate_l1 <- function(safe_col, org_cut) {
    mean(abs(safe_col - org_cut))
  }
  calculate_l2 <- function(safe_col, org_cut) {
    sqrt(mean((safe_col - org_cut)^2))
  }
  calculate_max <- function(safe_col, org_cut) {
    max(abs(safe_col - org_cut))
  }

  l1_vector <- sapply(safe_cut, calculate_l1, org_cut = org_cut)
  l2_vector <- sapply(safe_cut, calculate_l2, org_cut = org_cut)
  max_vector <- sapply(safe_cut, calculate_max, org_cut = org_cut)

  out <- data.frame(
    l1 = l1_vector,
    l2 = l2_vector,
    max = max_vector
  )

  out_t <- t(out)
  return(out_t)
}
