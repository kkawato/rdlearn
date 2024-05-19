#' Calculating regret
#'
#' @param data_mid data whose runnig variable is under the maximam cutoff and over the minimum cutoff
#' @param c.vec A vector of cutoff values for the continuous variable X.
#' @param g group indicator
#' @param c.alt alternative cutoff
#' @param n The total sample size.
#' @param temp_cost the cost of treatment.
#'
#' @return the regret calculated with alternative cutoff.
#'
#' @keywords internal
#' @noRd
calculate_regret <- function(data_mid,
                             c.vec,
                             g,
                             q,
                             c.alt,
                             n,
                             temp_cost) {
  if (c.alt >= c.vec[g]) {
    d <- 0
    range1 <- (data_mid$X >= c.vec[g]) & (data_mid$X < c.alt) & (data_mid$G == g)
    range2 <- (data_mid$X < c.alt) & (data_mid$X >= c.vec[g]) & (data_mid$X >= c.vec[ifelse(data_mid$G == 1, 1, data_mid$G - 1)]) & (data_mid$X < c.vec[data_mid$G])
  } else {
    d <- 1
    range1 <- (data_mid$X < c.vec[g]) & (data_mid$X >= c.alt) & (data_mid$G == g)
    range2 <- (data_mid$X >= c.alt) & (data_mid$X < c.vec[g]) & (data_mid$X >= c.vec[data_mid$G]) & (data_mid$X < c.vec[ifelse(data_mid$G == q, q, data_mid$G + 1)])
  }

  base_regret <- sum(data_mid[data_mid$G == g, "Y"])
  Iden_alt <- (sum(data_mid[data_mid$X >= c.vec[g] & data_mid$X >= c.alt & data_mid$G == g, "Y"]) +
                 sum(data_mid[data_mid$X < c.vec[g] & data_mid$X < c.alt & data_mid$G == g, "Y"]))

  data_temp1 <- data_mid[range1,]
  DR_1 <- sum(data_temp1[, "mu.m"])
  Theta_2 <- sum(data_temp1[, paste0("d", d)])

  data_temp2 <- data_mid[range2, ]
  DR_2 <- 0
  if (nrow(data_temp2) > 0 && ncol(data_temp2) > 0) {
    DR_2 <- sum(with(data_temp2,
                     eval(parse(text = paste0("pseudo.ps", g))) /
                       eval(parse(text = paste0("pseudo.ps", G))) *
                       (Y - eval(parse(text = "mu.aug"))))) / n
  }

  cost <- temp_cost * dim(data_mid[range1, ])[1]
  temp_reg <- ((Iden_alt + DR_1 + DR_2 + Theta_2 + cost * (c.alt >= c.vec[g]) - cost * (c.alt < c.vec[g])) / n) - (base_regret / n)

  return(temp_reg)
}
