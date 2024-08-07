#' Calculating regret
#'
#' Please refer to the description of 1-2-0crossfit.R.
#'
#' @param data_mid A data whose running variable is under the maximum cutoff and over the minimum cutoff.
#' @param c.vec A vector of cutoff values for the continuous variable X.
#' @param g A group indicator.
#' @param c.alt An alternative cutoff.
#' @param n The total sample size.
#' @param temp_cost the cost of treatment.
#'
#' @return The regret calculated with an alternative cutoff.
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
  DR_1 <- sum(ifelse(is.na(data_temp1[, "mu.m"]), 0, data_temp1[, "mu.m"]))
  Xi_2 <- sum(data_temp1[, paste0("d", d)])

  data_temp2 <- data_mid[range2, ]

  DR_2_temp <- tryCatch({
    sum(with(data_temp2,
             eval(parse(text = paste0("pseudo.ps", g))) /
               eval(parse(text = paste0("pseudo.ps", G))) *
               (Y - eval(parse(text = "mu.aug")))))
  }, error = function(e) {
    DR_2_temp <- 0
  })

  DR_2 <- if (is.na(DR_2_temp)) { 0 } else { DR_2_temp }

  # ------------------------------------------------------------------ #
  # trycatch to avoid the following error
  # Error in eval(parse(text = paste0("pseudo.ps", G))) :  object 'pseudo.ps' not found
  # This occurs in case nrow(data_temp2) == 0, and maybe other cases...?
  #
  # The following code does not produce exactly the same result
  #
  # if (nrow(data_temp2) != 0) {
  #   DR_2 <- sum(with(data_temp2,
  #                    eval(parse(text = paste0("pseudo.ps", g))) /
  #                      eval(parse(text = paste0("pseudo.ps", G))) *
  #                      (Y - eval(parse(text = "mu.aug"))))) / n
  # }
  # ------------------------------------------------------------------ #

  Xi_1 <- DR_1 + DR_2
  cost <- temp_cost * dim(data_mid[range1, ])[1]

  temp_reg <- ((Iden_alt + Xi_1 + Xi_2 + cost * (c.alt >= c.vec[g]) - cost * (c.alt < c.vec[g])) / n) - (base_regret / n)
  return(temp_reg)
}
