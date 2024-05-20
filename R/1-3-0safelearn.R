#' Implement Safe Policy Learning
#'
#' This function implements the safe policy learning algorithm for estimating
#' optimal cutoffs. It follows the procedures outlined in Sections 4.1 and 4.2
#' of the referenced source.
#'
#' @param c.vec A vector of cutoff values for the continuous variable X.
#' @param n The total sample size.
#' @param q The number of groups.
#' @param cost A vector of cost values to consider.
#' @param M A vector of multipliers for the Lipschitz constants.
#' @param group_name The name of the grouping variable.
#' @param cross_fit_result A list containing the results from the cross-fitting
#'   procedure.
#' @param trace A logical value that controls whether to display the progress of
#'   cross-fitting and regret calculation.
#' @return A data frame containing the estimated optimal cutoff values for each
#'   combination of M and cost. The columns represent different combinations of
#'   M and cost, and the rows correspond to the groups.
#'
#' @importFrom dplyr %>% filter
#' @importFrom purrr map
#' @keywords internal
#' @noRd
safelearn <- function(
    c.vec,
    n,
    q,
    cost,
    M,
    group_name,
    cross_fit_result,
    trace
) {
  ################################################################################
  # please refer to
  # Section 4.1. Doubly robust estimation
  # Section 4.2. Estimating the bounds
  ################################################################################

  dif_1 <- cross_fit_result$dif_1
  dif_0 <- cross_fit_result$dif_0
  Lip_1 <- cross_fit_result$Lip_1
  Lip_0 <- cross_fit_result$Lip_0
  data_all <- cross_fit_result$cross_fit_output

  Y <- data_all['Y']
  X <- data_all['X']
  C <- data_all['C']
  G <- data_all['G']

  safecut_all <- data.frame(group = group_name)
  Lip_1temp <- Lip_1
  Lip_0temp <- Lip_0

  for (temp_cost in cost) {
    for (temp_M in M) {
      if (trace == TRUE){
        print(paste("Calculation in progress for M =", temp_M, "and C =", temp_cost))
      }
      Lip_1 <- temp_M * Lip_1temp
      Lip_0 <- temp_M * Lip_0temp
      Lip_list <- list(Lip_1, Lip_0)
      c.all <- rep(0, length(c.vec))

      for(g in seq(1, q, 1)) {
        for (d in c(1, 0)) {
          eval_cond <- (data_all$G == g) & (data_all$X >= c.vec[1]) & (data_all$X < c.vec[q])

          if (d == 1) {
            eval_cond <- eval_cond & (data_all$X < c.vec[g])
          } else {
            eval_cond <- eval_cond & (data_all$X >= c.vec[g])
          }

          eval_dat <- data_all[eval_cond, ]$X
          IND <- sapply(eval_dat, function(x) sum(c.vec < x))
          temp_df <- cbind(eval_dat, IND)

          if (nrow(temp_df) > 0 && ncol(temp_df) > 0) {
            data_all[eval_cond, paste0("d", d)] <-
              apply(temp_df, 1, function(x) {
                sum(unlist(
                  sapply(x[2] + (1 - d), function(g.temp) {
                    lip_extra(
                      x.train = x[1],
                      treat = d,
                      g = g,
                      g.pr = g.temp,
                      Lip_1 = Lip_1,
                      Lip_0 = Lip_0,
                      dif_1 = dif_1,
                      dif_0 = dif_0,
                      G = G,
                      C = C
                    )
                  })[2, ]
                ))
              })
          }
        }
      }

      data_mid <- data_all %>% filter(X >= min(c.vec), X < max(c.vec))
      regret_sum <- NULL

      for (g in seq(1, q, 1)) {
        regret <- NULL
        for (c.alt in unique(X[X >= c.vec[1] & X < c.vec[q]])) {
          temp_reg <- calculate_regret(data_mid = data_mid,
                                       c.vec = c.vec,
                                       g = g,
                                       q = q,
                                       c.alt = c.alt,
                                       n = n,
                                       temp_cost = temp_cost)
          regret <- c(regret, temp_reg)
        }
        if (max(regret) == 0) { # if baseline policy is the best policy
          c.all[g] <- c.vec[g]
        } else {
          c.all[g] <- unique(X[X >= c.vec[1] & X < c.vec[q]])[which(regret == max(regret))[1]]
        }
        regret_sum <- c(regret_sum, max(regret))
      }
      c.all_df <- data.frame(c.all, group = group_name)
      names(c.all_df)[1] <- paste0("M=", temp_M, ",", "C=", temp_cost)
      safecut_all <- full_join(safecut_all, c.all_df, by = ("group" = "group"))
    }
  }
  safecut_all
}

