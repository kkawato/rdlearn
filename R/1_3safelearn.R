#' Implement estimation of \mu, group specific regression function, at each fold of cross fitting
#'
#' @param data_train A training data at each fold of cross fitting
#' @param data_test A test data at each fold of cross fitting
#' @param c.vec A vector containing cutoffs from lowest to highest
#' @param n A sample size
#' @param q The number of groups
#' @param g A group indicator
#' @importFrom nprobust lprobust
#' @importFrom dplyr %>% filter pull
#' @return A list containing \mu
#' @keywords internal
#' @noRd

safelearn = function(
    c.vec,
    n,
    q,
    cost,
    M,
    group_name,
    cross_fit_result)
{
  ################################################################################
  # please refer to
  # Section 4.1. Doubly robust estimation
  # Section 4.2. Estimating the bounds
  ################################################################################

  dif.1m <- cross_fit_result$dif_1
  dif.0m <- cross_fit_result$dif_0
  Lip_1 <- cross_fit_result$Lip_1
  Lip_0 <- cross_fit_result$Lip_0
  data_all <- cross_fit_result$cross_fit_output

  Y <- data_all['Y']
  X <- data_all['X']
  C <- data_all['C']
  G <- data_all['G']

  lip_extra <- function(x.train,
                        group,
                        g,
                        g.pr) {

    if (group == "dif1") { # B1 G=1
      d <- 1
      Lip <- Lip_1[g, g.pr]
      dif.m <- dif.1m[g, g.pr]
      eval.main <- unique(C[G == max(g, g.pr)])
    }

    if (group == "dif0") { # B1 G=1
      d <- 0
      Lip <- Lip_0[g, g.pr]
      dif.m <- dif.0m[g, g.pr]
      eval.main <- unique(C[G == min(g, g.pr)])
    }

    upper <- map(x.train, function(x) min(1, min(dif.m + Lip * abs(x - eval.main))))
    lower <- map(x.train, function(x) max(-1, max(dif.m - Lip * abs(x - eval.main))))
    return(list(upper = upper, lower = lower))
  }

  safecut_all <- data.frame(group = group_name)
  Lip_1temp <- Lip_1
  Lip_0temp <- Lip_0

  for (temp_cost in cost) {
    for (temp_M in M) {
      print(paste("Calculation in progress for M =", temp_M, "and C =", temp_cost))

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
                      group = paste0("dif", d),
                      g = g,
                      g.pr = g.temp
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
          DR_2 <- tryCatch(sum(with(data_temp2,
                                    eval(parse(text = paste0("pseudo.ps", g))) /
                                      eval(parse(text = paste0("pseudo.ps", G))) *
                                      (Y - eval(parse(text = "mu.aug"))))),
                           error = function(e) return(0))
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
          cost <- temp_cost * dim(data_mid[range1, ])[1]
          temp_reg <- ((Iden_alt + DR_1 + DR_2 + Theta_2 + cost * (c.alt >= c.vec[g]) - cost * (c.alt < c.vec[g])) / n) - (base_regret / n)
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



