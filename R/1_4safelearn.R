safelearn = function(
    c.vec,
    n,
    q,
    cost,
    M,
    groupname,
    temp_result)
{
  ################################################################################
  # please refer to
  # Section 4.1. Doubly robust estimation
  # Section 4.2. Estimating the bounds
  ################################################################################

  dif.1m <- temp_result$dif.1m_temp
  dif.0m <- temp_result$dif.0m_temp
  Lip_1 <- temp_result$Lip_1_temp
  Lip_0 <- temp_result$Lip_0_temp
  data_all <- temp_result$data_all_temp

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

  safecut_all <- data.frame(group = groupname)
  Lip_1temp <- Lip_1
  Lip_0temp <- Lip_0

  for (temp_cost in cost) {
    for (temp_M in M) {

      print(paste("Calculating the case of M =", temp_M, "C =", temp_cost))

      Lip_1 <- temp_M * Lip_1temp
      Lip_0 <- temp_M * Lip_0temp
      c.all <- rep(0, length(c.vec))

      for(g in seq(1, q, 1)) {
        # ======================================================================== #
        # Section 4.2. Estimating the bounds
        # please refer to (15)

        # ---------------------------------------------------------------------- #
        # treatment group
        eval1 <- (data_all$G == g) & (data_all$X >= c.vec[1]) & (data_all$X < c.vec[q]) & (data_all$X < c.vec[g])
        eval.dat1 <- data_all[eval1, ]$X
        IND.1 <- sapply(eval.dat1, function(x) sum(c.vec < x)) #the number of treated
        temp_df1 <- cbind(eval.dat1, IND.1)
        data_all[eval1, paste0("d", 1)] <- apply(temp_df1, 1, function(x) sum(unlist(sapply(x[2], function(g.temp) lip_extra(x.train = x[1], group = "dif1", g = g, g.pr = g.temp))[2, ])))

        # ---------------------------------------------------------------------- #
        # control group
        eval0 <- (data_all$G == g) & (data_all$X >= c.vec[1]) & (data_all$X < c.vec[q]) & (data_all$X >= c.vec[g])
        eval.dat0 <- data_all[eval0, ]$X
        IND.0 <- sapply(eval.dat0, function(x) sum(c.vec < x)) #the number of untreated
        temp_df0 <- cbind(eval.dat0, IND.0)

        tryCatch({
        data_all[eval0, paste0("d", 0)] <- apply(temp_df0, 1, function(x) sum(unlist(sapply(x[2] + 1, function(g.temp) lip_extra(x.train = x[1], group = "dif0", g = g, g.pr = g.temp))[2, ])))
        }, error = function(e) return(0))
      }

      # ======================================================================== #
      # Section 4.1. Doubly Robust Estimation
      # Section 4.2. Estimating the bounds
      # please refer to (12), (14), (15), (17)
      # ======================================================================== #

      data_mid <- data_all %>% filter(X >= min(c.vec), X < max(c.vec))
      regret_sum <- NULL

      for (g in seq(1, q, 1)) {
        regret <- NULL
        for (c.alt in unique(X[X >= c.vec[1] & X < c.vec[q]])) {
          if (c.alt >= c.vec[g]) {
            d <- 0
            range1 <- (data_mid$X >= c.vec[g]) & (data_mid$X < c.alt) & (data_mid$G == g)
            range2 <- (data_mid$X < c.alt) & (data_mid$X >= c.vec[g]) & (data_mid$X >= c.vec[ifelse(data_mid$G == 1, 1, data_mid$G - 1)]) & (data_mid$X < c.vec[data_mid$G])
            cost <- temp_cost * dim(data_mid[range1, "Y"])[1] / n
          } else {
            d <- 1
            range1 <- (data_mid$X < c.vec[g]) & (data_mid$X >= c.alt) & (data_mid$G == g)
            range2 <- (data_mid$X >= c.alt) & (data_mid$X < c.vec[g]) & (data_mid$X >= c.vec[data_mid$G]) & (data_mid$X < c.vec[ifelse(data_mid$G == q, q, data_mid$G + 1)])
            cost <- -1 * temp_cost * dim(data_mid[range1, "Y"])[1] / n
          }

          data_temp1 <- data_mid[range1,]
          base_regret <- sum(data_mid[data_mid$G == g, "Y"]) / n
          Iden_alt <- (sum(data_mid[data_mid$X >= c.vec[g] & data_mid$X >= c.alt & data_mid$G == g, "Y"]) +
                         sum(data_mid[data_mid$X < c.vec[g] & data_mid$X < c.alt & data_mid$G == g, "Y"])) / n
          DR_1 <- sum(data_temp1[, "mu.m"]) / n
          Theta_2 <- sum(data_temp1[, paste0("d", d)]) / n

          data_temp2 <- data_mid[range2, ]

          if (nrow(data_temp2) != 0) {
            DR_2 <- 0
            for (i in seq(1, nrow(data_temp2), 1)) {
              data_i <- data_temp2[i,]
              DR_2 <- DR_2 + (data_i[[paste0("pseudo.ps", g)]] / data_i[[paste0("pseudo.ps", data_i$G)]]) * (data_i$Y - data_i$mu.aug)
            }
            DR_2 <- DR_2 / n
          }

          # DR_2 <- tryCatch(sum(with(data_temp2,
          #                           eval(parse(text = paste0("pseudo.ps", g))) /
          #                             eval(parse(text = paste0("pseudo.ps", G))) *
          #                             (Y - eval(parse(text = "mu.aug"))))) / n,
          #                  error = function(e) return(0))

          temp_reg <- (Iden_alt + DR_1 + DR_2 + Theta_2 + cost) - base_regret
          regret <- c(regret, temp_reg)
        }
        if (max(regret) == 0) { # if baseline policy is the best policy
          c.all[g] <- c.vec[g]
        } else {
          c.all[g] <- unique(X[X >= c.vec[1] & X < c.vec[q]])[which(regret == max(regret))[1]]
        }
        regret_sum <- c(regret_sum, max(regret))
      }
      c.all_df <- data.frame(c.all, group = groupname)
      names(c.all_df)[1] <- paste0("M=", temp_M, ",", "C=", temp_cost)
      safecut_all <- full_join(safecut_all, c.all_df, by = ("group" = "group"))
    }
  }
  safecut_all
}



