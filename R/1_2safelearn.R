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

  # ====================================================================== #
  # Section 4.2. Estimating the bounds
  # please refer to (16)

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
  # ====================================================================== #

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
      eval.dat1 <- c(data_all %>%
                       filter(G == g,
                              X >= c.vec[1],
                              X < c.vec[q],
                              X < c.vec[g]) %>%
                       select(X))$X # d(1)
      IND.1 <- sapply(eval.dat1, function(x) sum(c.vec < x)) #the number of treated
      temp_df1 <- cbind(eval.dat1, IND.1)

      # tryCatch({
      # results <- numeric(nrow(temp_df1))
      # for (i in seq_len(nrow(temp_df1))) {
      #   x <- temp_df1[i, ]
      #   lip_extra_results <- sapply(x[2], function(g.temp) {
      #     lip_extra(x.train = x[1], group = "dif1", g = g, g.pr = g.temp)[2, ]
      #   })
      #   results[i] <- sum(unlist(lip_extra_results))
      # }
      # }, error = function(e) return(0))

      tryCatch(
        {
          data_all[data_all$G == g
                   & data_all$X >= c.vec[1]
                   & data_all$X < c.vec[q]
                   & data_all$X < c.vec[g],
                   paste0("d", 1)] <- apply(temp_df1, 1, function(x) sum(unlist(sapply(x[2], function(g.temp) lip_extra(x.train = x[1], group = "dif1", g = g, g.pr = g.temp))[2, ])))
        }, error = function(e) return(0))

      # ---------------------------------------------------------------------- #
      # control group
      eval.dat0 <- c(data_all %>%
                       filter(G == g,
                              X >= c.vec[1],
                              X < c.vec[q],
                              X >= c.vec[g]) %>%
                       select(X))$X # d(0)
      IND.0 <- sapply(eval.dat0, function(x) sum(c.vec < x)) #the number of untreated
      temp_df0 <- cbind(eval.dat0, IND.0)

      # tryCatch({
      # results <- numeric(nrow(temp_df0))
      # for (i in seq_len(nrow(temp_df0))) {
      #   x <- temp_df0[i, ]
      #   lip_extra_results <- sapply(x[2] + 1, function(g.temp) {
      #     lip_extra(x.train = x[1], group = "dif0", g = g, g.pr = g.temp)[2, ]
      #   })
      #   results[i] <- sum(unlist(lip_extra_results))
      # }
      # }, error = function(e) return(0))

      tryCatch(
        {
          data_all[data_all$G == g
                   & data_all$X >= c.vec[1]
                   & data_all$X < c.vec[q]
                   & data_all$X >= c.vec[g],
                   paste0("d", 0)] <- apply(temp_df0, 1, function(x) sum(unlist(sapply(x[2] + 1, function(g.temp) lip_extra(x.train = x[1], group = "dif0", g = g, g.pr = g.temp))[2, ])))
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
        # -------------------------------------------------------------------- #
        if (c.alt >= c.vec[g]) {
          base_regret <- sum(data_mid[data_mid$G == g, "Y"]) / n

          # (12) I_iden: Identified
          Iden_alt <- (sum(data_mid[data_mid$X >= c.vec[g] & data_mid$X >= c.alt & data_mid$G == g, "Y"]) +
                    sum(data_mid[data_mid$X < c.vec[g] & data_mid$X < c.alt & data_mid$G == g, "Y"])) / n


          # (14) Theta_DR: first term and second term
          data_temp1 <- data_mid %>%
            filter(G == g,
                   X < c.alt,
                   X >= c.vec[g])

          DR_1 <- tryCatch(sum(data_temp1[, "mu.m"]) / n,
                              error = function(e) return(0))

          data_temp2 <- data_mid %>%
            filter(X < c.alt,
                   X >= c.vec[g],
                   X >= c.vec[ifelse(G == 1, 1, G - 1)], # X >= c.vec[G-1]
                   X < c.vec[G])

          DR_2 <- tryCatch(sum(with(data_temp2,
                                       eval(parse(text = paste0("pseudo.ps", g))) /
                                         eval(parse(text = paste0("pseudo.ps", G))) *
                                         (Y - eval(parse(text = "mu.aug"))))) / n,
                              error = function(e) return(0))

          # (15) Theta_2:
          Theta_2 <- tryCatch(sum(data_temp1[, paste0("d", 0)]) / n,
                            error = function(e) return(0))

          # cost
          cost <- tryCatch(temp_cost * dim(data_mid[data_mid$X >= c.vec[g]
                                                      & data_mid$X < c.alt
                                                      & data_mid$G == g, "Y"])[1] / n, # the number of
                               error = function(e) return(0))

          temp_reg <- (Iden_alt + DR_1 + DR_2 + Theta_2 + cost) - base_regret
        }

        # -------------------------------------------------------------------- #
        if (c.alt < c.vec[g]) {
          base_regret <- sum(data_mid[data_mid$G == g, "Y"]) / n

          # (12) I_iden: Identified
          Iden_alt <- (sum(data_mid[data_mid$X >= c.vec[g] & data_mid$X >= c.alt & data_mid$G == g, "Y"]) +
                         sum(data_mid[data_mid$X < c.vec[g] & data_mid$X < c.alt & data_mid$G == g, "Y"])) / n

          # (14) Theta_DR: first term and second term
          data_temp1 <- data_mid %>%
            filter(G == g,
                   X >= c.alt,
                   X < c.vec[g])

          DR_1 <- tryCatch(sum(data_temp1[, "mu.m"]) / n,
                              error = function(e) return(0))

          data_temp2 <- data_mid %>%
            filter(X >= c.alt,
                   X < c.vec[g],
                   X >= c.vec[G],
                   X < c.vec[ifelse(G == q, q, G + 1)]) # X < c.vec[G + 1]

          DR_2 <- tryCatch(sum(with(data_temp2,
                                       eval(parse(text = paste0("pseudo.ps", g))) /
                                         eval(parse(text = paste0("pseudo.ps", G))) *
                                         (Y - eval(parse(text = "mu.aug"))))) / n,
                              error = function(e) return(0))

          # (15) Theta_2
          Theta_2 <- tryCatch(sum(data_temp1[, paste0("d", 1)]) / n,
                            error = function(e) return(0))

          # cost
          cost <- tryCatch(temp_cost * dim(data_mid[data_mid$X < c.vec[g] & data_mid$X >= c.alt & data_mid$G == g, "Y"])[1] / n,
                               error = function(e) return(0))

          temp_reg <- (Iden_alt + DR_1 + DR_2 + Theta_2 - cost) - base_regret
        }
        # -------------------------------------------------------------------- #
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
