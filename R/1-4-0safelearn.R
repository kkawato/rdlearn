#' Implement Safe Policy Learning
#'
#' @description
#' This function implements the safe policy learning algorithm for estimating
#' optimal cutoffs. There are the following two steps.
#'
#' First, we estimate the upper and lower bounds of \code{dif} by leveraging the
#' smoothness parameter \code{Lip} for constructing \code{Xi_2}. For more
#' detail, please refer to " 4.2 Estimating the bounds" of the referenced
#' source.
#'
#' Next, we calculate the regrets under baseline cutoffs and alternative
#' cutoffs. For alternative cutoffs, we estimate three components
#' \code{Iden_alt}, \code{Xi_1} and \code{Xi_2}. After calculating the
#' regrets of all plausible alternative cutoffs, we choose the safest cutoffs. For
#' more detail, please refer to "4.1 Doubly robust estimation" and "4.2
#' Estimating the bounds" of the referenced source.
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
#' @importFrom dplyr filter full_join
#' @keywords internal
#' @noRd
safelearn <- function(
    c.vec,
    n,
    q,
    cost,
    M,
    group_name_vec,
    dif_lip_output,
    cross_fit_output,
    trace) {
  temp_reg_df <- data.frame(M = numeric(), C = numeric(), group = integer(), c_alt = numeric(), temp_reg = numeric(), stringsAsFactors = FALSE)

  dif_1 <- dif_lip_output$dif_1
  dif_0 <- dif_lip_output$dif_0
  Lip_1 <- dif_lip_output$Lip_1
  Lip_0 <- dif_lip_output$Lip_0
  data_all <- cross_fit_output

  Y <- data_all["Y"]
  X <- data_all["X"]
  C <- data_all["C"]
  G <- data_all["G"]

  safecut_all <- data.frame(group = group_name_vec)
  Lip_1temp <- Lip_1
  Lip_0temp <- Lip_0

  for (temp_cost in cost) {
    for (temp_M in M) {
      if (isTRUE(trace)) {
        cat(paste("Calculation in progress for M =", temp_M, "and C =", temp_cost), "\n")
      }
      Lip_1 <- temp_M * Lip_1temp
      Lip_0 <- temp_M * Lip_0temp
      c.all <- rep(0, length(c.vec))

      for (g in seq(1, q, 1)) {
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
                temp_result <- extrapolation(
                  x.train = x[1],
                  treat = d,
                  g = g,
                  g.pr = x[2] + (1 - d),
                  Lip_1 = Lip_1,
                  Lip_0 = Lip_0,
                  dif_1 = dif_1,
                  dif_0 = dif_0,
                  G = G,
                  C = C
                )
                sum(temp_result)
              })
          }
        }
      }

      data_mid <- dplyr::filter(data_all, X >= min(c.vec), X < max(c.vec))
      regret_sum <- NULL

      for (g in seq(1, q, 1)) {
        regret <- NULL
        for (c.alt in unique(X[X >= c.vec[1] & X < c.vec[q]])) {
          temp_reg <- calculate_regret(
            data_mid = data_mid,
            c.vec = c.vec,
            g = g,
            q = q,
            c.alt = c.alt,
            n = n,
            temp_cost = temp_cost
          )
          temp_reg_df <- rbind(temp_reg_df, data.frame(M = temp_M, C = temp_cost, group = g, c_alt = c.alt, temp_reg = temp_reg))
          regret <- c(regret, temp_reg)
        }
        if (max(regret) == 0) {
          c.all[g] <- c.vec[g]
        } else {
          c.all[g] <- unique(X[X >= c.vec[1] & X < c.vec[q]])[which(regret == max(regret))[1]]
        }
        regret_sum <- c(regret_sum, max(regret))
      }
      c.all_df <- data.frame(c.all, group = group_name_vec)
      names(c.all_df)[1] <- paste0("M=", temp_M, ",", "C=", temp_cost)
      safecut_all <- dplyr::full_join(safecut_all, c.all_df, by = "group")
    }
  }

  rownames(safecut_all) <- safecut_all$group
  safecut_all <- safecut_all[, -which(names(safecut_all) == "group")]

  if (length(M) == 1 && length(cost) == 1) {
    dif_cut <- safecut_all - c.vec
    dif_cut <- as.data.frame(dif_cut)
    colnames(dif_cut) <- paste0("M=", M, ",", "C=", cost)
    rownames(dif_cut) <- group_name_vec
    safecut_all <- as.data.frame(safecut_all)
    colnames(safecut_all) <- paste0("M=", M, ",", "C=", cost)
    rownames(safecut_all) <- group_name_vec
  } else {
    dif_cut <- safecut_all - matrix(c.vec)
  }

  out <- list(safe_cut = safecut_all, dif_cut = dif_cut, temp_reg_df = temp_reg_df)
  return(out)
}

#' Estimating the bounds of cross-group differences
#'
#' Please refer to the description of 1-4-0crossfit.R.
#'
#' @param x.train A training data.
#' @param treat Treatment indicator.
#' @param g Cutoff group indicator.
#' @param g.pr Another cutoff group indicator.
#' @param Lip_1 The cross-group smoothness parameter among treatment group.
#' @param Lip_0 The cross-group smoothness parameter among control group.
#' @param dif_1 The cross-group differences among treatment group.
#' @param dif_0 The cross-group differences among control group.
#' @param G Cutoff group indicator for each individual.
#' @param C Cutoff value for each individual.
#' @return A list containing the empirical upper bound and lower bound of
#' cross-group difference \code{dif}.
#' @keywords internal
#' @noRd
extrapolation <- function(x.train,
                          treat,
                          g,
                          g.pr,
                          Lip_1,
                          Lip_0,
                          dif_1,
                          dif_0,
                          G = G,
                          C = C) {
  if (treat == 1) {
    Lip <- Lip_1[g, g.pr]
    dif <- dif_1[g, g.pr]
    eval.main <- unique(C[G == max(g, g.pr)])
  }

  if (treat == 0) {
    Lip <- Lip_0[g, g.pr]
    dif <- dif_0[g, g.pr]
    eval.main <- unique(C[G == min(g, g.pr)])
  }
  suppressWarnings({
    lower <- sapply(x.train, function(x) max(-1, max(dif - Lip * abs(x - eval.main))))
  })
  return(lower)
}


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

  data_temp1 <- data_mid[range1, ]

  if (nrow(data_temp1) == 0) {
    DR_1 <- 0
  } else {
    # DR_1 <- sum(ifelse(is.na(data_temp1[, "mu.m"]), 0, data_temp1[, "mu.m"]))
    DR_1 <- sum(ifelse(is.na(data_temp1[["mu.m"]]), 0, data_temp1[["mu.m"]]))
  }

  Xi_2 <- sum(data_temp1[, paste0("d", d)])

  data_temp2 <- data_mid[range2, ]

  DR_2_temp <- tryCatch(
    {
      sum(with(
        data_temp2,
        eval(parse(text = paste0("pseudo.ps", g))) /
          eval(parse(text = paste0("pseudo.ps", G))) *
          (Y - eval(parse(text = "mu.aug")))
      ))
    },
    error = function(e) {
      DR_2_temp <- 0
    }
  )

  DR_2 <- if (is.na(DR_2_temp)) {
    0
  } else {
    DR_2_temp
  }

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
