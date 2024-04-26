#' Implement cross-fitting for estimating cross-group differences
#'
#' @importFrom dplyr %>% filter ungroup select arrange
#' @importFrom tidyr unnest
#' @importFrom nnet multinom
#' @importFrom nprobust lprobust
#' @noRd
crossfit <- function(
  c.vec,
  q,
  fold,
  data_split,
  data_all)
{
  ################################################################################
  # please refer to
  # Appendix A.2. A double robust estimator for heterogeneous cross-group differences
  # Section 4.1. Doubly robust estimation
  # Section 4.3. Choosing the smoothness parameter
  ################################################################################

  cross_fit_output <- data.frame()

  for (k in 1:fold) {
    data_train <- data_all %>% filter(fold_id != k)
    data_test <- data_all %>% filter(fold_id  == k)

    # conditional prob of group
    gamfit <- multinom(formula = G ~ X, data = data_train)
    ps <- predict(gamfit, newdata = data_test, "probs")
    data_test[, paste0("pseudo.ps", seq(1, q, 1))] <- predict(gamfit, newdata = data_test, "probs")

    for (g in seq(1,q,1)){

      mu_all <- estimate_mu(data_train, data_test, c.vec, k, g, q)

      data_test[data_test$D == 1 & data_test$X >= c.vec[g], paste0("pseudo.", g)] <- mu_all$pseudo1
      if (nrow(data_test[data_test$X >= c.vec[g] & data_test$X < c.vec[min(g + 1, q)] & data_test$D == 0, ]) == 0) { }
      else {
      data_test[data_test$X >= c.vec[g] & data_test$X < c.vec[min(g + 1, q)] & data_test$D == 0, paste0("mu.m")] <- mu_all$mu_m1
      }
      if (nrow(data_test[data_test$X >= c.vec[g] & data_test$X < c.vec[min(g + 1, q)] & data_test$G == g, ]) == 0) { }
      else {
      data_test[data_test$X >= c.vec[g] & data_test$X < c.vec[min(g + 1, q)] & data_test$G == g, paste0("mu.aug")] <- mu_all$mu_aug1
      }

      data_test[data_test$D == 0 & data_test$X < c.vec[g], paste0("pseudo.", g)] <- mu_all$pseudo0
      if (nrow(data_test[data_test$X >= c.vec[max(g - 1, 1)] & data_test$X < c.vec[g] & data_test$D == 1, ]) == 0) { }
      else {
      data_test[data_test$X >= c.vec[max(g - 1, 1)] & data_test$X < c.vec[g] & data_test$D == 1, paste0("mu.m")] <- mu_all$mu_m0
      }
      if (nrow(data_test[data_test$X >= c.vec[max(g - 1, 1)] & data_test$X < c.vec[g] & data_test$G == g, ]) == 0) { }
      else {
      data_test[data_test$X >= c.vec[max(g - 1, 1)] & data_test$X < c.vec[g] & data_test$G == g, paste0("mu.aug")] <- mu_all$mu_aug0
      }
    }

    print(cross_fit_output)
    print(data_test)

    cross_fit_output <- rbind(cross_fit_output, data_test)
  }

  data_all <- cross_fit_output

  Y <- data_all[['Y']]
  psd_dat1 <- psd_dat0 <- NULL  # storing the pseudo outcome (A.2. Step 2 Pseudo Outcome Regression)
  Lip_1 <- Lip_0 <- matrix(0, q, q)  # storing the value of smoothness parameter; 1/0: treatment/control
  dif.1m <- dif.0m <- matrix(0, nrow = q, ncol = q)  # storing the value of estimated cross-group differences at cutoff point

  for (g in seq(1, q - 1, 1)) {
    for (g.pr in seq(g + 1, q, 1)) {
      # ====================================================================== #
      # treatment group
      temp.dat <- data_all %>% filter(D == 1 & X >= c.vec[g.pr])

      # --------------- A.2. Step 2 Pseudo Outcome Regression - 1 --------------
      psout1 <- temp.dat[, paste0("pseudo.", g)] -
        temp.dat[, paste0("pseudo.", g.pr)] +
        with(temp.dat, I(G == g) *
               (Y - eval(parse(text = paste0("pseudo.", g)))) /
               eval(parse(text = paste0("pseudo.ps", g)))) -
        with(temp.dat, I(G == g.pr) *
               (Y - eval(parse(text = paste0("pseudo.", g.pr)))) /
               eval(parse(text = paste0("pseudo.ps", g.pr))))

      temp.vc <- data.frame(psout1, temp.dat$X, g, g.pr)
      names(temp.vc)[1:2] <- c("psout", "X")
      psd_dat1 <- rbind(psd_dat1, temp.vc)

      dif.1m[g, g.pr] <- lprobust(temp.vc[, "psout"],
                                  temp.vc[, "X"],
                                  eval = c.vec[g.pr],
                                  deriv = 0,
                                  p = 1,
                                  bwselect = "mse-dpi")$Estimate[, 5]

      # ---------------- Section 4.3 -------------------------------------------
      Lip_1[g, g.pr] <- abs(lprobust(temp.vc[, "psout"],
                                     temp.vc[, "X"],
                                     eval = c.vec[g.pr],
                                     deriv = 1,
                                     p = 2,
                                     bwselect = "mse-dpi")$Estimate[, 5])

      # ====================================================================== #
      # control group
      temp.dat <- data_all %>% filter(D == 0 & X < c.vec[g])

      # --------------- A.2. Step 2 Pseudo Outcome Regression - 1 --------------
      psout0 <- temp.dat[, paste0("pseudo.", g)] -
        temp.dat[, paste0("pseudo.", g.pr)] +
        with(temp.dat, I(G == g) * (Y - eval(parse(text = paste0("pseudo.", g)))) /
               eval(parse(text = paste0("pseudo.ps", g)))) -
        with(temp.dat, I(G == g.pr) * (Y - eval(parse(text = paste0("pseudo.", g.pr)))) /
               eval(parse(text = paste0("pseudo.ps", g.pr))))

      temp.vc <- data.frame(psout0, temp.dat$X, g, g.pr)
      names(temp.vc)[1:2] <- c("psout", "X")
      psd_dat0 <- rbind(psd_dat0, temp.vc)

      dif.0m[g, g.pr] <- lprobust(temp.vc[, "psout"],
                                  temp.vc[, "X"],
                                  eval = c.vec[g],
                                  deriv = 0,
                                  p = 1,
                                  bwselect = "mse-dpi")$Estimate[, 5]

      # ---------------- Section 4.3 -------------------------------------------
      Lip_0[g, g.pr] <- abs(lprobust(temp.vc[, "psout"],
                                     temp.vc[, "X"],
                                     eval = c.vec[g],
                                     deriv = 1,
                                     p = 2,
                                     bwselect = "mse-dpi")$Estimate[, 5])

    }
  }

  dif.1m <- dif.1m + t(-dif.1m)
  dif.0m <- dif.0m + t(-dif.0m)
  Lip_1 <- Lip_1 + t(Lip_1)
  Lip_0 <- Lip_0 + t(Lip_0)

  out <- list(
    dif.1m_temp = dif.1m,
    dif.0m_temp = dif.0m,
    Lip_1_temp = Lip_1,
    Lip_0_temp = Lip_0,
    data_all_temp = data_all
  )

  return(out)
}

