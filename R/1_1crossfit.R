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

    gamfit <- multinom(formula = G ~ X, data = data_train)
    ps <- predict(gamfit, newdata = data_test, "probs")
    data_test[, paste0("pseudo.ps", seq(1, q, 1))] <- predict(gamfit, newdata = data_test, "probs")

    for (g in seq(1, q, 1)) {
      mu_all <- estimate_mu(data_train, data_test, c.vec, k, g, q)

      pseudo1 <- (data_test$D == 1) & (data_test$X >= c.vec[g])
      pseudo0 <- (data_test$D == 0) & (data_test$X < c.vec[g])
      m1 <- (data_test$X >= c.vec[g]) & (data_test$X < c.vec[min(g + 1, q)]) & (data_test$D == 0)
      aug1 <- (data_test$X >= c.vec[g]) & (data_test$X < c.vec[min(g + 1, q)]) & (data_test$G == g)
      m0 <- (data_test$X >= c.vec[max(g - 1, 1)]) & (data_test$X < c.vec[g]) & (data_test$D == 1)
      aug0 <- (data_test$X >= c.vec[max(g - 1, 1)]) & (data_test$X < c.vec[g]) & (data_test$G == g)

      data_test[pseudo1, paste0("pseudo.", g)] <- ifelse(!is.null(mu_all$pseudo1), mu_all$pseudo1, 0)
      data_test[pseudo0, paste0("pseudo.", g)] <- ifelse(!is.null(mu_all$pseudo0), mu_all$pseudo0, 0)
      data_test[m1, paste0("mu.m")] <- ifelse(!is.null(mu_all$mu_m1), mu_all$mu_m1, 0)
      data_test[aug1, paste0("mu.aug")] <- ifelse(!is.null(mu_all$mu_aug1), mu_all$mu_aug1, 0)
      data_test[m0, paste0("mu.m")] <- ifelse(!is.null(mu_all$mu_m0), mu_all$mu_m0, 0)
      data_test[aug0, paste0("mu.aug")] <- ifelse(!is.null(mu_all$mu_aug0), mu_all$mu_aug0, 0)
    }
    cross_fit_output <- rbind(cross_fit_output, data_test)
  }

  Y <- cross_fit_output[['Y']]
  for (d in c(1, 0)) {
    psd_dat <- NULL  # storing the pseudo outcome (A.2. Step 2 Pseudo Outcome Regression)
    Lip <- matrix(0, q, q)
    dif <- matrix(0, nrow = q, ncol = q)

    for (g in seq(1, q - 1, 1)) {
      for (g.pr in seq(g + 1, q, 1)) {
        if (d == 1){ temp.dat <- cross_fit_output %>% filter(D == 1 & X >= c.vec[g.pr]) }
        if (d == 0) { temp.dat <- cross_fit_output %>% filter(D == 0 & X < c.vec[g]) }

        psout <- temp.dat[, paste0("pseudo.", g)] - temp.dat[, paste0("pseudo.", g.pr)] +
          with(temp.dat, I(G == g) *
                 (Y - eval(parse(text = paste0("pseudo.", g)))) /
                 eval(parse(text = paste0("pseudo.ps", g)))) -
          with(temp.dat, I(G == g.pr) *
                 (Y - eval(parse(text = paste0("pseudo.", g.pr)))) /
                 eval(parse(text = paste0("pseudo.ps", g.pr))))

        temp.vc <- data.frame(psout, temp.dat$X, g, g.pr)
        names(temp.vc)[1:2] <- c("psout", "X")
        psd_dat <- rbind(psd_dat, temp.vc)

        eval_point <- c.vec[g.pr] * (d == 1) + c.vec[g] * (d == 0)
        dif[g, g.pr] <- lprobust(temp.vc[, "psout"], temp.vc[, "X"],
                                 eval = eval_point, deriv = 0, p = 1, bwselect = "mse-dpi")$Estimate[, 5]
        Lip[g, g.pr] <- abs(lprobust(temp.vc[, "psout"], temp.vc[, "X"],
                                     eval = eval_point, deriv = 1, p = 2, bwselect = "mse-dpi")$Estimate[, 5])

      }
    }
    dif <- dif + t(-dif)
    Lip <- Lip + t(Lip)
    assign(paste0("dif_", d), dif)
    assign(paste0("Lip_", d), Lip)
  }

  out <- list(
    dif.1m_temp = dif_1,
    dif.0m_temp = dif_0,
    Lip_1_temp = Lip_1,
    Lip_0_temp = Lip_0,
    data_all_temp = cross_fit_output
  )
  return(out)
}


