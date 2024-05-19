#' Implement cross-fitting for estimating cross-group differences and
#' calculating the smoothness parameter
#'
#' This function performs cross-fitting to estimate cross-group difference
#' (dif). This function also calculates the smoothness parameter (Lip). It
#' follows the procedure outlined in Appendix A.2 and Sections 4.1 and 4.3 of
#' the referenced source.
#'
#' @param c.vec A vector of cutoff values.
#' @param q The number of groups.
#' @param fold The number of folds for cross-fitting.
#' @param data_all The full data frame containing all variables.
#' @param trace A logical value that controls whether to display the progress.
#'   If set to TRUE, the progress will be printed. The default value is TRUE.
#'
#' @return A list with the following components: \item{dif_1}{A matrix of
#'   estimated differences for the treated group (D = 1).} \item{dif_0}{A matrix
#'   of estimated differences for the control group (D = 0).} \item{Lip_1}{A
#'   matrix of estimated Lipschitz constants for the treated group (D = 1).}
#'   \item{Lip_0}{A matrix of estimated Lipschitz constants for the control
#'   group (D = 0).} \item{cross_fit_output}{The data frame containing the
#'   cross-fitted outcomes and other intermediate calculations.}
#'
#' @importFrom dplyr %>% filter ungroup select arrange
#' @importFrom tidyr unnest
#' @importFrom nnet multinom
#' @keywords internal
#' @noRd
crossfit <- function(
  c.vec,
  q,
  fold,
  data_all,
  trace)
{
  ################################################################################
  # please refer to
  # Appendix A.2. A double robust estimator for heterogeneous cross-group differences
  # Section 4.1. Doubly robust estimation
  # Section 4.3. Choosing the smoothness parameter
  ################################################################################

  cross_fit_output <- data.frame()

  for (k in 1:fold) {
    if (trace == TRUE){
      print(paste0("Cross fitting for fold ",k))
    }
    data_train <- data_all %>% filter(fold_id != k)
    data_test <- data_all %>% filter(fold_id  == k)

    # conditional prob of group
    gamfit <- multinom(formula = G ~ X, data = data_train, trace = "FALSE")
    ps <- predict(gamfit, newdata = data_test, "probs")
    data_test[, paste0("pseudo.ps", seq(1, q, 1))] <- predict(gamfit, newdata = data_test, "probs")

    for (g in seq(1, q, 1)){
      mu_all <- estimate_mu(data_train, data_test, c.vec, k, g, q)
      data_test[data_test$D == 1 & data_test$X >= c.vec[g], paste0("pseudo.", g)] <- mu_all$pseudo1
      data_test[data_test$D == 0 & data_test$X < c.vec[g], paste0("pseudo.", g)] <- mu_all$pseudo0

      m1 <- (data_test$X >= c.vec[g]) & (data_test$X < c.vec[min(g + 1, q)]) & (data_test$D == 0)
      aug1 <- (data_test$X >= c.vec[g]) & (data_test$X < c.vec[min(g + 1, q)]) & (data_test$G == g)
      m0 <- (data_test$X >= c.vec[max(g - 1, 1)]) & (data_test$X < c.vec[g]) & (data_test$D == 1)
      aug0 <- (data_test$X >= c.vec[max(g - 1, 1)]) & (data_test$X < c.vec[g]) & (data_test$G == g)

      if (nrow(data_test[m1, ]) > 0) {
        data_test[m1, paste0("mu.m")] <- mu_all$mu_m1
      }

      if (nrow(data_test[aug1, ]) > 0) {
        data_test[aug1, paste0("mu.aug")] <- mu_all$mu_aug1
      }

      if (nrow(data_test[m0, ]) > 0) {
        data_test[m0, paste0("mu.m")] <- mu_all$mu_m0
      }

      if (nrow(data_test[aug0, ]) > 0) {
      data_test[aug0, paste0("mu.aug")] <- mu_all$mu_aug0
      }
    }
    cross_fit_output <- rbind(cross_fit_output, data_test)
  }

  Y <- cross_fit_output[['Y']]

  dif_Lip_output <- estimate_dif_lip(
    cross_fit_output = cross_fit_output,
    q = q,
    c.vec = c.vec,
    trace = trace
  )

  out <- list(
    dif_1 = dif_Lip_output$dif_1,
    dif_0 = dif_Lip_output$dif_0,
    Lip_1 = dif_Lip_output$Lip_1,
    Lip_0 = dif_Lip_output$Lip_0,
    cross_fit_output = cross_fit_output
  )

  return(out)
}
