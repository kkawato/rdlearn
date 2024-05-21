#' Implement cross-fitting to estimate nuisance components
#'
#' @description
#' This function performs two calculations simultaneously.
#'
#' First, to construct a doubly robust estimator of the point identifiable component
#' \code{\Theta_1}, we adopt a fully nonparametric approach to estimating the following
#' nuisance components: the conditional outcome regression for each group g
#' \tilde{m(X,g)} and the conditional probability of group membership g given
#' \tilde{e_g(X)}. Specifically, we fit a local linear regression for
#' \tilde{m(X,g)} available in \code{nprobust} and fit a multinomial logistic
#' regression for \tilde{e_g(X)} based on a single hidden layer neural network
#' of the \code{nnet}. For more detail, Please refer to the "4.1 Doubly robust
#' estimation"
#'
#' Second, to calculate the worst-case value of \code{\Theta_2}, which is partially
#' identifiable, we need to estimate the conditional differences \code{dif}
#' between observed treatment groups or between observed control groups using
#' data above or below the threshold, respectively. We propose an efficient
#' two-stage doubly robust estimator that has a fast convergence rate, which
#' requires the calculation of \code{pseudo}. For more detail, Please refer to
#' the "Appendix A.2. A double robust estimator for heterogeneous cross-group
#' differences Step 1. Nuisance training. and Step 2. Pseudo-outcome
#' regression", "4.2 Estimating the bounds" and "4.3 Choosing the smoothness
#' parameter".
#'
#' @param c.vec A vector of cutoff values.
#' @param q The number of groups.
#' @param fold The number of folds for cross-fitting.
#' @param data_all The full data frame containing all variables.
#' @param trace A logical value that controls whether to display the progress.
#'   If set to TRUE, the progress will be printed. The default value is TRUE.
#'
#' @return A dataframe containing the cross-fitted outcomes and other intermediate calculations.
#'
#' @importFrom stats predict
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
  cross_fit_output <- data.frame()

  for (k in 1:fold) {
    if (trace == TRUE){
      print(paste0("Cross fitting for fold ",k))
    }
    data_train <- data_all %>% filter(fold_id != k)
    data_test <- data_all %>% filter(fold_id  == k)

    # conditional prob of group
    gamfit <- nnet::multinom(formula = G ~ X, data = data_train, trace = "FALSE")
    ps <- predict(gamfit, newdata = data_test, "probs")
    data_test[, paste0("pseudo.ps", seq(1, q, 1))] <- predict(gamfit, newdata = data_test, "probs")

    for (g in seq(1, q, 1)){
      mu_all <- estimate_mu(data_train, data_test, c.vec, k, g, q)

      pseudo1 <- (data_test$D == 1) & (data_test$X >= c.vec[g])
      pseudo0 <- (data_test$D == 0) & (data_test$X < c.vec[g])
      m1 <- (data_test$X >= c.vec[g]) & (data_test$X < c.vec[min(g + 1, q)]) & (data_test$D == 0)
      aug1 <- (data_test$X >= c.vec[g]) & (data_test$X < c.vec[min(g + 1, q)]) & (data_test$G == g)
      m0 <- (data_test$X >= c.vec[max(g - 1, 1)]) & (data_test$X < c.vec[g]) & (data_test$D == 1)
      aug0 <- (data_test$X >= c.vec[max(g - 1, 1)]) & (data_test$X < c.vec[g]) & (data_test$G == g)

      if (nrow(data_test[pseudo1, ]) > 0 && !is.null(mu_all$pseudo1)){
        data_test[pseudo1, paste0("pseudo.", g)] <- mu_all$pseudo1
      }

      if (nrow(data_test[pseudo0, ]) > 0 && !is.null(mu_all$pseudo0)){
        data_test[pseudo0, paste0("pseudo.", g)] <- mu_all$pseudo0
      }

      if (nrow(data_test[m1, ]) > 0 && !is.null(mu_all$mu_m1)) {
        data_test[m1, paste0("mu.m")] <- mu_all$mu_m1
      }

      if (nrow(data_test[aug1, ]) > 0 && !is.null(mu_all$mu_aug1)) {
        data_test[aug1, paste0("mu.aug")] <- mu_all$mu_aug1
      }

      if (nrow(data_test[m0, ]) > 0 && !is.null(mu_all$mu_m0)) {
        data_test[m0, paste0("mu.m")] <- mu_all$mu_m0
      }

      if (nrow(data_test[aug0, ]) > 0 && !is.null(mu_all$mu_aug0)) {
        data_test[aug0, paste0("mu.aug")] <- mu_all$mu_aug0
      }
    }
    cross_fit_output <- rbind(cross_fit_output, data_test)
  }
  return(cross_fit_output)
}
