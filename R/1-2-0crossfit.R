#' Implement cross-fitting to estimate nuisance components
#'
#' @description
#' This function performs two calculations simultaneously.
#'
#' First, to construct a doubly robust estimator of the point identifiable component
#' \code{Xi_1}, we adopt a fully nonparametric approach to estimating the following
#' nuisance components: the conditional outcome regression for each group g
#' \tilde{m(X,g)} and the conditional probability of group membership g given
#' \tilde{e_g(X)}. Specifically, we fit a local linear regression (\code{nprobust}) for
#' \tilde{m(X,g)} and fit a multinomial logistic
#' regression for \tilde{e_g(X)} based on a single hidden layer neural network (\code{nnet})
#' . For more detail, Please refer to the "4.1 Doubly robust
#' estimation"
#'
#' Second, to calculate the worst-case value of \code{Xi_2}, which is partially
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
#' @return A data frame containing the cross-fitted outcomes and other intermediate calculations.
#'
#' @importFrom stats predict
#' @importFrom dplyr %>% filter ungroup select arrange
#' @importFrom tidyr unnest
#' @importFrom nnet multinom
#' @importFrom cli cli_progress_bar
#' @importFrom cli cli_progress_update
#' @keywords internal
#' @noRd
crossfit <- function(
    c.vec,
    q,
    fold,
    data_all,
    trace) {
  cross_fit_output <- data.frame()
  for (k in 1:fold) {
    if (isTRUE(trace)) {
      cat(paste0("Cross fitting for fold ", k, "\n"))
    }

    data_train <- data_all %>% filter(fold_id != k)
    data_test <- data_all %>% filter(fold_id == k)

    # conditional prob of group
    gamfit <- nnet::multinom(formula = G ~ X, data = data_train, trace = "FALSE")
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

      if (nrow(data_test[pseudo1, ]) > 0 && !is.null(mu_all$pseudo1)) {
        data_test[pseudo1, paste0("pseudo.", g)] <- mu_all$pseudo1
      }

      if (nrow(data_test[pseudo0, ]) > 0 && !is.null(mu_all$pseudo0)) {
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



#' Estimate the conditional outcome regression for each group g \tilda{m(X,g)}
#'
#' Please refer to the description of 1-2-0crossfit.R.
#'
#' @param data_train The training data for the current fold of cross-fitting.
#' @param data_test The test data for the current fold of cross-fitting.
#' @param c.vec A vector containing cutoff values.
#' @param fold The current fold ID for cross-fitting.
#' @param g The group indicator.
#' @param q The total number of groups.
#'
#' @return A list containing the estimates of the group-specific regression
#'   functions for each group: \item{pseudo1}{Estimates for the treated group
#'   for Appendix A.2.} \item{mu_m1}{Estimates for the treated group for DR
#'   estimator (14) in Section 4.1.} \item{mu_aug1}{Estimates for the treated
#'   group for the part of augmentation in DR estimator (14) in Section 4.1.}
#'   \item{pseudo0}{Estimates for the control group for Appendix A.2.}
#'   \item{mu_m0}{Estimates for the control group for DR estimator (14) in
#'   Section 4.1.} \item{mu_aug0}{Estimates for the control group for the part
#'   of augmentation in DR estimator (14) in Section 4.1.}
#'
#' @importFrom nprobust lprobust
#' @importFrom dplyr %>% filter pull
#' @keywords internal
#' @noRd
estimate_mu <- function(data_train,
                        data_test,
                        c.vec,
                        fold,
                        g,
                        q) {
  data_test1 <- data_test %>% filter(D == 1)
  data_test0 <- data_test %>% filter(D == 0)
  data_train1 <- data_train %>% filter(D == 1)
  data_train0 <- data_train %>% filter(D == 0)

  # m is for DR estimator (14) in Section 4.1.
  eval.dat1.m <- data_test0 %>%
    filter(X >= c.vec[g], X < c.vec[min(g + 1, q)]) %>%
    pull(X)

  # aug is for DR estimator (14) in Section 4.1.
  eval.dat1.aug <- data_test %>%
    filter(X >= c.vec[g], X < c.vec[min(g + 1, q)], G == g) %>%
    pull(X)

  # pseudo is for Appendix A.2.
  eval.dat1.pseudo <- data_test1 %>%
    filter(X >= c.vec[g]) %>%
    pull(X)

  # m is for DR estimator (14) in Section 4.1.
  eval.dat0.m <- data_test1 %>%
    filter(X >= c.vec[max(g - 1, 1)], X < c.vec[g]) %>%
    pull(X)

  # aug is for DR estimator (14) in Section 4.1.
  eval.dat0.aug <- data_test %>%
    filter(X >= c.vec[max(g - 1, 1)], X < c.vec[g], G == g) %>%
    pull(X)

  # pseudo is for Appendix A.2.
  eval.dat0.pseudo <- data_test0 %>%
    filter(X < c.vec[g]) %>%
    pull(X)

  Y1g <- data_train1 %>%
    filter(G == g) %>%
    pull(Y)
  X1g <- data_train1 %>%
    filter(G == g) %>%
    pull(X)
  Y0g <- data_train0 %>%
    filter(G == g) %>%
    pull(Y)
  X0g <- data_train0 %>%
    filter(G == g) %>%
    pull(X)

  mu_all <- list()
  data_list <- list(
    list(y = Y1g, x = X1g, eval_dat = eval.dat1.pseudo, name = "pseudo1"),
    list(y = Y1g, x = X1g, eval_dat = eval.dat1.m, name = "mu_m1"),
    list(y = Y1g, x = X1g, eval_dat = eval.dat1.aug, name = "mu_aug1"),
    list(y = Y0g, x = X0g, eval_dat = eval.dat0.pseudo, name = "pseudo0"),
    list(y = Y0g, x = X0g, eval_dat = eval.dat0.m, name = "mu_m0"),
    list(y = Y0g, x = X0g, eval_dat = eval.dat0.aug, name = "mu_aug0")
  )

  for (data in data_list) {
    y <- data$y
    x <- data$x
    eval_dat <- data$eval_dat
    name <- data$name

    if (length(eval_dat) > 0) {
      tryCatch(
        {suppressWarnings({
          estimate <- nprobust::lprobust(y, x, eval = eval_dat, bwselect = "imse-dpi")$Estimate[, 5]
          estimate[is.na(estimate)] <- 0
          mu_all[[name]] <- estimate
        })
        },
        error = function(e) {
          mu_all[[name]] <- 0
        }
      )
    }
  }
  return(mu_all)
}
