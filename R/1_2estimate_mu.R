estimate_mu <- function (data_train,
                         data_test,
                         c.vec,
                         fold,
                         g,
                         q) {
  mu_all <- data.frame()

  data_testg <- data_test %>% filter(G == g)

  data_test1 <- data_test %>% filter(D == 1)
  data_test1g <- data_test %>% filter(D == 1, G == g)

  data_test0 <- data_test %>% filter(D == 0)
  data_test0g <- data_test %>% filter(D == 0, G == g)

  data_train1 <- data_train %>% filter(D == 1)
  data_train1g <- data_train %>% filter(D == 1, G == g)

  data_train0 <- data_train %>% filter(D == 0)
  data_train0g <- data_train %>% filter(D == 0, G == g)

  # m is for DR estimator (14) in Section 4.1.
  eval.dat1.m <- data_test0 %>% filter(X >= c.vec[g], X < c.vec[min(g + 1, q)]) %>% pull(X) # G == min(g + 1, q) did not work

  # aug is for DR estimator (14) in Section 4.1.
  eval.dat1.aug <- data_testg %>% filter(X >= c.vec[g], X < c.vec[min(g + 1, q)]) %>% pull(X)

  # pseudo is for Appendix A.2.
  eval.dat1.pseudo <- data_test1 %>% filter(X >= c.vec[g]) %>% pull(X)

  # local linear regression
  pseudo1 <- lprobust(data_train1g$Y,
                      data_train1g$X,
                      eval = eval.dat1.pseudo,
                      bwselect = "imse-dpi")$Estimate[, 5]

  if (length(eval.dat1.m) == 0) { }
  else {
  mu.m1 <- lprobust(data_train1g$Y,
                      data_train1g$X,
                      eval = eval.dat1.m,
                      bwselect = "imse-dpi")$Estimate[, 5]
  }

  if (length(eval.dat1.aug) == 0) { }
  else{
  mu.aug1 <- lprobust(data_train1g$Y,
                      data_train1g$X,
                      eval = eval.dat1.aug,
                      bwselect = "imse-dpi")$Estimate[, 5]
  }

  # ====================================================================== #
  # Appendix A.2. Step 1. (b)
  # Constructing the estimates of the group-specific regression functions
  # control group
  # ====================================================================== #

  # m is for DR estimator (14) in Section 4.1.
  eval.dat0.m <- data_test1 %>% filter(X >= c.vec[max(g - 1, 1)], X < c.vec[g] ) %>% pull(X)# G == max(g - 1, 1) did not work

  # aug is for DR estimator (14) in Section 4.1.
  eval.dat0.aug <- data_testg %>% filter(X >= c.vec[max(g - 1, 1)], X < c.vec[g]) %>% pull(X)

  # pseudo is for Appendix A.2.
  eval.dat0.pseudo <- data_test0 %>% filter(X < c.vec[g]) %>% pull(X)

  # local linear regression
  pseudo0 <- lprobust(data_train0g$Y,
                      data_train0g$X,
                      eval = eval.dat0.pseudo,
                      bwselect = "imse-dpi")$Estimate[, 5]

  if (length(eval.dat0.m) == 0) { }
  else {
  mu.m0 <- lprobust(data_train0g$Y,
                      data_train0g$X,
                      eval = eval.dat0.m,
                      bwselect = "imse-dpi")$Estimate[, 5]
  }

  if (length(eval.dat0.aug) == 0) { }
  else{
  mu.aug0 <- lprobust(data_train0g$Y,
                      data_train0g$X,
                      eval = eval.dat0.aug,
                      bwselect = "imse-dpi")$Estimate[, 5]
  }

  mu_all <- list(
    pseudo1 = pseudo1,
    pseudo0 = pseudo0
  )

  if (exists("mu.m1")) {
    mu_all$mu_m1 <- mu.m1
  }
  if (exists("mu.aug1")) {
    mu_all$mu_aug1 <- mu.aug1
  }
  if (exists("mu.m0")) {
    mu_all$mu_m0 <- mu.m0
  }
  if (exists("mu.aug0")) {
    mu_all$mu_aug0 <- mu.aug0
  }

  return(mu_all)
}
