estimate_mu <- function (data_train,
                         data_test,
                         c.vec,
                         fold,
                         g,
                         q) {
  mu_all <- list()

  data_test1 <- data_test %>% filter(D == 1)
  data_test0 <- data_test %>% filter(D == 0)
  data_train1 <- data_train %>% filter(D == 1)
  data_train0 <- data_train %>% filter(D == 0)

  # m is for DR estimator (14) in Section 4.1.
  eval.dat1.m <- data_test0 %>% filter(X >= c.vec[g], X < c.vec[min(g + 1, q)]) %>% pull(X) # G == min(g + 1, q) did not work

  # aug is for DR estimator (14) in Section 4.1.
  eval.dat1.aug <- data_test %>% filter(X >= c.vec[g], X < c.vec[min(g + 1, q)], G == g) %>% pull(X)

  # pseudo is for Appendix A.2.
  eval.dat1.pseudo <- data_test1 %>% filter(X >= c.vec[g]) %>% pull(X)

  # m is for DR estimator (14) in Section 4.1.
  eval.dat0.m <- data_test1 %>% filter(X >= c.vec[max(g - 1, 1)], X < c.vec[g] ) %>% pull(X)# G == max(g - 1, 1) did not work

  # aug is for DR estimator (14) in Section 4.1.
  eval.dat0.aug <- data_test %>% filter(X >= c.vec[max(g - 1, 1)], X < c.vec[g], G == g) %>% pull(X)

  # pseudo is for Appendix A.2.
  eval.dat0.pseudo <- data_test0 %>% filter(X < c.vec[g]) %>% pull(X)


  Y1g <- data_train1 %>% filter(G == g) %>% pull(Y)
  X1g <- data_train1 %>% filter(G == g) %>% pull(X)
  Y0g <- data_train0 %>% filter(G == g) %>% pull(Y)
  X0g <- data_train0 %>% filter(G == g) %>% pull(X)

  # local linear regression
  mu_all$pseudo1 <- lprobust(Y1g, X1g, eval = eval.dat1.pseudo, bwselect = "imse-dpi")$Estimate[, 5]
  mu_all$pseudo0 <- lprobust(Y0g, X0g, eval = eval.dat0.pseudo, bwselect = "imse-dpi")$Estimate[, 5]

  if (length(eval.dat1.m) != 0) {
    mu_all$mu_m1 <- lprobust(Y1g, X1g, eval = eval.dat1.m, bwselect = "imse-dpi")$Estimate[, 5]
  }

  if (length(eval.dat1.aug) != 0) {
    mu_all$mu_aug1 <- lprobust(Y1g, X1g, eval = eval.dat1.aug, bwselect = "imse-dpi")$Estimate[, 5]
  }

  if (length(eval.dat0.m) != 0) {
    mu_all$mu_m0 <- lprobust(Y0g, X0g, eval = eval.dat0.m, bwselect = "imse-dpi")$Estimate[, 5]
  }

  if (length(eval.dat0.aug) != 0) {
    mu_all$mu_aug0 <- lprobust(Y0g, X0g, eval = eval.dat0.aug, bwselect = "imse-dpi")$Estimate[, 5]
  }

  return(mu_all)
}
