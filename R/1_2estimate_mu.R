estimate_mu <- function (data_train,
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
  eval.dat1.m <- data_test0 %>% filter(X >= c.vec[g], X < c.vec[min(g + 1, q)]) %>% pull(X)

  # aug is for DR estimator (14) in Section 4.1.
  eval.dat1.aug <- data_test %>% filter(X >= c.vec[g], X < c.vec[min(g + 1, q)], G == g) %>% pull(X)

  # pseudo is for Appendix A.2.
  eval.dat1.pseudo <- data_test1 %>% filter(X >= c.vec[g]) %>% pull(X)

  # m is for DR estimator (14) in Section 4.1.
  eval.dat0.m <- data_test1 %>% filter(X >= c.vec[max(g - 1, 1)], X < c.vec[g] ) %>% pull(X)

  # aug is for DR estimator (14) in Section 4.1.
  eval.dat0.aug <- data_test %>% filter(X >= c.vec[max(g - 1, 1)], X < c.vec[g], G == g) %>% pull(X)

  # pseudo is for Appendix A.2.
  eval.dat0.pseudo <- data_test0 %>% filter(X < c.vec[g]) %>% pull(X)

  Y1g <- data_train1 %>% filter(G == g) %>% pull(Y)
  X1g <- data_train1 %>% filter(G == g) %>% pull(X)
  Y0g <- data_train0 %>% filter(G == g) %>% pull(Y)
  X0g <- data_train0 %>% filter(G == g) %>% pull(X)

  mu_all <- list()
  data_list <- list(
    list(Y1g, X1g, eval.dat1.pseudo, "pseudo1"),
    list(Y1g, X1g, eval.dat1.m, "mu_m1"),
    list(Y1g, X1g, eval.dat1.aug, "mu_aug1"),
    list(Y0g, X0g, eval.dat0.pseudo, "pseudo0"),
    list(Y0g, X0g, eval.dat0.m, "mu_m0"),
    list(Y0g, X0g, eval.dat0.aug, "mu_aug0")
  )

  for (data in data_list) {
    y <- data[[1]]
    x <- data[[2]]
    eval_dat <- data[[3]]
    name <- data[[4]]

    if (length(eval_dat) > 0) {
      tryCatch({
        mu_all[[name]] <- lprobust(y, x, eval = eval_dat, bwselect = "imse-dpi")$Estimate[, 5]
      }, error = function(e) mu_all[[name]] <- 0)
    }
  }

  return(mu_all)
}

#-------------------------------------#
# trycatch for avoiding this error
# this error frequently occurs in case the number of folds is small
#-------------------------------------#
# Error in matrix(NA, n.B, o.B + 1) :
#   invalid 'nrow' value (too large or NA)
# 9.
# matrix(NA, n.B, o.B + 1)
# 8.
# lprobust.bw(y, x, cluster, c = eval, o = p, nu = deriv, o.B = q,
#             h.V = c.bw, h.B1 = bw.mp1, h.B2 = bw.mp2, bwregul, vce, nnmatch,
#             kernel, dups, dupsid)
# 7.
# lpbwselect.mse.dpi(y = y, x = x, cluster = cluster, eval = eval[i],
#                    p = p, q = q, deriv = deriv, kernel = kernel, bwcheck = bwcheck,
#                    bwregul = bwregul, vce = vce, nnmatch = nnmatch, interior = interior)
# 6.
# lpbwselect.imse.dpi(y = y, x = x, cluster = cluster, p = p, q = q,
#                     deriv = deriv, kernel = kernel, bwcheck = bwcheck, bwregul = bwregul,
#                     imsegrid = imsegrid, vce = vce, nnmatch = nnmatch, interior = interior)
# 5.
# lpbwselect(y = y, x = x, eval = eval, deriv = deriv, p = p, vce = vce,
#            cluster = cluster, bwselect = bwselect, interior = interior,
#            kernel = kernel, bwcheck = bwcheck, bwregul = bwregul, imsegrid = imsegrid,
#            subset = subset)
# 4.
# lprobust(Y1g, X1g, eval = eval.dat1.pseudo, bwselect = "imse-dpi") at 1_2estimate_mu.R#39
# 3.
# estimate_mu(data_train, data_test, c.vec, k, g, q) at 1_1crossfit.R#35
# 2.
# crossfit(c.vec = c.vec, q = q, fold = fold, data_split = data_split,
#          data_all = data_all) at 1_0rdlearn.R#135
# 1.
# rdlearn(y = "elig", x = "saber11", c = "cutoff", groupname = "department",
#         data = acces, fold = 20, M = c(0, 1), cost = 0)
