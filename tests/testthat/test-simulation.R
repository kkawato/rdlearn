test_that("rdlearn learns oracle policy", {
  # ---------------------------------------------------------------------------- #
  # data generating process from appendix B
  # ---------------------------------------------------------------------------- #
  n <- 2000
  X <- runif(n, -1000, -1)
  G <- 2 * as.numeric(I(0.01 * X + 5 + rnorm(n, sd = 10) > 0)) + as.numeric(I(0.01 * X + 5 + rnorm(n, sd = 10) <= 0))
  c1 <- -850
  c0 <- -571
  C <- ifelse(G == 1, c1, c0)
  D <- as.numeric(X >= C)

  coef0 <- c(-1.992230e+00, -1.004582e-02, -1.203897e-05, -4.587072e-09)
  coef1 <- c(9.584361e-01, 5.308251e-04, 1.103375e-06, 1.146033e-09)
  Px <- poly(X - 735.4334 - c1, degree = 3, raw = TRUE) # -735.4334-c1 = 164.43
  Px <- cbind(rep(1, nrow(Px)), Px)
  EY0 <- Px %*% coef0
  EY1 <- Px %*% coef1

  d <- 0.2 + exp(0.01 * X) * (1 - G) + 0.3 * (1 - D)
  Y <- EY0 * (1 - D) + EY1 * D - d * as.numeric(I(G == 1)) + rnorm(n, sd = 0.3)

  simdata_A <- data.frame(Y = Y, X = X, C = C)

  # ---------------------------------------------------------------------------- #
  # test
  # ---------------------------------------------------------------------------- #
  result <- rdlearn(y = "Y", x = "X", c = "C", data = simdata_A, fold = 2, M = 1, cost = 0)
  expect_equal(result$safe_cut[1, 1], -850, tolerance = 10)
})
