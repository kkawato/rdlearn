test_that("plot works with any M or cost", {
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
  result1 <- rdlearn(y = "Y", x = "X", c = "C", data = simdata_A, fold = 2, M = 1, cost = 0)
  expect_silent(
    plot(result1, opt = "safe")
  )
  expect_silent(
    plot(result1, opt = "dif")
  )

  result2 <- rdlearn(y = "Y", x = "X", c = "C", data = simdata_A, fold = 2, M = c(0, 1, 2, 4, 8, 16), cost = 0)

  expect_silent(
    plot(result2, opt = "safe")
  )
  expect_silent(
    plot(result2, opt = "safe")
  )

  result3 <- rdlearn(y = "Y", x = "X", c = "C", data = simdata_A, fold = 2, M = 1, cost = c(0, 0.2, 0.4, 0.6, 0.8, 1))
  expect_silent(
    plot(result3, opt = "safe")
  )
  expect_silent(
    plot(result3, opt = "dif")
  )
})

test_that("sens works", {
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
  expect_silent(
    result <- sens(result, M = c(0, 1, 2, 4), cost = 0, trace = FALSE)
  )
  expect_silent(
    result <- sens(result, M = 1, cost = c(0, 0.5, 1), trace = FALSE)
  )

  expect_error(
    sens(result, cost = 0),
    "M is missing"
  )
  expect_error(
    sens(result, M = 1),
    "cost is missing"
  )
  expect_error(
    sens(result, M = c(1, 2), cost = c(0.1, 0.2)),
    "Both M and cost are vectors."
  )
})
