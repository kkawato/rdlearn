test_that("plot works with any M or cost", {
  data("simdata_A")
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
    plot(result2, opt = "dif")
  )

  result3 <- rdlearn(y = "Y", x = "X", c = "C", data = simdata_A, fold = 2, M = 1, cost = c(0, 0.2, 0.4, 0.6, 0.8, 1))
  expect_silent(
    plot(result3, opt = "safe")
  )
  expect_silent(
    plot(result3, opt = "dif")
  )
})

test_that("sens works with any M or cost", {
  data("simdata_A")
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
