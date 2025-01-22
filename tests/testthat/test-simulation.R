test_that("rdlearn learns oracle policy", {
  data("simdata_A")
  result <- rdlearn(y = "Y", x = "X", c = "C", data = simdata_A, fold = 2, M = 1, cost = 0)
  expect_equal(result$safe_cut[1, 1], -850, tolerance = 10)
})
