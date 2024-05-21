test_that("sens and plot", {
  data(acces)
  result <- rdlearn(y = "elig", x = "saber11", c = "cutoff", data = acces, fold = 5, M = 0, cost = 0)
  sens_result <- sens(result, M = 1, cost=c(0, 0.2, 0.4, 0.6, 0.8, 1))
  plot(sens_result)
})
