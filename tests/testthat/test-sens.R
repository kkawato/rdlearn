test_that("sens and plot", {
  data(acces)
  result <- rdlearn(y = "elig", x = "saber11", c = "cutoff", group_name = "department", data = acces, fold = 20, M = c(0,1,2,4), cost = 0)
  # result2 <- rdlearn(y = "elig", x = "saber11", c = "cutoff", group_name = "department", data = acces, fold = 20, M = 1, cost=c(0, 0.2, 0.4, 0.6, 0.8, 1))
  plot(result)
  sens_result <- sens(result, M = 1, cost=c(0, 0.2, 0.4, 0.6, 0.8, 1))
  plot(sens_result)
})

set.seed(12345)
