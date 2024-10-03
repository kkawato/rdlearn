test_that("rdlearn and plot", {
  data(acces)
  set.seed(12345)
  result <- rdlearn(y = "elig", x = "saber11", c = "cutoff", group = "department", data = acces, fold = 2, M = c(0, 1), cost = 0)
  # result <- rdlearn(y = "elig", x = "saber11", c = "cutoff", data = acces, fold = 10, M = c(0, 1), cost = 0)
  # result <- rdlearn(y = "elig", x = "saber11", c = "cutoff", data = acces, fold = 20, M = c(0, 1), cost = 0)
  plot(result, opt = "dif")
})

# result <- rdlearn(y = "elig", x = "saber11", c = "cutoff", group = "department", data = acces, fold = 2, M = c(0, 1), cost = 0)
