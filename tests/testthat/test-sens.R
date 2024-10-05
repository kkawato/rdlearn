# test_that("sens and plot", {
#   set.seed(1)
#   data(acces)
#   # result <- rdlearn(y = "elig", x = "saber11", c = "cutoff", group_name = "department", data = acces, fold = 20, M = c(0,1,2,4), cost = 0)
#   result <- rdlearn(y = "elig", x = "saber11", c = "cutoff", group_name = "department", data = acces, fold = 2, M = c(0,1,2,4), cost = 0)
#   # result2 <- rdlearn(y = "elig", x = "saber11", c = "cutoff", group_name = "department", data = acces, fold = 20, M = 1, cost=c(0, 0.2, 0.4, 0.6, 0.8, 1))
#   plot(result, opt = "dif")
#   # sens_result <- sens(result, M = 1, cost=c(0, 0.2, 0.4, 0.6, 0.8, 1))
#   # plot(sens_result)
# })

# # ---------------------------------------------------------------------------- #
# set.seed(1)
# result <- rdlearn(y = "elig", x = "saber11", c = "cutoff", group_name = "department", data = acces, fold = 2, M = c(0,1,2,4), cost = 0)
# plot(result, opt = "dif")
# # ---------------------------------------------------------------------------- #
# set.seed(12345)
# result <- rdlearn(y = "elig", x = "saber11", c = "cutoff", group_name = "department", data = acces, fold = 5, M = c(0,1,2,4), cost = 0)
# result <- sens(result, M = c(0,1,2,4), cost = 0)
# plot(result, opt = "dif")

