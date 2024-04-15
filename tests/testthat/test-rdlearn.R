rm(list=ls())
set.seed(12345)
library(tidyverse)
library(nprobust)
library(nnet)
library(ggplot2)

result <- rdlearn(y = "acces", x = "saber11", c = "cutoff", groupname = "department", data = colombia_acces, fold = 5, M = c(0, 1), cost = 0)
plot.rdlearn(result)
result2 <- sens.rdlearn(result, M = 1, cost = c(0, 0.2, 0.4, 0.6, 0.8, 1))
plot.rdlearn(result2)


sens.rdlearn(result, M = 1, cost = c(0, 0.2, 0.4, 0.6, 0.8, 1))
sens.rdlearn(result, M = 1, cost = c(0, 1))
sens.rdlearn(result, M = c(0,1), cost = 0)


result <- rdlearn(y = "acces", x = "saber11", c = "cutoff", groupname = "department", data = colombia_acces, fold = 20, M = c(0, 1, 2, 4), cost = 0)
plot.rdlearn(result)
# Figure 2
# use "plot.rdlearn" to visualize the result.

sens.rdlearn(result, M=1, cost=c(0, 0.2, 0.4, 0.6, 0.8, 1))
sens.rdlearn(result, M=1, cost=c(0, 1))
# Figure 3
# "sens.rdlearn" is for sensitivity analysis. The output is a plot.
# This function inherits the cross-fitting data.

result_simA <- rdlearn(y = "out", x = "run", c = "cut", data = simdata_A, fold = 10, M=1, cost=0)
plot.rdlearn(result_simA)
# This worked with the simulation data in the appendix.


# Experiment!
cost = 0
M = c(1, 2, 3, 4)
# groupname = list("Group1", "Group2", "Group3")
groupname = c("Group1", "Group2", "Group3")
safecut_all <- data.frame(group = groupname)

  for (temp_cost in cost) {
    for (temp_M in M) {
      print(paste("Calculating the case of M =", temp_M, "C =", temp_cost))

      c.all <- rep(0, 3)
      for (i in 1:3) {
        c.all[i] = i + temp_M + temp_cost
        print(c.all)
      }
      print("result")
      print(c.all)

      c.all_df <- data.frame(colname = c.all, group = groupname)
      names(c.all_df)[1] <- paste0("M=", temp_M, ",", "C=", temp_cost)
      safecut_all <- full_join(safecut_all, c.all_df, by = ("group" = "group"))
    }
  }

safecut_all

# 求めている結果は

c.all[1] <- 1
c.all[2] <- 2
c.all
