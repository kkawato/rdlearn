# rm(list=ls())
set.seed(12345)
library(tidyverse)
library(nprobust)
library(nnet)
library(ggplot2)

result <- rdlearn(y = "elig", x = "saber11", c = "cutoff", groupname = "department", data = acces, fold = 20, M = c(0, 1), cost = 0)
plot(result)

# result <- rdlearn(y = "elig", x = "saber11", c = "cutoff", groupname = "department", data = acces, fold = 10, M = c(0, 1), cost = 0)
# plot(result)

result2 <- sens(result, M = c(0,1), cost = 0)
plot(result2)

# result <- rdlearn(y = "elig", x = "saber11", c = "cutoff", groupname = "department", data = acces, fold = 20, M = c(0, 1, 2, 4), cost = 0)
# plot.rdlearn(result)
# # Figure 2
# use "plot.rdlearn" to visualize the result.

result2 <- sens.rdlearn(result, M = 1, cost=c(0, 0.2, 0.4, 0.6, 0.8, 1))
plot.rdlearn(result2)
# Figure 3
# "sens.rdlearn" is for sensitivity analysis. The output is a plot.
# This function inherits the cross-fitting data.

result_simA <- rdlearn(y = "out", x = "run", c = "cut", data = simdata_A, fold = 5, M=c(1,2), cost=0)
plot(result_simA)
# This also worked with the simulation data in the appendix.

debug(rdlearn)




