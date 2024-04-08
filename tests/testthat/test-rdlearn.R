rm(list=ls())
set.seed(12345)
library(tidyverse)
library(nprobust)
library(nnet)
library(ggplot2)


result = rdlearn(y = "acces", x = "saber11", c = "cutoff", groupname = "department", data = colombia_acces, fold = 20, M = c(0,1), cost = 0)
plot.rdlearn(result)
sens.rdlearn(result, M = 1, cost = c(0, 1, 2, 4))


result <- rdlearn(y = "acces", x = "saber11", c = "cutoff", groupname = "department",
                  data = colombia_acces, fold = 20, M = c(0,1,2,4), cost = 0)
plot.rdlearn(result)
# Figure 2
# use "plot.rdlearn" to visualize the result.

sens.rdlearn(result, M=1, cost=c(0, 0.2, 0.4, 0.6, 0.8, 1))
sens.rdlearn(result, M=1, cost=c(0, 1))
# Figure 3
# "sens.rdlearn" is for sensitivity analysis. The output is a plot.
# This function inherits the cross-fitting data.

result_simA <- rdlearn(y = "out", x = "run", c = "cut",
                       data = simdata_A, fold = 10, M=1, cost=0)
plot.rdlearn(result_simA)
# This worked with the simulation data in the appendix.
