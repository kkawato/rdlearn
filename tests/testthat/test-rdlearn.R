rm(list=ls())
set.seed(12345)
library(tidyverse)
library(nprobust)
library(nnet)
library(ggplot2)

result <- rdlearn(y = "elig", x = "saber11", c = "cutoff", groupname = "department", data = acces, fold = 20, M = c(0, 1), cost = 0)
plot(result)

result2 <- sens(result, M = 1, cost = c(0, 0.2, 0.4, 0.6, 0.8, 1))
plot(result2)

result <- rdlearn(y = "elig", x = "saber11", c = "cutoff", groupname = "department", data = acces, fold = 20, M = c(0, 1, 2, 4), cost = 0)
plot.rdlearn(result)
# Figure 2
# use "plot.rdlearn" to visualize the result.

result2 <- sens.rdlearn(result, M=1, cost=c(0, 0.2, 0.4, 0.6, 0.8, 1))
plot.rdlearn(result2)
# Figure 3
# "sens.rdlearn" is for sensitivity analysis. The output is a plot.
# This function inherits the cross-fitting data.

result_simA <- rdlearn(y = "out", x = "run", c = "cut", data = simdata_A, fold = 10, M=1, cost=0)
plot.rdlearn(result_simA)
# This also worked with the simulation data in the appendix.

################################################################################
acces
data <- acces

Y <- data[['elig']]
X <- data[['saber11']]
C <- data[['cutoff']]
groupname = "department"
fold<-20

# Sort cutoffs from min to max
c.vec <- sort(unique(C))
# Sample size
n <- length(Y)
# Number of groups
q <- length(unique(C))
# Group index, from min cutoff to max cutoff
G <- match(C, c.vec)
# Treatment indicator
D <- as.numeric(X >= C)

# When groupname is not provided, assign a new name "Group k"
if (is.null(groupname)) {
  groupname <- character(q)
  for (k in 1:q) {
    groupname[k] <- paste0("Group", k)
  }
} else {
  grouplist <- data[[groupname]]
  dict <- setNames(grouplist, C)
  groupname <- sapply(c.vec, function(x) dict[[as.character(x)]])
}

# Add fold_id to data used for cross-fitting
tempdata <- data.frame(Y = Y, X = X, C = C, D = D, G = G)

tempdata

### simple
data_all <- tempdata %>%
  mutate(fold_id = sample(1:fold, size = n, replace = TRUE))

data_split <- data_all %>%
  group_by(fold_id) %>%
  nest() %>%
  arrange(fold_id)

### previous one
data_split <- tempdata %>%
  mutate(
    fold_id = sample(1:fold, size = n, replace = TRUE)) %>%
  group_by(fold_id) %>%
  nest() %>%
  arrange(fold_id)

data_all <- data_split %>%
  unnest(data) %>%
  ungroup()

