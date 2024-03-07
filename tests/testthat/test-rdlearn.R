library(tidyverse)
library(nprobust)
library(nnet)
library(ggplot2)

# rdlearn(y = "acces", x = "saber11", c = "cutoff", groupname = "department", data = colombia_acces, fold = 20, M = c(1,2,4))
# rdlearn(y = "acces", x = "saber11", c = "cutoff", groupname = "department", data = colombia_acces, fold = 20, cost = c(0,0.2,0.4))

##########################################################################
### Empirical Data ######################################################
##########################################################################

# data(colombia_acces)
# result <- rdlearn(y="acces", x="saber11", c="cutoff", data = colombia_acces, fold = 20)
# result <- rdlearn(y="acces", x="saber11", c="cutoff", groupname = "department", data = colombia_acces, fold = 20)
# plot(result)
# plot.rdlearn(result)

# WHY??? -> it worked for some reason
# Probably the number of fold was small
# Error (test-rdlearn.R:6:1): (code run outside of `test_that()`)
# Error in `matrix(NA, n.V, o + 1)`: invalid 'nrow' value (too large or NA)
# Backtrace:
#   ▆
# 1. └─RDDPackage::rdlearn(...) at test-rdlearn.R:6:1
# 2.   └─nprobust::lprobust(...) at RDDPackage/R/rdlearn.R:141:7
# 3.     └─nprobust::lpbwselect(...)
# 4.       └─nprobust:::lpbwselect.imse.dpi(...)
# 5.         └─nprobust:::lpbwselect.mse.dpi(...)
# 6.           └─nprobust:::lprobust.bw(...)
# 7.             └─base::matrix(NA, n.V, o + 1)



##########################################################################
### Random Data ######################################################
##########################################################################
# input <- data.frame(
#   Run = sample(1:100, 1000, replace = TRUE),
#   Out = sample(1:100, 1000, replace = TRUE),
#   Cut = c(rep(seq(100, 1000, by = 100), each = 100))
# )
# result <- RDDPackage::rdlearn(y="Out",x="Run",c="Cut", data = input)
# ══ Results ════════════════════════════════════════════════════════════════
# ── Failed tests ───────────────────────────────────────────────────────────
# Error (test-rdlearn.R:11:1): (code run outside of `test_that()`)
# Error in `seq.default(x.min, x.max, length.out = imsegrid)`: 'from' must be a finite number
# Backtrace:
#   ▆
# 1. └─RDDPackage::rdlearn(y = "Out", x = "Run", c = "Cut", data = input) at test-rdlearn.R:11:1
# 2.   └─nprobust::lprobust(...) at RDDPackage/R/rdlearn.R:127:7
# 3.     └─nprobust::lpbwselect(...)
# 4.       └─nprobust:::lpbwselect.imse.dpi(...)
# 5.         ├─base::seq(x.min, x.max, length.out = imsegrid)
# 6.         └─base::seq.default(x.min, x.max, length.out = imsegrid)


##########################################################################
### Simulation Data (A) ##################################################
##########################################################################
set.seed(1234)

n <- 2000
sig=10
c0 = -850 ; c1 = -571
coef0=c(-1.992230e+00 ,-1.004582e-02 ,-1.203897e-05 ,-4.587072e-09)
coef1=c(9.584361e-01, 5.308251e-04 ,1.103375e-06 , 1.146033e-09 )
dif= 0.3

X = runif(n,-1000,-1)
G = as.numeric(I(0.01*X+rnorm(n,5,sig)>0)) #strong overlap
C = ifelse(G==1,c1,c0) ; D = as.numeric(X>=C) ; W = as.numeric(X<c1 & X>=c0)
#G: Group
#DはTreatmentを受けたかどうか
#Wは重ねっている区間に入っているかどうか

Px = poly(X - 735.4334 - c1 , degree=3 , raw=TRUE) #735.4334はまじでどっからきた？c1を代入すればAppendixと同じ数字になる
Px = cbind(rep(1,nrow(Px)),Px)　#定数項を含めている

#Px 行列と coef0、coef1 のベクトルの行列積を計算しています。これにより、それぞれの多項式の期待値が計算されます。
EY0 = Px%*%coef0
EY1 = Px%*%coef1

delta = 0.2

Y0 = EY0 + rnorm(n,sd=.3)
Y1 = EY1 + rnorm(n,sd=.3)

# 関数dがこのように書かれている - delta*(1-G) - exp(0.01*X)*(1-G) - dif *(1-G)*(1-D), dif = 0.3
Y = Y0*(1-D) + Y1*D - delta*(1-G) - exp(0.01*X)*(1-G) - dif *(1-G)*(1-D)
# G=0,D=1
# G=0,D=0
# G=1,D=1
# G=1,D=0

simdata_1 <- data.frame(
  X = X,
  C = C,
  Y = Y
)
colnames(simdata_1) <- c("run", "cut", "out")

simresult_1 <- rdlearn(y="out",x="run",c="cut", data = simdata_1, fold = 10)

##########################################################################
### Simulation Data (B) ##################################################
##########################################################################

sig=10
n <- 2000
c0 = -850 ; c1 = -571
coef0=c( -1.992230e+00 ,-1.004582e-02 ,-1.203897e-05 ,-4.587072e-09)
coef1=c(9.584361e-01, 5.308251e-04 ,1.103375e-06 , 1.146033e-09 )

X = runif(n,-1000,-1)
G= as.numeric(I(0.01*X+rnorm(n,5,sig)>0)) #strong overlap
C= ifelse(G==1,c1,c0) ; D = as.numeric(X>=C)  ; W = as.numeric(X<c1 & X>=c0)

Px = poly(X,degree=3,raw=TRUE)
Px = cbind(rep(1,nrow(Px)),Px)
EY0 = Px%*%coef0
EY1 = Px%*%coef1

delta = -0.2
Y0 = EY0 + rnorm(n,sd=.3)
Y1 = EY1 + rnorm(n,sd=.3)
Y = Y0*(1-D) + Y1*D + delta*(1-G) - exp(0.01*X)*(1-G) + 0.1*(1-G)*(1-D)

simdata_2 <- data.frame(
  X = X,
  C = C,
  Y = Y
)
colnames(simdata_2) <- c("run", "cut", "out")

simresult_2 <- rdlearn(y="out",x="run",c="cut", data = simdata_2, fold = 10)