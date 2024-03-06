plot.rdlearn <- function(x){

x$call
x$variables
x$sample
x$basecut
x$optcut
x$changecut
x$M
x$cost

}

# call = cl, #the matched call to the rdlearn function.
# variables = varnames, #outcome, running variable, cutoff, pretreatment covariates (for calculating propensity score)
# sample = n, #sample sizes withnin baseline cutoffs(like the left side of Table 1)
# # ps_model = #model for estimating propensity score
# # psout_model: #model for group specific regression
# # lip_model: #Any generic nonparametric regression methods
# # b_model: #Any generic nonparametric regression methods with the running variable as the only predictor to construct the DR pseudo outcome for the actual observed difference
# M = M, #multiplicative smoothness factor
# cost = cost, #cost for calculating regret
# #rdestimates: and standard error within conventional RD framework(like the right side of Table 1) # todo
#
# basecut = c.vec, #baseline cutoffs
# optcut = c.all, #learned optimal cutoffs
# changecut = c.vec - c.all, #change in cutoff (like the M=1 column in the Figure 2)
# regret = regret_sum #regret of optimal policy
