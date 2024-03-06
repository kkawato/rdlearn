#' plot.rdlearn
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
plot.rdlearn <- function(x){
  # x$call
  # x$variables
  # x$sample
  # x$basecut
  # x$optcut
  # x$changecut
  # x$M
  # x$cost
  # x$numgroup

  q <- x$numgroup

  dataall <- data.frame(y=rev(1:q),
                        org.c = x$basecut,
                        safe.c = x$safecut,
                        type = paste0("M=",x$M)) #ここもMがベクトルである場合に対応させなければいけない

  ggplot(data = dataall, aes(type, y)) +
    geom_tile(aes(fill = safe.c - org.c), color = "white") +
    scale_fill_gradient2(low = "purple", mid = "white", high = "orange",
                         name = "Change in cutoff"
                         ) +
                         # limits = c(-200, 200)) + # how can I scale this
    geom_text(aes(label = safe.c - org.c), color = "black", size = 3,
              position = position_dodge(width = 1)) +
    scale_y_continuous(breaks = seq(1, q, 1),
                      labels = rev(x$groupname)
                       ) +
    xlab("Smoothness factor") +
    ylab("") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.text = element_text(size = 10, hjust = 0.5),
      axis.title = element_text(size = 14, face = "bold"),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.ticks.length = unit(0, "cm")
    )
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
