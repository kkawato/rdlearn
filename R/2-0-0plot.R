#' Plot Cutoff Changes for rdlearn Objects
#'
#' This function plots the changes in cutoff values relative to the baseline cutoffs
#' for each group, under different combinations of the smoothness
#' multiplier (M) and the cost of treatment (C).
#'
#' @param result An object of class \code{rdlearn} returned by the \code{\link{rdlearn}} function.
#' @param opt "safe"と指定することによって、derived safe cutoffsとoriginal cutoffsを表示する。
#' "dif"と指定することによって、change in cutoffsを表示する。
#'
#' @return A \code{ggplot2} plot which also contains the distance measure between original cutoffs and safe cutoffs.
#'
#' @import ggplot2
#'
#' @rdname plot.rdlearn
#' @examples
#' \dontrun{
#' # Load example data
#' data(acces)
#' library(rdlearn)
#' library(nprobust)
#' library(nnet)
#' library(ggplot2)
#' library(dplyr)
#' library(glue)
#' library(purrr)
#' library(tidyr)
#'
#' result <- rdlearn(y = "elig", x = "saber11", c = "cutoff",
#'                   group_name = "department", data = acces,
#'                   fold = 20, M = c(0, 1), cost = 0)
#' plot(result)
#' }
#'
#' @export
plot <- function(result, opt){
  if (!inherits(result, "rdlearn")) {
    stop("The 'result' argument must be an object of class 'rdlearn'.")
  }
  if (missing(opt) || !opt %in% c("safe", "dif")) {
    stop("Please specify 'opt' as 'safe' or 'dif'.")
  }

  var_names <- result$var_names
  y <- var_names$outcome
  x <- var_names$run_var
  c <- var_names$cutoff
  n <- result$sample
  q <- result$num_group
  org_cut <- result$org_cut
  safe_cut <- select(result$safe_cut, -group)
  l2norm <- result$l2norm

  if (opt == "safe") {
   extended_safe_cut <- cbind(org_cut, safe_cut)
   colnames(extended_safe_cut)[1] <- "original"

   plotdata <- data.frame(
     y_axis = rep(rev(1:q), ncol(extended_safe_cut)),
     org_cut = rep(org_cut, ncol(extended_safe_cut)),
     safe_cut = unlist(extended_safe_cut),
     type = rep(names(extended_safe_cut), each = q)
   )

   plot <- ggplot(data = plotdata, aes(type, y_axis)) +
     geom_tile(aes(fill = safe_cut - org_cut), color = "black") +
     scale_fill_gradient2(
       low = "purple",
       mid = "white",
       high = "orange",
       midpoint = 0,
       name = "Change in cutoff"
     ) +
     geom_text(
       aes(label = safe_cut),
       color = "black",
       size = 3,
       position = position_dodge(width = 1)
     ) +
     scale_y_continuous(
       breaks = seq(1, q, 1),
       labels = rev(result$safe_cut$group)
     ) +
     xlab("") +
     ylab("") +
     theme(
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.background = element_blank(),
       axis.text = element_text(size = 10, hjust = 0.5),
       axis.title = element_text(size = 14, face = "bold"),
       axis.line = element_blank(),
       axis.ticks = element_blank(),
       axis.ticks.length = unit(0, "cm"),
       plot.caption = element_text(hjust = 0, size = 8, face = "plain")
     )
 }
  if (opt == "dif") {
   plotdata <- data.frame(
     y_axis = rep(rev(1:q), ncol(safe_cut)),
     org_cut = rep(org_cut, ncol(safe_cut)),
     safe_cut = unlist(safe_cut),
     type = rep(names(safe_cut), each = q)
   )

    plot <- ggplot(data = plotdata, aes(type, y_axis)) +
      geom_tile(aes(fill = safe_cut - org_cut), color = "black") +
      scale_fill_gradient2(
        low = "purple",
        mid = "white",
        high = "orange",
        midpoint = 0,
        name = "Change in cutoff"
      ) +
      geom_text(
        aes(label = safe_cut - org_cut),
        color = "black",
        size = 3,
        position = position_dodge(width = 1)
      ) +
      scale_y_continuous(
        breaks = seq(1, q, 1),
        labels = rev(result$safe_cut$group)
      ) +
      xlab("") +
      ylab("") +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 10, hjust = 0.5),
        axis.title = element_text(size = 14, face = "bold"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        plot.caption = element_text(hjust = 0, size = 8, face = "plain")
      )
 }
  plot + labs(caption = paste0("Outcome: ", y, "; Running Variable: ", x, "; Cutoff: ", c, "   ",
                               "Sample Size: ", n, "; Number of Groups: ", q, "\n",
                               "L2 Norms: ", paste(names(l2norm), round(l2norm, 2), sep = "=", collapse = ", ")))
}


