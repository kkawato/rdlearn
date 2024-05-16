#' @export
plot <- function(x, ...) UseMethod("plot")

#' Plot Cutoff Changes for rdlearn Objects
#'
#' This function plots the changes in cutoff values relative to the baseline cutoffs
#' for each group, under different combinations of the smoothness
#' multiplier (M) and the cost of treatment (C).
#'
#' @param result An object of class \code{rdlearn} returned by the \code{\link{rdlearn}} function.
#' @param xlab A character string specifying the label for the x-axis.
#' @param ylab A character string specifying the label for the y-axis.
#'
#' @return A \code{ggplot2} object representing the plot of cutoff changes.
#'
#' @import ggplot2
#'
#' @examples
#' result <- rdlearn(y = "elig", x = "saber11", c = "cutoff",
#'                   group_name = "department", data = acces,
#'                   fold = 20, M = c(0, 1), cost = 0)
#' plot(result)
#'
#' @export
plot.rdlearn <- function(result, xlab = "", ylab = "") {
  ...
}
plot.rdlearn <- function(result,
                         xlab = "",
                         ylab = ""
                         )
{
  var_names <- result$var_names
  y <- var_names$outcome
  x <- var_names$run_var
  c <- var_names$cutoff
  n <- result$sample
  q <- result$num_group
  org_cut <- result$org_cut
  safe_cut <- select(result$safe_cut, -group)

  plotdata <- data.frame(
    y_axis = rep(rev(1:q), ncol(safe_cut)),
    org_cut = rep(org_cut, ncol(safe_cut)),
    safe_cut = unlist(safe_cut),
    type = rep(names(safe_cut), each = q)
  )

  plot <- ggplot(data = plotdata, aes(type, y_axis)) +
    geom_tile(aes(fill = safe_cut - org_cut), color = "white") +
    scale_fill_gradient2(
      low = "purple",
      mid = "white",
      high = "orange",
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
    xlab(xlab) +
    ylab(ylab) +
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

  plot + labs(caption = paste0("Outcome: ", y, "; Running Variable: ", x, "; Cutoff: ", c, "   ",
                               "Sample Size: ", n, "; Number of Groups: ", q))
}
