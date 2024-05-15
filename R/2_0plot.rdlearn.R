#' @export
plot <- function(x, ...) UseMethod("plot")

#' Plot method for \code{rdlearn} objects
#'
#' \code{plot} plots the cutoff change relative to the baseline cutoff for each department (y-axis)
#' under different smoothness multiplicative factor M and cost of treatment C (x-axis).
#'
#' @param result An object of class \code{rdlearn} returned by the \code{\link{rdlearn}}.
#' @param xlab A label of x-axis.
#' @param ylab A label of y-axis.
#' @return a \code{ggplot2} plot of changes in cutoffs.
#' @import ggplot2
#'
#' @examples
#' result <- rdlearn(y = "elig", x = "saber11", c = "cutoff", groupname = "department", data = acces, fold = 20, M = c(0, 1), cost = 0)
#' plot(result)
#' @export
plot.rdlearn <- function(result,
                         xlab = "",
                         ylab = ""
                         )
{
  org_cut <- result$org_cut
  safe_cut <- select(result$safe_cut, -group)
  q <- result$num_group

  plotdata <- data.frame(
    y_axis = rep(rev(1:q), ncol(safe_cut)),
    org_cut = rep(org_cut, ncol(safe_cut)),
    safe_cut = unlist(safe_cut),
    type = rep(names(safe_cut), each = q)
  )

  ggplot(data = plotdata, aes(type, y_axis)) +
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
      axis.ticks.length = unit(0, "cm")
    )
}
