#' @export
plot <- function(x, ...) UseMethod("plot")

#'
#' Plot method for \code{rdlearn} objects
#'
#' \code{plot} plots the cutoff change relative to the baseline cutoff for each department (y-axis)
#' under different smoothness multiplicative factor M and cost of treatment C (x-axis).
#'
#' @param result an object of class \code{rdlearn} returned by the \code{\link{rdlearn}}.
#' @param xlab a label of x-axis.
#' @param ylab a label of y-axis.
#'
#' @return a \code{ggplot2} plot of changes in cutoffs.
#'
#' @import ggplot2
#'
#' @examples
#' @export
plot.rdlearn <- function(result,
                         xlab = "",
                         ylab = ""
                         )
{
  safecut <- result$safecut
  q <- result$numgroup
  dataall <- data.frame()

  for (k in 1:(ncol(safecut) - 1)) {
    tempdf <- data.frame(
      y = rev(1:q),
      org.c = result$basecut,
      safe.c = safecut[k + 1],
      type = names(safecut)[k + 1]
    )
    names(tempdf)[3] <- "safe.c"
    dataall <- rbind(dataall, tempdf)
  }

  ggplot(data = dataall, aes(type, y)) +
    geom_tile(aes(fill = safe.c - org.c), color = "white") +
    scale_fill_gradient2(
      low = "purple",
      mid = "white",
      high = "orange",
      name = "Change in cutoff"
    ) +
    geom_text(
      aes(label = safe.c - org.c),
      color = "black",
      size = 3,
      position = position_dodge(width = 1)
    ) +
    scale_y_continuous(
      breaks = seq(1, q, 1),
      labels = rev(result$groupname)
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
