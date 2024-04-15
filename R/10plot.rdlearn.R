#ここもデータフレーム作るところはきれいに書きたいな
#早く説明文を書く

plot <- function(x, ...) UseMethod("plot")

#' plot.rdlearn
#'
#' @param result
#' @param xlab
#' @param ylab
#' @param safecut
#'
#' @return
#' @export
#'
#' @examples
#' result <- rdlearn(y = "acces", x = "saber11", c = "cutoff", groupname = "department", data = colombia_acces, fold = 20, M = c(0, 1), cost = 0)
#' plot.rdlearn(result)
#' @export

plot.rdlearn <- function(result, xlab = "", ylab = "", safecut = NULL) {
  if (is.null(safecut)) {
    safecut <- result$safecut
  } else {
    print("new safecut")
  }

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
