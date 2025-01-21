#' Plot Cutoff Changes for rdlearn Objects
#'
#' This function plots the changes in cutoff values relative to the baseline
#' cutoffs for each group, under different combinations of the smoothness
#' multiplier (M) and the cost of treatment (C).
#'
#' @param x An object of class \code{rdlearn} returned by the
#'   \code{\link{rdlearn}} function.
#' @param opt When set to "safe", it displays the derived safe cutoffs and the
#'   original cutoffs. When set to "dif", it displays the change in cutoffs.
#' @param ... additional arguments.
#'
#' @return A \code{ggplot2} plot which also contains the distance measure
#'   between original cutoffs and safe cutoffs.
#'
#' @import ggplot2
#' @inherit package_rdlearn examples
#' @export
plot <- function(x, opt, ...) {
  UseMethod("plot")
}

#' @export
plot.rdlearn <- function(x, opt, ...) {
  if (!inherits(x, "rdlearn")) {
    stop("The 'result' argument must be an object of class 'rdlearn'.")
  }
  if (missing(opt) || !opt %in% c("safe", "dif")) {
    stop("Please specify 'opt' as 'safe' or 'dif'.")
  }

  var_names <- x$var_names
  y <- var_names$outcome
  runvar <- var_names$run_var
  c <- var_names$cutoff
  n <- x$sample
  q <- x$num_group
  org_cut <- x$org_cut
  safe_cut <- x$safe_cut
  group_name <- x$group_name

  if (opt == "safe") {
    extended_safe_cut <- cbind(org_cut, safe_cut)
    colnames(extended_safe_cut)[1] <- "original"

    plotdata <- data.frame(
      y_axis = rep(rev(1:q), ncol(extended_safe_cut)),
      org_cut = rep(org_cut, ncol(extended_safe_cut)),
      safe_cut = unlist(extended_safe_cut),
      type = rep(colnames(extended_safe_cut), each = q)
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
        labels = rev(group_name)
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
      type = rep(colnames(safe_cut), each = q)
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
        labels = rev(group_name)
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
  plot + labs(caption = paste0(
    "Outcome: ", y, "; Running Variable: ", runvar, "; Cutoff: ", c, "   ",
    "Sample Size: ", n, "; Number of Groups: ", q
  ))
}
