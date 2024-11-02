# prepare_data <- function(y,
#                          x,
#                          c,
#                          data,
#                          group_name,
#                          fold) {
#   # Prepare variables
#   Y <- data[[y]]
#   X <- data[[x]]
#   C <- data[[c]]
#
#   # Sort cutoffs from min to max
#   # Group index, from min cutoff to max cutoff
#   # Treatment indicator
#   c.vec <- sort(unique(C))
#   G <- match(C, c.vec)
#   D <- as.numeric(X >= C)
#
#   # Sample size and number of groups
#   n <- length(Y)
#   q <- length(unique(C))
#
#   # Define group names
#   if (is.null(group_name)) {
#     group_name_list <- paste0("Group", 1:q)
#   } else {
#     grouplist <- data[[group_name]]
#     dict <- setNames(grouplist, C)
#     group_name_list <- sapply(c.vec, function(x) dict[[as.character(x)]])
#   }
#
#   # Add fold_id to data for cross-fitting
#   data_all <- data.frame(Y = Y, X = X, C = C, D = D, G = G) %>%
#     dplyr::mutate(fold_id = sample(1:fold, size = n, replace = TRUE)) %>%
#     arrange(fold_id)
#
#   return(list(data_all = data_all, group_names = group_name_list))
# }
