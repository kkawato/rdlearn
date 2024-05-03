# # # # ====================================================================== #
# # # # Section 4.2. Estimating the bounds
# # # # please refer to (16)
# # #
# # # lip_extra <- function(x.train,
# # #                       group,
# # #                       g,
# # #                       g.pr,
# # #                       temp_result) {
# # #
# # #     dif.1m <- temp_result$dif.1m_temp
# # #     dif.0m <- temp_result$dif.0m_temp
# # #     Lip_1 <- temp_result$Lip_1_temp
# # #     Lip_0 <- temp_result$Lip_0_temp
# # #     data_all <- temp_result$data_all_temp
# # #     Y <- data_all['Y']
# # #     X <- data_all['X']
# # #     C <- data_all['C']
# # #     G <- data_all['G']
# # #
# # #     if (group == "dif1") { # B1 G=1
# # #       d <- 1
# # #       Lip <- Lip_1[g, g.pr]
# # #       dif.m <- dif.1m[g, g.pr]
# # #       eval.main <- unique(C[G == max(g, g.pr)])
# # #     }
# # #
# # #     if (group == "dif0") { # B1 G=1
# # #       d <- 0
# # #       Lip <- Lip_0[g, g.pr]
# # #       dif.m <- dif.0m[g, g.pr]
# # #       eval.main <- unique(C[G == min(g, g.pr)])
# # #     }
# # #
# # #     upper <- map(x.train, function(x) min(1, min(dif.m + Lip * abs(x - eval.main))))
# # #     lower <- map(x.train, function(x) max(-1, max(dif.m - Lip * abs(x - eval.main))))
# # #     return(list(upper = upper, lower = lower))
# # #   }
# #
# #
# # regret <- NULL
# # for (c.alt in unique(X[X >= c.vec[1] & X < c.vec[q]])) {
# #   if (c.alt >= c.vec[g]) {
# #     # (14) Theta_DR: first term and second term
# #     data_temp1 <- data_mid %>% filter(G == g, X < c.alt, X >= c.vec[g])
# #     data_temp2 <- data_mid %>% filter(X < c.alt, X >= c.vec[g], X >= c.vec[ifelse(G == 1, 1, G - 1)], X < c.vec[G])
# #     cost <- tryCatch({ temp_cost * dim(data_mid[data_mid$X >= c.vec[g] & data_mid$X < c.alt & data_mid$G == g, "Y"])[1] / n }, error = function(e) return(0))
# #   } else {
# #     # (14) Theta_DR: first term and second term
# #     data_temp1 <- data_mid %>% filter(G == g, X >= c.alt, X < c.vec[g])
# #     data_temp2 <- data_mid %>% filter(X >= c.alt, X < c.vec[g], X >= c.vec[G], X < c.vec[ifelse(G == q, q, G + 1)])
# #     cost <- tryCatch({ temp_cost * dim(data_mid[data_mid$X < c.vec[g] & data_mid$X >= c.alt & data_mid$G == g, "Y"])[1] / n }, error = function(e) return(0))
# #   }
# #     base_regret <- sum(data_mid[data_mid$G == g, "Y"]) / n
# #     Iden_alt <- (sum(data_mid[data_mid$X >= c.vec[g] & data_mid$X >= c.alt & data_mid$G == g, "Y"]) +
# #                  sum(data_mid[data_mid$X < c.vec[g] & data_mid$X < c.alt & data_mid$G == g, "Y"])) / n
# #
# #     DR_1 <- sum(data_temp1[, "mu.m"]) / n
# #     DR_2 <- tryCatch(sum(with(data_temp2,
# #                               eval(parse(text = paste0("pseudo.ps", g))) /
# #                                 eval(parse(text = paste0("pseudo.ps", G))) *
# #                                 (Y - eval(parse(text = "mu.aug"))))) / n,
# #                      error = function(e) return(0))
# #
# #     Theta_2 <- tryCatch(sum(data_temp1[, paste0("d", 0)]) / n,
# #                         error = function(e) return(0))
# #     temp_reg <- (Iden_alt + DR_1 + DR_2 + Theta_2 + cost * (c.alt >= c.vec[g]) - cost * (c.alt < c.vec[g])) - base_regret
# #   }
#
# for (c.alt in unique(X[X >= c.vec[1] & X < c.vec[q]])) {
# if (c.alt >= c.vec[g]) {
#   base_regret <- sum(data_mid[data_mid$G == g, "Y"]) / n
#
#   # (12) I_iden: Identified
#   Iden_alt <- (sum(data_mid[data_mid$X >= c.vec[g] & data_mid$X >= c.alt & data_mid$G == g, "Y"]) +
#                  sum(data_mid[data_mid$X < c.vec[g] & data_mid$X < c.alt & data_mid$G == g, "Y"])) / n
#
#
#   # (14) Theta_DR: first term and second term
#   data_temp1 <- data_mid %>%
#     filter(G == g,
#            X < c.alt,
#            X >= c.vec[g])
#
#   DR_1 <- tryCatch(sum(data_temp1[, "mu.m"]) / n,
#                    error = function(e) return(0))
#
#   data_temp2 <- data_mid %>%
#     filter(X < c.alt,
#            X >= c.vec[g],
#            X >= c.vec[ifelse(G == 1, 1, G - 1)], # X >= c.vec[G-1]
#            X < c.vec[G])
#
#   DR_2 <- tryCatch(sum(with(data_temp2,
#                             eval(parse(text = paste0("pseudo.ps", g))) /
#                               eval(parse(text = paste0("pseudo.ps", G))) *
#                               (Y - eval(parse(text = "mu.aug"))))) / n,
#                    error = function(e) return(0))
#
#   # (15) Theta_2:
#   Theta_2 <- tryCatch(sum(data_temp1[, paste0("d", 0)]) / n,
#                       error = function(e) return(0))
#
#   # cost
#   cost <- tryCatch(temp_cost * dim(data_mid[data_mid$X >= c.vec[g]
#                                             & data_mid$X < c.alt
#                                             & data_mid$G == g, "Y"])[1] / n, # the number of
#                    error = function(e) return(0))
#
#   temp_reg <- (Iden_alt + DR_1 + DR_2 + Theta_2 + cost) - base_regret
# }
#
# # -------------------------------------------------------------------- #
# if (c.alt < c.vec[g]) {
#   base_regret <- sum(data_mid[data_mid$G == g, "Y"]) / n
#
#   # (12) I_iden: Identified
#   Iden_alt <- (sum(data_mid[data_mid$X >= c.vec[g] & data_mid$X >= c.alt & data_mid$G == g, "Y"]) +
#                  sum(data_mid[data_mid$X < c.vec[g] & data_mid$X < c.alt & data_mid$G == g, "Y"])) / n
#
#   # (14) Theta_DR: first term and second term
#   data_temp1 <- data_mid %>%
#     filter(G == g,
#            X >= c.alt,
#            X < c.vec[g])
#
#   DR_1 <- tryCatch(sum(data_temp1[, "mu.m"]) / n,
#                    error = function(e) return(0))
#
#   data_temp2 <- data_mid %>%
#     filter(X >= c.alt,
#            X < c.vec[g],
#            X >= c.vec[G],
#            X < c.vec[ifelse(G == q, q, G + 1)]) # X < c.vec[G + 1]
#
#   DR_2 <- tryCatch(sum(with(data_temp2,
#                             eval(parse(text = paste0("pseudo.ps", g))) /
#                               eval(parse(text = paste0("pseudo.ps", G))) *
#                               (Y - eval(parse(text = "mu.aug"))))) / n,
#                    error = function(e) return(0))
#
#   # (15) Theta_2
#   Theta_2 <- tryCatch(sum(data_temp1[, paste0("d", 1)]) / n,
#                       error = function(e) return(0))
#
#   # cost
#   cost <- tryCatch(temp_cost * dim(data_mid[data_mid$X < c.vec[g] & data_mid$X >= c.alt & data_mid$G == g, "Y"])[1] / n,
#                    error = function(e) return(0))
#
#   temp_reg <- (Iden_alt + DR_1 + DR_2 + Theta_2 - cost) - base_regret
# }
# # -------------------------------------------------------------------- #
# regret <- c(regret, temp_reg)
# }
#
#
#
