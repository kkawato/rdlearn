#' Estimating cross-group differences dif and calculating the smoothness
#' parameter Lip.
#'
#' Please refer to A.2. Step 2 Pseudo Outcome Regression and Section 4.3 of the
#' referenced source.
#'
#' @param cross_fit_output The output of the function \code{estimate_mu}.
#' @param q The total number of groups.
#' @param c.vec A vector of cutoff values for the continuous variable X.
#' @param trace A logical value that controls whether to display the progress of
#'   cross-fitting and regret calculation.
#' @return A list containing cross-group differences dif and the moothness
#'   parameter Lip for treatment group and control group
#' @importFrom nprobust lprobust
#' @keywords internal
#' @noRd
estimate_dif_lip <- function(
    cross_fit_output,
    q,
    c.vec,
    trace
) {
  dif0 <- matrix(0, nrow = q, ncol = q)
  dif1 <- matrix(0, nrow = q, ncol = q)
  Lip0 <- matrix(0, nrow = q, ncol = q)
  Lip1 <- matrix(0, nrow = q, ncol = q)

  for (d in c(1, 0)) {
    if (trace == TRUE){
      print(paste0("Estimatin dif and Lip for d = ", d))
    }
    dif <- matrix(0, nrow = q, ncol = q)
    Lip <- matrix(0, q, q)
    for (g in seq(1, q - 1, 1)) {
      for (g.pr in seq(g + 1, q, 1)) {
        if (d == 1) {
          temp.dat <- cross_fit_output %>% filter(D == 1 & X >= c.vec[g.pr])
        } else {
          temp.dat <- cross_fit_output %>% filter(D == 0 & X < c.vec[g])
        }

        psout <- temp.dat[, paste0("pseudo.", g)] - temp.dat[, paste0("pseudo.", g.pr)] +
          with(temp.dat, I(G == g) *
                 (Y - eval(parse(text = paste0("pseudo.", g)))) /
                 eval(parse(text = paste0("pseudo.ps", g)))) -
          with(temp.dat, I(G == g.pr) *
                 (Y - eval(parse(text = paste0("pseudo.", g.pr)))) /
                 eval(parse(text = paste0("pseudo.ps", g.pr))))

        temp.vc <- data.frame(psout, temp.dat$X, g, g.pr)
        names(temp.vc)[1:2] <- c("psout", "X")

        eval_point <- c.vec[g.pr] * (d == 1) + c.vec[g] * (d == 0)

        dif[g, g.pr] <- lprobust(temp.vc[, "psout"],
                                 temp.vc[, "X"],
                                 eval = eval_point,
                                 deriv = 0,
                                 p = 1,
                                 bwselect = "mse-dpi")$Estimate[, 5]

        Lip[g, g.pr] <- abs(lprobust(temp.vc[, "psout"],
                                     temp.vc[, "X"],
                                     eval = eval_point,
                                     deriv = 1,
                                     p = 2,
                                     bwselect = "mse-dpi")$Estimate[, 5])
      }
    }

    dif <- dif + t(-dif)
    Lip <- Lip + t(Lip)

    if (d == 0) {
      dif_0 <- dif
      Lip_0 <- Lip
    } else {
      dif_1 <- dif
      Lip_1 <- Lip
    }
  }

  out <- list(dif_0 = dif_0,
              dif_1 = dif_1,
              Lip_0 = Lip_0,
              Lip_1 = Lip_1)
  return(out)
}

