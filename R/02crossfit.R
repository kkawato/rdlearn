crossfit <- function(
  c.vec,
  q,
  fold,
  data_split,
  data_all)
{
  ################################################################################
  # please refer to
  # Appendix A.2. A double robust estimator for heterogeneous cross-group differences
  # Section 4.1. Doubly robust estimation
  # Section 4.3. Choosing the smoothness parameter
  ################################################################################
  mu.fit <- NULL
  Y <- data_all['Y']

  for(k in 1 : fold){
    data_train <- data_split %>% filter(fold_id!=k) %>% unnest(data) %>% ungroup() %>% select(-fold_id)
    data_test <- data_split %>% filter(fold_id==k) %>% unnest(data) %>% ungroup() %>% select(-fold_id)

    # conditional prob of group
    gamfit <- multinom(formula = G ~ X, data = data_train)

    for(g in seq(1,q,1)){
      # ====================================================================== #
      # Appendix A.2. Step 1. (a)
      # Constructing the estimates of group propensity score
      # ====================================================================== #

      # treatment group
      eval.dat1.p <- data_test %>% filter(D == 1)
      ps1 <- predict(gamfit, newdata = eval.dat1.p, "probs")

      if (is.null(dim(ps1)[1])) {
        data_all <- as.data.frame(data_all) # -----------------------------------------------------------------fix this later
        ps1 <- as.data.frame(ps1)
        data_all[data_all$fold_id == k & data_all$D == 1, paste0("pseudo.ps", seq(1, q, 1))] <- ps1
        data_all <- as_tibble(data_all)
      } else {
        data_all[data_all$fold_id == k & data_all$D == 1, paste0("pseudo.ps", seq(1, q, 1))] <- ps1
      }

      # control group
      eval.dat0.p <- data_test %>%
        filter(D == 0)

      ps0 <- predict(gamfit, newdata = eval.dat0.p, "probs")

      if (is.null(dim(ps0)[1])) {
        data_all <- as.data.frame(data_all) # -----------------------------------------------------------------fix this later
        ps0 <- as.data.frame(ps0)
        data_all[data_all$fold_id == k & data_all$D == 0, paste0("pseudo.ps", seq(1, q, 1))] <- ps0
        data_all <- as_tibble(data_all)
      } else {
        data_all[data_all$fold_id == k & data_all$D == 0, paste0("pseudo.ps", seq(1, q, 1))] <- ps0
      }

      # ====================================================================== #
      # Appendix A.2. Step 1. (b)
      # Constructing the estimates of the group-specific regression functions
      # treatment group
      # ====================================================================== #

      # m is for DR estimator (14) in Section 4.1.
      eval.dat1.m <- data_test %>%
        filter(X >= c.vec[g],
               X < c.vec[min(g + 1, q)],
               D == 0) %>% # G == min(g + 1, q) did not work
        pull(X)

      # aug is for DR estimator (14) in Section 4.1.
      eval.dat1.aug <- data_test %>%
        filter(X >= c.vec[g],
               X < c.vec[min(g + 1, q)],
               G == g) %>%
        pull(X)

      # pseudo is for Appendix A.2.
      eval.dat1.pseudo <- data_test %>%
        filter(D == 1,
               X >= c.vec[g]) %>%
        pull(X)

      eval.dat1.all <- c(eval.dat1.m, eval.dat1.aug, eval.dat1.pseudo)

      # local linear regression
      tryCatch({
        mu.fit1 <- lprobust(data_train$Y[data_train$D == 1 & data_train$G == g],
                            data_train$X[data_train$D == 1 & data_train$G == g],
                            eval = eval.dat1.all,
                            bwselect = "imse-dpi")$Estimate[, 5]
      }, error = function(e) return(0))



      pseudo1 <- mu.fit1[(length(eval.dat1.m) + length(eval.dat1.aug) + 1):length(eval.dat1.all)]

      tryCatch({
          data_all[data_all$fold_id == k &
                     data_all$D == 1 &
                     data_all$X >= c.vec[g],
                   paste0("pseudo.", g)] <- pseudo1
        }, error = function(e) return(0) )

      # ====================================================================== #
      # Section 4.1. Doubly robust estimation
      # ====================================================================== #

      # Section 4.1. Doubly robust estimation
      mu.m1 <- mu.fit1[1:length(eval.dat1.m)]
      mu.aug1 <- mu.fit1[(length(eval.dat1.m) + 1):(length(eval.dat1.m) + length(eval.dat1.aug))]

      tryCatch({
          data_all[data_all$fold_id == k &
                     data_all$X >= c.vec[g] &
                     data_all$X < c.vec[min(g + 1, q)] &
                     data_all$D == 0, # D == 0 # G == min(g + 1, q)
                   paste0("mu", ".m")] <- mu.m1
        }, error = function(e) return(0) )

      tryCatch({
          data_all[data_all$fold_id == k &
                     data_all$X >= c.vec[g] &
                     data_all$X < c.vec[min(g + 1, q)] &
                     data_all$G == g,
                   paste0("mu", ".aug")] <- mu.aug1
        }, error = function(e) return(0) )

      # ====================================================================== #
      # Appendix A.2. Step 1. (b)
      # Constructing the estimates of the group-specific regression functions
      # control group
      # ====================================================================== #

      # m is for DR estimator (14) in Section 4.1.
      eval.dat0.m <- data_test %>%
        filter(X >= c.vec[max(g - 1, 1)],
               X < c.vec[g],
               D == 1) %>% # G == max(g - 1, 1) did not work
        pull(X)

      # aug is for DR estimator (14) in Section 4.1.
      eval.dat0.aug <- data_test %>%
        filter(X >= c.vec[max(g - 1, 1)],
               X < c.vec[g],
               G == g) %>%
        pull(X)

      # pseudo is for Appendix A.2.
      eval.dat0.pseudo <- data_test %>%
        filter(D == 0,
               X < c.vec[g]) %>%
        pull(X)

      eval.dat0.all <- c(eval.dat0.m, eval.dat0.aug, eval.dat0.pseudo)

      # local linear regression
      tryCatch({
        mu.fit0 <- lprobust(data_train$Y[data_train$D == 0 & data_train$G == g],
                            data_train$X[data_train$D == 0 & data_train$G == g],
                            eval = eval.dat0.all,
                            bwselect = "imse-dpi")$Estimate[, 5]
      },error = function(e) return(0) )


      pseudo0 <- mu.fit0[(length(eval.dat0.m) + length(eval.dat0.aug) + 1):length(eval.dat0.all)]

      tryCatch({
        data_all[data_all$fold_id == k &
                   data_all$D == 0 &
                   data_all$X < c.vec[g],
                 paste0("pseudo.", g)] <- pseudo0
        },error = function(e) return(0) )

      # ====================================================================== #
      # Section 4.1. Doubly robust estimation
      # ====================================================================== #

      mu.m0 <- mu.fit0[1:length(eval.dat0.m)]
      mu.aug0 <- mu.fit0[(length(eval.dat0.m) + 1):(length(eval.dat0.m) + length(eval.dat0.aug))]

      tryCatch({
          data_all[data_all$fold_id == k &
                     data_all$X >= c.vec[max(g - 1, 1)] &
                     data_all$X < c.vec[g] &
                     data_all$D == 1, # D == 1, G == max(g - 1, 1) → なんでこれで動かないのか
                   paste0("mu", ".m")] <- mu.m0
        }, error = function(e) return(0) )

      tryCatch({
          data_all[data_all$fold_id == k &
                     data_all$X >= c.vec[max(g - 1, 1)] &
                     data_all$X < c.vec[g] &
                     data_all$G == g,
                   paste0("mu", ".aug")] <- mu.aug0
        }, error = function(e) return(0) )
    }
  }

  ##############################################################################
  # A.2. Step 2 Pseudo Outcome Regression
  # Constructing the pseudo outcome
  # Estimating cross-group differences dif
  # Choosing the value of smoothness parameter Lip
  ##############################################################################

  psd_dat1 <- psd_dat0 <- NULL  # storing the pseudo outcome (A.2. Step 2 Pseudo Outcome Regression)
  Lip_1 <- Lip_0 <- matrix(0, q, q)  # storing the value of smoothness parameter; 1/0: treatment/control
  dif.1m <- dif.0m <- matrix(0, nrow = q, ncol = q)  # storing the value of estimated cross-group differences at cutoff point

  for (g in seq(1, q - 1, 1)) {
    for (g.pr in seq(g + 1, q, 1)) {
      # ====================================================================== #
      # treatment group
      temp.dat <- data_all %>% filter(D == 1 & X >= c.vec[g.pr])

      # --------------- A.2. Step 2 Pseudo Outcome Regression - 1 --------------
      psout1 <- temp.dat[, paste0("pseudo.", g)] -
        temp.dat[, paste0("pseudo.", g.pr)] +
        with(temp.dat, I(G == g) *
               (Y - eval(parse(text = paste0("pseudo.", g)))) /
               eval(parse(text = paste0("pseudo.ps", g)))) -
        with(temp.dat, I(G == g.pr) *
               (Y - eval(parse(text = paste0("pseudo.", g.pr)))) /
               eval(parse(text = paste0("pseudo.ps", g.pr))))

      temp.vc <- data.frame(psout1, temp.dat$X, g, g.pr)
      names(temp.vc)[1:2] <- c("psout", "X")
      psd_dat1 <- rbind(psd_dat1, temp.vc)

      dif.1m[g, g.pr] <- lprobust(temp.vc[, "psout"],
                                  temp.vc[, "X"],
                                  eval = c.vec[g.pr],
                                  deriv = 0,
                                  p = 1,
                                  bwselect = "mse-dpi")$Estimate[, 5]

      # ---------------- Section 4.3 -------------------------------------------
      Lip_1[g, g.pr] <- abs(lprobust(temp.vc[, "psout"],
                                     temp.vc[, "X"],
                                     eval = c.vec[g.pr],
                                     deriv = 1,
                                     p = 2,
                                     bwselect = "mse-dpi")$Estimate[, 5])

      # ====================================================================== #
      # control group
      temp.dat <- data_all %>% filter(D == 0 & X < c.vec[g])

      # --------------- A.2. Step 2 Pseudo Outcome Regression - 1 --------------
      psout0 <- temp.dat[, paste0("pseudo.", g)] -
        temp.dat[, paste0("pseudo.", g.pr)] +
        with(temp.dat, I(G == g) * (Y - eval(parse(text = paste0("pseudo.", g)))) /
               eval(parse(text = paste0("pseudo.ps", g)))) -
        with(temp.dat, I(G == g.pr) * (Y - eval(parse(text = paste0("pseudo.", g.pr)))) /
               eval(parse(text = paste0("pseudo.ps", g.pr))))

      temp.vc <- data.frame(psout0, temp.dat$X, g, g.pr)
      names(temp.vc)[1:2] <- c("psout", "X")
      psd_dat0 <- rbind(psd_dat0, temp.vc)

      dif.0m[g, g.pr] <- lprobust(temp.vc[, "psout"],
                                  temp.vc[, "X"],
                                  eval = c.vec[g],
                                  deriv = 0,
                                  p = 1,
                                  bwselect = "mse-dpi")$Estimate[, 5]

      # ---------------- Section 4.3 -------------------------------------------
      Lip_0[g, g.pr] <- abs(lprobust(temp.vc[, "psout"],
                                     temp.vc[, "X"],
                                     eval = c.vec[g],
                                     deriv = 1,
                                     p = 2,
                                     bwselect = "mse-dpi")$Estimate[, 5])

    }
  }

  dif.1m <- dif.1m + t(-dif.1m)
  dif.0m <- dif.0m + t(-dif.0m)
  Lip_1 <- Lip_1 + t(Lip_1)
  Lip_0 <- Lip_0 + t(Lip_0)

  out <- list(
    data_all_temp = data_all,
    dif.1m_temp = dif.1m,
    dif.0m_temp = dif.0m,
    Lip_1_temp = Lip_1,
    Lip_0_temp = Lip_0
  )

  out
}

