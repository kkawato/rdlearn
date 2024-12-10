test_that("rdlearn check NA", {
  # -------------------------------------------------------------------------- #
  # 　"acces" data with half sample size for testing
  # -------------------------------------------------------------------------- #
  c.vec <- sort(unique(acces_half$C))
  q <- length(unique(acces_half$C))
  n <- nrow(acces_half)
  fold <- 2
  trace <- TRUE
  M <- 1
  cost <- 0

  acces_half <- acces_half %>%
    dplyr::mutate(fold_id = sample(1:fold, size = n, replace = TRUE)) %>%
    arrange(fold_id)

  cross_fit_output <- crossfit(
    c.vec = c.vec,
    q = q,
    fold = fold,
    data_all = acces_half,
    trace = trace
  )

  dif_lip_output <- estimate_dif_lip(
    cross_fit_output = cross_fit_output,
    q = q,
    c.vec = c.vec,
    trace = trace
  )

  group_name_vec <- character(q)
  for (k in 1:q) {
    group_name_vec[k] <- paste0("Group", k)
  }

  safelearn_out <- safelearn(
    c.vec = c.vec,
    n = n,
    q = q,
    cost = cost,
    M = M,
    group_name_vec = group_name_vec,
    dif_lip_output = dif_lip_output,
    cross_fit_output = cross_fit_output,
    trace = trace
  )

  # -------------------------------------------------------------------------- #
  # test
  # -------------------------------------------------------------------------- #

  # -------------------------------------------------------------------------- #
  # crossfit
  all_cols <- colnames(cross_fit_output)

  expected_cols <- c("G", "Y", "X", "C", "D", "fold_id", "mu.m", "mu.aug")
  for (col in expected_cols) {
    expect_true(col %in% all_cols, info = paste("Missing column:", col))
  }

  expected_cols <- c("mu.m", "mu.aug")
  for (col in expected_cols) {
    expect_true(col %in% all_cols, info = paste("Missing column:", col))
  }

  pseudo_ps_cols <- paste0("pseudo.ps", 1:q)
  for (col in pseudo_ps_cols) {
    expect_true(col %in% all_cols, info = paste("Missing pseudo.ps column:", col))
  }

  pseudo_cols <- paste0("pseudo.", 1:q)
  for (col in pseudo_cols) {
    expect_true(col %in% all_cols, info = paste("Missing pseudo. column:", col))
  }

  all_cols <- colnames(cross_fit_output)
  for (col in all_cols) {
    expect_true(any(!is.na(cross_fit_output[[col]])), info = paste("All values in column", col, "are NA"))
  }

  # -------------------------------------------------------------------------- #
  # estimate_dif_lip

  expected_elements <- c("dif_0", "dif_1", "Lip_0", "Lip_1")
  for (el in expected_elements) {
    expect_true(el %in% names(dif_lip_output), info = paste("Missing element:", el))
  }

  for (el in expected_elements) {
    expect_false(any(is.na(dif_lip_output[[el]])), info = paste(el, "contains NA values"))
  }

  # -------------------------------------------------------------------------- #
  # safelearn

  expected_elements <- c("safe_cut", "dif_cut", "temp_reg_df")
  for (el in expected_elements) {
    expect_true(el %in% names(safelearn_out), info = paste("Missing element:", el))
  }
  expect_false(all(is.na(safelearn_out$safe_cut)), info = "All values in safe_cut are NA")
})
