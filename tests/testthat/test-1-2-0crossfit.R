test_that("crossfit", {
  set.seed(12345)
  data(acces2)

  # -------------------------------------------------------------------------- #
  # arguments
  # -------------------------------------------------------------------------- #
  y = "elig"
  x = "saber11"
  c = "cutoff"
  group_name = "department"
  data = acces2
  fold = 20
  M = c(0,1,2,4)
  cost = 0
  trace = TRUE
　
  # -------------------------------------------------------------------------- #
  # prepare data
  # -------------------------------------------------------------------------- #
  Y <- data[[y]]
  X <- data[[x]]
  C <- data[[c]]
  c.vec <- sort(unique(C))
  G <- match(C, c.vec)
  D <- as.numeric(X >= C)
  n <- length(Y)
  q <- length(unique(C))

  # When group_name is not provided, assign a new name "Group k"
  if (is.null(group_name)) {
    group_name_list <- character(q)
    for (k in 1:q) {
      group_name_list[k] <- paste0("Group", k)
    }
  } else {
    grouplist <- data[[group_name]]
    dict <- setNames(grouplist, C)
    group_name_list <- sapply(c.vec, function(x) dict[[as.character(x)]])
  }

  # Add fold_id to data used for cross-fitting
  data_all <- data.frame(Y = Y, X = X, C = C, D = D, G = G) %>%
    dplyr::mutate(fold_id = sample(1:fold, size = n, replace = TRUE)) %>%
    arrange(fold_id)

  # -------------------------------------------------------------------------- #
  # crossfit
  # -------------------------------------------------------------------------- #

  cross_fit_output <- crossfit(
    c.vec = c.vec,
    q = q,
    fold = fold,
    data_all = data_all,
    trace = trace
  )

  # -------------------------------------------------------------------------- #
  # test
  # -------------------------------------------------------------------------- #

  data(crossfit_test)
  # pseudo.ps
  for (i in 1:23) {
    expect_true(all.equal(cross_fit_output[[paste0("pseudo.ps", i)]], crossfit_test[[paste0("pseudo.ps", i)]], check.names = FALSE))
  }
  # pseudo
  for (i in 1:23) {
    expect_true(all.equal(cross_fit_output[[paste0("pseudo.", i)]], crossfit_test[[paste0("pseudo.", i)]], check.names = FALSE))
  }
  # mu.m and mu.aug
  expect_true(all.equal(cross_fit_output$mu.m, crossfit_test$mu.m, check.names = FALSE))
  expect_true(all.equal(cross_fit_output$mu.aug, crossfit_test$mu.aug, check.names = FALSE))
})
