test_that("learned cutoffs loop over lip(M)", {
  # -------------------------------------------------------------------------- #
  # esimate dif and lip
  # -------------------------------------------------------------------------- #

  set.seed(12345)
  result <- rdlearn(y = "elig", x = "saber11", c = "cutoff",
                    group_name = "department", data = acces2,
                    fold = 20, M = c(0, 1, 2, 4), cost = 0)

  # -------------------------------------------------------------------------- #
  # test
  # -------------------------------------------------------------------------- #

  # Setting tolerance to 5 in case where regrets have ties.
  data("safecut_M0124C0_test")
  expect_true(
    all.equal(
      as.numeric(as.matrix(result$safe_cut)),
      as.numeric(as.matrix(safecut_M0124C0_test)),
      check.names = FALSE,
      tolerance = 5
    )
  )

})
