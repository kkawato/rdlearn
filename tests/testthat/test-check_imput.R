test_that("check_input validates correct input", {
  expect_silent(
    check_input(
      y = "elig",
      x = "saber11",
      c = "cutoff",
      data = acces,
      M = 1,
      cost = 0,
      fold = 10,
      var_names = list(outcome = "elig", run_var = "saber11", cutoff = "cutoff"),
      trace = TRUE
    )
  )
})

test_that("check_input fails with missing or incorrect 'y'", {
  expect_error(check_input(
    x = "saber11",
    c = "cutoff",
    data = acces,
    M = 1,
    cost = 0,
    var_names = list(outcome = "elig", run_var = "saber11", cutoff = "cutoff"),
    trace = TRUE
  ), "'y' must be a character string of length one.")

  expect_error(check_input(
    y = c("outcome1", "outcome2"),
    x = "saber11",
    c = "cutoff",
    data = acces,
    M = 1,
    cost = 0,
    var_names = list(outcome = "elig", run_var = "saber11", cutoff = "cutoff"),
    trace = TRUE
  ), "'y' must be a character string of length one.")

  expect_error(check_input(
    y = 1,
    x = "saber11",
    c = "cutoff",
    data = acces,
    M = 1,
    cost = 0,
    var_names = list(outcome = "elig", run_var = "saber11", cutoff = "cutoff"),
    trace = TRUE
  ), "'y' must be a character string of length one.")
})

test_that("check_input fails when variables are missing in 'data'", {
  acces_modified <- acces %>% select(-saber11)
  expect_error(check_input(
    y = "elig",
    x = "saber11",
    c = "cutoff",
    data = acces_modified,
    M = 1,
    cost = 0,
    var_names = list(outcome = "elig", run_var = "saber11", cutoff = "cutoff"),
    trace = TRUE
  ), "all variables must be in 'data'.")
})

test_that("check_input fails when there are NA values in the columns", {
  acces_modified <- acces
  acces_modified$elig[1] <- NA
  expect_error(check_input(
    y = "elig",
    x = "saber11",
    c = "cutoff",
    data = acces_modified,
    M = 1,
    cost = 0,
    var_names = list(outcome = "elig", run_var = "saber11", cutoff = "cutoff"),
    trace = TRUE
  ), "the column 'y' contains NA.")
})

test_that("check_input fails when both M and cost are vectors", {
  expect_error(check_input(
    y = "elig",
    x = "saber11",
    c = "cutoff",
    data = acces,
    M = c(1, 0),
    cost = c(1, 0),
    var_names = list(outcome = "elig", run_var = "saber11", cutoff = "cutoff"),
    trace = TRUE
  ), "Either M or cost must be a scalar.")
})

test_that("check_input fails when trace is not logical", {
  expect_error(check_input(
    y = "elig",
    x = "saber11",
    c = "cutoff",
    data = acces,
    M = 1,
    cost = 0,
    var_names = list(outcome = "elig", run_var = "saber11", cutoff = "cutoff"),
    trace = "TRUE"
  ), "trace must be TRUE or FALSE.")
})

test_that("check_input fails when fold is less than 2", {
  expect_error(check_input(
    y = "elig",
    x = "saber11",
    c = "cutoff",
    data = acces,
    M = 1,
    cost = 0,
    fold = 1,
    var_names = list(outcome = "elig", run_var = "saber11", cutoff = "cutoff"),
    trace = TRUE
  ), "fold must be an integer of 2 or greater.")
})
