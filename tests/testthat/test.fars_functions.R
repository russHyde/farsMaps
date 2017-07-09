###############################################################################

context("Unit tests for the pure functions in fars_functions.R")

###############################################################################

test_that("make_filename tests", {
  expect_error(
    object = make_filename(),
    info   = "make_filename called without arguments."
  )
  expect_equal(
    object = make_filename(character(0)),
    expected = character(0),
    info   = "make_filename called with an empty vector should return empty."
  )
  expect_equal(
    object = make_filename(1984),
    expected = "accident_1984.csv.bz2",
    info = "make_filename called with a single integer argument"
  )
  expect_equal(
    object = make_filename("1984"),
    expected = "accident_1984.csv.bz2",
    info = "make_filename called with a single string argument"
  )
  expect_equal(
    object = make_filename(1981:1982),
    expected = c("accident_1981.csv.bz2", "accident_1982.csv.bz2"),
    info = "make_filename called with two years"
  )
})
