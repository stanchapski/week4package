# test_hello_world

test_that("make_filename works", {
  expect_equal(make_filename("2016"), "accident_2016.csv.bz2")
})
