# test_hello_world
require(dplyr)
require(tidyr)
require(readr)
require(graphics)
require(maps)

test_that("make_filename works", {
  expect_equal(make_filename("2016"), "accident_2016.csv.bz2")
})
