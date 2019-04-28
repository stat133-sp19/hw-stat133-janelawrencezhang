context("Tests for binomial")

# library(testthat)
# test_file("../../R/binomial.R")


test_that("bin_choose with valid parameters",{
  expect_equal(bin_choose(5,0),1)
  expect_equal(bin_choose(5,2),10)
  expect_error(bin_choose(-5,2))
  expect_error(bin_choose(5,-2))
  expect_error(bin_choose(5,2.5))
  expect_error(bin_choose(15.5,2))
  expect_error(bin_choose(0,2))

})


test_that("bin_probability with valid parameters",{
  expect_is(bin_probability(0,5,0.3),"numeric")
  expect_length(bin_probability(3,10,0.2),1)
  expect_equal(bin_probability(2,5,0.5),0.3125)
  expect_error(bin_probability(5,2,0.5))
  expect_error(bin_probability(5,-2,0.5))
  expect_error(bin_probability(-5,2,0.5))
  expect_error(bin_probability(5,2,-0.5))
  expect_error(bin_probability(5,2.5,0.6))
  expect_error(bin_probability(15.5,20,0.8))
  expect_error(bin_probability(0,20,2))

})



test_that("bin_distribution with valid parameters and output",{
  expect_type(bin_distribution(10,0.6),"list")

  expect_length(bin_distribution(50,0.6),2)
  expect_error(bin_distribution(0,0.5))
  expect_error(bin_distribution(-5,0.5))
  expect_error(bin_distribution(5,2))
  expect_error(bin_distribution(15,-0.5))
})

test_that("bin_cumulative with valid parameters and output",{
  expect_type(bin_cumulative(10,0.6),"list")

  expect_length(bin_cumulative(50,0.6),3)
  expect_error(bin_cumulative(0,0.5))
  expect_error(bin_cumulative(-5,0.5))
  expect_error(bin_cumulative(5,2))
  expect_error(bin_cumulative(15,-0.5))

})

