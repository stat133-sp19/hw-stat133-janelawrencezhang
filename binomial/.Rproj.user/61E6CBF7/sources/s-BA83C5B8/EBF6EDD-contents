context("Tests for checkers")

# library(testthat)
# test_file("../../R/binomial.R")

test_that(".check_prob with valid prob parameter",{
  expect_true(.check_prob(0.3))
  expect_true(.check_prob(1))
  expect_true(.check_prob(0))
  expect_error(.check_prob(2))
  expect_error(.check_prob(-3))

})

test_that(".check_trials with valid trials parameter",{
  expect_true(.check_trials(5))
  expect_true(.check_trials(10))
  expect_error(.check_trials(0))
  expect_error(.check_trials(-5))
  expect_error(.check_trials(2.5))

})


test_that(.check_success with valid success parameter and trials",{
  expect_true(.check_success(5,10))
  expect_true(.check_success(0,10))
  expect_error(.check_success(2.2,10))
  expect_error(.check_success(10,5))
  expect_error(.check_success(2,10.5))
})
