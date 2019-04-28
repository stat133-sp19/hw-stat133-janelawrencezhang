context("Tests for summary measures")

test_that(".aux_mean with valid  parameters",{
  expect_equal(.aux_mean(5,0.5),2.5)
  expect_equal(.aux_mean(5,0),0)
  expect_error(.aux_mean(-5,0.8))
  expect_error(.aux_mean(0,0.1))
  expect_error(.aux_mean(10,2))
  expect_error(.aux_mean(10,-0.6))
})


test_that(".aux_variance with valid  parameters",{
  expect_equal(.aux_variance(5,0.5),1.25)
  expect_equal(.aux_variance(5,0),0)
  expect_error(.aux_variance(-5,0.8))
  expect_error(.aux_variance(0,0.1))
  expect_error(.aux_variance(10,2))
  expect_error(.aux_variance(10,-0.6))
})


test_that(".aux_mode with valid  parameters",{
  expect_equal(.aux_mode(5,0.5),3)
  expect_equal(.aux_mode(5,0),0)
  expect_error(.aux_mode(-5,0.8))
  expect_error(.aux_mode(0,0.1))
  expect_error(.aux_mode(10,2))
  expect_error(.aux_mode(10,-0.6))
})

test_that(".aux_skewness with valid  parameters",{
  expect_equal(.aux_skewness(5,0.5),0)
  expect_equal(.aux_skewness(5,0),Inf)
  expect_error(.aux_skewness(-5,0.8))
  expect_error(.aux_skewness(0,0.1))
  expect_error(.aux_skewness(10,2))
  expect_error(.aux_skewness(10,-0.6))
})

test_that(".aux_kurtosis with valid  parameters",{
  expect_equal(.aux_kurtosis(5,0.5),-0.4)
  expect_equal(.aux_kurtosis(5,0),Inf)
  expect_error(.aux_kurtosis(-5,0.8))
  expect_error(.aux_kurtosis(0,0.1))
  expect_error(.aux_kurtosis(10,2))
  expect_error(.aux_kurtosis(10,-0.6))
})
