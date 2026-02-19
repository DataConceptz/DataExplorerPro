library(testthat)
source('../../R/performance.R')

context('performance')

test_that('get_analysis_data returns sampled data when over threshold', {
  df <- data.frame(x = 1:1000)
  res <- get_analysis_data(df, threshold = 100, sample_size = 50, auto_sample = TRUE)
  expect_true(res$sampled)
  expect_equal(nrow(res$data), 50)
})

test_that('get_analysis_data returns full data when below threshold', {
  df <- data.frame(x = 1:50)
  res <- get_analysis_data(df, threshold = 100, sample_size = 20, auto_sample = TRUE)
  expect_false(res$sampled)
  expect_equal(nrow(res$data), 50)
})

test_that('profile_expr measures time and returns result', {
  res <- profile_expr({ Sys.sleep(0.1); 42 })
  expect_equal(res$result, 42)
  expect_true(res$time >= 0.09)
})
