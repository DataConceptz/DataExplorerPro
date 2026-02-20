library(testthat)

library(DataExplorerPro)

context('network_viz')

test_that('create_correlation_network returns visNetwork for numeric data', {
  df <- mtcars
  net <- DataExplorerPro:::create_correlation_network(df, cor_threshold = 0.2)
  # visNetwork object is an htmlwidget
  expect_true(inherits(net, 'htmlwidget') || inherits(net, 'visNetwork'))
})

test_that('calculate_network_metrics returns data frame with degree', {
  df <- mtcars
  metrics <- DataExplorerPro:::calculate_network_metrics(df, cor_threshold = 0.2)
  expect_true(is.data.frame(metrics))
  expect_true('degree' %in% names(metrics))
})