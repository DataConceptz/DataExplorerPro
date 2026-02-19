library(testthat)

source('../../R/visualization_engine.R')

context('visualization_engine')

test_that('create_bar_chart handles numeric y and categorical x', {
  df <- data.frame(group = rep(c('a','b','c'), each = 5), val = rnorm(15, 10, 2))
  p <- create_bar_chart(df, 'group', 'val')
  expect_s3_class(p, 'plotly')
})

test_that('create_heatmap_plot aggregates counts', {
  df <- data.frame(x = rep(letters[1:3], each = 4), y = rep(LETTERS[1:4], 3))
  p <- create_heatmap(df, 'x', 'y')
  expect_s3_class(p, 'plotly')
})

test_that('create_scatter_plot returns plotly scatter', {
  df <- mtcars
  p <- create_scatter_plot(df, 'wt', 'mpg')
  expect_s3_class(p, 'plotly')
})