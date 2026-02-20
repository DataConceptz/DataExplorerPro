# Tests for DataExplorerPro

test_that("check_ollama_connection returns logical", {
  result <- check_ollama_connection()
  expect_type(result, "logical")
})

test_that("summarize_data works with data frame", {
  test_data <- data.frame(
    x = 1:10,
    y = letters[1:10],
    z = c(rep(TRUE, 5), rep(FALSE, 5))
  )
  
  result <- summarize_data(test_data)
  expect_equal(result$dimensions$rows, 10)
  expect_equal(result$dimensions$columns, 3)
})

test_that("detect_outliers works", {
  test_data <- data.frame(value = c(1, 2, 3, 4, 5, 100))
  result <- detect_outliers(test_data, "value")
  expect_equal(result$n_outliers, 1)
  expect_equal(result$outlier_values, 100)
})

test_that("analyze_correlations works", {
  test_data <- data.frame(
    a = 1:10,
    b = 1:10 * 2,
    c = 10:1
  )
  result <- analyze_correlations(test_data)
  expect_true(is.matrix(result$correlation_matrix))
  expect_equal(dim(result$correlation_matrix), c(3, 3))
})

test_that("ollama_chat returns character", {
  skip_if_not(check_ollama_connection())
  result <- ollama_chat("What is 2+2?")
  expect_type(result, "character")
})

test_that("query_data_natural_language returns list", {
  test_data <- data.frame(x = 1:10, y = 1:10)
  result <- query_data_natural_language("Show me the data", test_data)
  expect_type(result, "list")
  expect_true(all(c("code", "explanation", "insights") %in% names(result)))
})

test_that("generate_eda_report works", {
  test_data <- data.frame(x = 1:10, y = 1:10)
  expect_error(generate_eda_report(test_data, open_browser = FALSE))
})

test_that("run_app function exists", {
  expect_true(exists("run_app"))
  expect_true(exists("addin"))
})

test_that("visualization functions exist", {
  expect_true(exists("create_scatter_plot"))
  expect_true(exists("create_histogram"))
  expect_true(exists("create_box_plot"))
  expect_true(exists("create_heatmap"))
})

test_that("package exports are correct", {
  exports <- c("addin", "run_app", "generate_eda_report", 
               "query_data_natural_language", "ollama_chat",
               "check_ollama_connection", "list_ollama_models",
               "pull_ollama_model", "detect_outliers",
               "analyze_correlations", "summarize_data")
  
  for (fn in exports) {
    expect_true(exists(fn), info = paste(fn, "not found"))
  }
})