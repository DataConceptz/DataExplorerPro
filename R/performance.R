# DataExplorerPro - Performance Helpers
# Utilities for sampling large datasets and basic profiling

#' Get data for analysis with sampling fallback
#'
#' If data is larger than threshold and auto_sample is TRUE, returns a sampled
#' subset for faster analysis. Otherwise, returns original data.
#'
#' @param data Data frame
#' @param threshold Number of rows above which sampling is applied
#' @param sample_size Number of rows to sample when sampling is applied
#' @param auto_sample Logical, whether to apply sampling automatically
#' @return A list with: data (data frame to use), sampled (logical), sample_n (int)
#' @export
get_analysis_data <- function(data, threshold = 50000, sample_size = 50000, auto_sample = TRUE) {
  if (!is.data.frame(data)) stop("data must be a data frame")
  n <- nrow(data)
  if (auto_sample && n > threshold) {
    sample_n <- min(sample_size, n)
    sampled_idx <- sample.int(n, sample_n)
    return(list(data = data[sampled_idx, , drop = FALSE], sampled = TRUE, sample_n = sample_n, original_n = n))
  }
  list(data = data, sampled = FALSE, sample_n = n, original_n = n)
}

#' Simple profiler wrapper
#'
#' Runs expr and returns elapsed seconds with result
#'
#' @param expr Expression to evaluate
#' @return List with result and time (elapsed seconds)
#' @export
profile_expr <- function(expr) {
  start <- proc.time()
  res <- eval.parent(substitute(expr))
  elapsed <- (proc.time() - start)[[3]]
  list(result = res, time = elapsed)
}
