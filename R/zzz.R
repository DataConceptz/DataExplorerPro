# DataExplorerPro - Package Startup
# AI-Powered Data Exploration Studio for RStudio

.onLoad <- function(libname, pkgname) {
  # Initialize gargoyle for reactive events
  gargoyle::init("data_loaded", "chart_created", "query_processed", "report_generated")
}

#' DataExplorerPro: AI-Powered Data Exploration Studio
#'
#' A comprehensive RStudio add-in for interactive data exploration,
#' automatic EDA reports, and natural language queries using Ollama.
#'
#' @section Available functions:
#' \code{\link{run_app}} - Launch the DataExplorerPro interface
#' \code{\link{generate_eda_report}} - Generate comprehensive EDA report
#' \code{\link{query_data_natural_language}} - Query data using natural language
#' \code{\link{ollama_chat}} - Chat with Ollama about your data
#'
#' @docType package
#' @name DataExplorerPro
NULL