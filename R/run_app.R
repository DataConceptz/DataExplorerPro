#' Launch DataExplorerPro Add-in
#'
#' Opens the interactive data exploration studio in a new RStudio viewer pane or window.
#'
#' @param port Port for Shiny server (default: chosen by Shiny)
#' @param host Host address (default: chosen by Shiny based on environment)
#' @param browser Whether to open in external browser (default: FALSE)
#' @param ... Additional arguments passed to shiny::runApp
#'
#' @return Invisibly returns NULL
#'
#' @examples
#' \dontrun{
#' # Launch the add-in
#' run_app()
#'
#' # Launch in external browser
#' run_app(browser = TRUE)
#' }
#'
#' @export
run_app <- function(port = getOption("shiny.port"), host = getOption("shiny.host", "0.0.0.0"), browser = FALSE, ...) {

  # Find the app directory
  app_dir <- system.file("app", package = "DataExplorerPro")

  # Fallback: development directory (when running from source)
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    dev_path <- file.path(getwd(), "inst", "app")
    if (dir.exists(dev_path)) app_dir <- dev_path
  }

  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("Could not find app directory. Please reinstall the package:\n",
         '  remotes::install_github("DataConceptz/DataExplorerPro")')
  }

  message("Loading app files from: ", app_dir)

  # Build runApp args — only pass port/host when explicitly provided
  run_args <- list(appDir = app_dir, launch.browser = browser, ...)
  if (!is.null(port)) run_args$port <- port
  if (!is.null(host)) run_args$host <- host

  do.call(shiny::runApp, run_args)

  return(invisible(NULL))
}

#' DataExplorerPro Add-in
#'
#' This function is called by RStudio when the add-in is invoked.
#' It launches the DataExplorerPro interface.
#'
#' @export
addin <- function() {
  run_app()
}