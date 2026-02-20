#' Launch DataExplorerPro Add-in
#'
#' Opens the interactive data exploration studio in a new RStudio viewer pane or window.
#'
#' @param ... Additional arguments passed to shiny::runApp
#'
#' @return Invisibly returns NULL
#'
#' @examples
#' \dontrun{
#' # Launch the add-in
#' run_app()
#' }
#'
#' @export
run_app <- function(...) {

  # Find the installed app directory
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

  message("Launching DataExplorerPro from: ", app_dir)

  # Use environment-aware defaults. In hosted environments (e.g., Posit Cloud),
  # host must be 0.0.0.0 for the proxy to connect.
  host <- getOption("shiny.host", "0.0.0.0")
  port <- getOption("shiny.port")

  run_args <- list(appDir = app_dir, host = host, ...)
  if (!is.null(port)) run_args$port <- port

  do.call(shiny::runApp, run_args)
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