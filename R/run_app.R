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

  # Let Shiny handle host, port, and browser/viewer automatically.
  # This works on local RStudio, Posit Cloud, and RStudio Server.
  shiny::runApp(appDir = app_dir, launch.browser = TRUE, ...)
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