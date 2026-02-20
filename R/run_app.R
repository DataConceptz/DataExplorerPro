#' Launch DataExplorerPro Add-in
#'
#' Opens the interactive data exploration studio in a new RStudio viewer pane or window.
#'
#' @param port Port for Shiny server (default: 5050)
#' @param host Host address (default: 127.0.0.1)
#' @param browser Whether to open in external browser (default: FALSE)
#' @param ... Additional arguments passed to shiny::shinyApp
#'
#' @return Shiny app object
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
run_app <- function(port = 5050, host = "127.0.0.1", browser = FALSE, ...) {
  
  # Check if required packages are installed
  required_packages <- c("shiny", "plotly", "dplyr", "tidyr", "ggplot2", 
                         "skimr", "DT", "jsonlite", "httr", "glue",
                         "purrr", "stringr", "lubridate", "scales", 
                         "htmltools", "bslib", "gargoyle")
  
  missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
  if (length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages, repos = "https://cloud.r-project.org")
  }
  
  # Load required packages
  lapply(required_packages, library, character.only = TRUE)
  
  # Source all R files
  # Try multiple locations for the app directory
  app_dir <- NULL
  
  # Method 1: Try installed package location
  tryCatch({
    installed_app_dir <- system.file("app", package = "DataExplorerPro")
    if (nzchar(installed_app_dir) && dir.exists(installed_app_dir)) {
      app_dir <- installed_app_dir
    }
  }, error = function(e) NULL)
  
  # Method 2: Try development directory (when running from source)
  if (is.null(app_dir)) {
    possible_paths <- c(
      file.path(getwd(), "inst", "app"),
      file.path(dirname(getwd()), "inst", "app"),
      "C:/Users/GADSPAA/Documents/R-Assistant/DataExplorerPro/inst/app"
    )
    for (p in possible_paths) {
      if (dir.exists(p)) {
        app_dir <- p
        break
      }
    }
  }
  
  if (is.null(app_dir) || !dir.exists(app_dir)) {
    stop("Could not find app directory. Please reinstall the package with:\n",
         'install.packages("C:/Users/GADSPAA/Documents/R-Assistant/DataExplorerPro", ',
         'repos = NULL, type = "source")')
  }
  
  message("Loading app files from: ", app_dir)

  # Launch app directly from directory so relative assets in app/www resolve
  # correctly in installed-package environments.
  if (browser) {
    shiny::runApp(appDir = app_dir, host = host, port = port, launch.browser = TRUE, ...)
  } else {
    viewer <- getOption("viewer")
    launch_target <- if (!is.null(viewer) && interactive()) viewer else TRUE
    shiny::runApp(appDir = app_dir, host = host, port = port, launch.browser = launch_target, ...)
  }

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