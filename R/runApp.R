#' @export
runExample <- function() {
  appDir <- system.file("myapp", package = "AdvancedRlab5")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `AdvancedRlab5`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}