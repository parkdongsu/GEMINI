#' Run GEMINI
#' @keywords gemini
#' @export
#'
# Run GEMINI
gemini_run <- function() {
  path <- paste0(.libPaths()[1],"/GEMINI")
  shiny::runApp(path)
}