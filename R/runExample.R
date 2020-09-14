#' Run a colourpicker example
#'
#' Launch a \code{colourpicker} example Shiny app that shows how to use the
#' \code{colourInput} control. \cr
#' The example is also
#' \href{https://daattali.com/shiny/colourInput/}{available online}.
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   runExample()
#' }
#' @export
runExample <- function() {
  appDir <- system.file("examples", "colourInput", package = "colourpicker")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `colourpicker`.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
