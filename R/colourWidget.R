#' Create a colour picker htmlwidget
#'
#' Create a colour picker htmlwidget. This is not terribly useful right now
#' since you can use the more powerful \code{\link[colourpicker]{colourInput}}
#' in Shiny apps and Rmarkdown documents, but this gives you an htmlwidget
#' version of that colour picker.
#'
#' @inheritParams colourInput
#' @param width Custom width for the input field.
#' @param height Custom height for the input field.
#' @param elementId Use an explicit element ID for the widget (rather than an
#' automatically generated one).
#' @import htmlwidgets
#' @examples
#' colourWidget()
#' colourWidget("red", palette = "limited", allowedCols = c("yellow", "red", "#123ABC"))
#'
#' @export
colourWidget <- function(value = "white",
                         showColour = c("both", "text", "background"),
                         palette = c("square", "limited"), allowedCols,
                         width = "300px", height = "35px", elementId = NULL) {
  # sanitize the arguments
  showColour <- match.arg(showColour)
  palette <- match.arg(palette)

  # forward options using x
  x = list(
    value = value,
    showColour = showColour,
    palette = palette
  )

  if (!missing(allowedCols)) {
    allowedCols <- paste(allowedCols, collapse = " ")
    x[['allowedCols']] <- allowedCols
  }

  deps <- list(
    rmarkdown::html_dependency_bootstrap("default")
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'colourWidget',
    x,
    width = width,
    height = height,
    dependencies = deps,
    package = 'colourpicker',
    elementId = elementId
  )
}

colourWidget_html <- function(id, class, style, ...) {
  class <- paste0(class, " form-control")
  htmltools::tags$input(id = id, class = class, style = style,
                        type = "text")
}
