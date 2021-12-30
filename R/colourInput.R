#' Create a colour input control
#'
#' Create an input control to select a colour.
#'
#' A colour input allows users to select a colour by clicking on the desired
#' colour, or by entering a valid colour in the input box. Colours can be
#' specified as either names ("blue"), HEX codes ("#0000FF"), RGB codes
#' ("rgb(0, 0, 255)"), or HSL codes ("hsl(240, 100, 50)"). Use
#' \code{allowTransparent = TRUE} to allow selecting semi-transparent colours.
#' The return value is a HEX value by default, but you can use the
#' \code{returnName = TRUE} parameter to get an R colour name instead
#' (only when an R colour exists for the selected colour).
#'
#' When \code{allowTransparent = TRUE}, the user can type into the input field
#' any RGBA value, HSLA value, or 8-digit HEX with alpha channel You can also use
#' any of these values as the \code{value} argument as the initial value of the
#' input.
#'
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label Display label for the control, or `\code{NULL} for no label.
#' @param value Initial value (can be a colour name or HEX code)
#' @param showColour Whether to show the chosen colour as text inside the input,
#' as the background colour of the input, or both (default).
#' @param palette The type of colour palette to allow the user to select colours
#' from. \code{square} (default) shows a square colour palette that allows the
#' user to choose any colour, while \code{limited} only gives the user a
#' predefined list of colours to choose from.
#' @param allowedCols A list of colours that the user can choose from. Only
#' applicable when \code{palette == "limited"}. The \code{limited} palette
#' uses a default list of 40 colours if \code{allowedCols} is not defined. If
#' the colour specified in \code{value} is not in the list, the default colour
#' will revert to black.
#' @param allowTransparent If \code{TRUE}, enables a slider to choose an alpha
#' (transparency) value for the colour. When a colour with opacity is
#' chosen, the return value is an 8-digit HEX code.
#' @param returnName If \code{TRUE}, then return the name of an R colour instead
#' of a HEX value when possible.
#' @param closeOnClick If \code{TRUE}, then the colour selection panel will close
#' immediately after selecting a colour.
#' @seealso \code{\link[colourpicker]{updateColourInput}}
#' \code{\link[colourpicker]{colourPicker}}
#' @examples
#' if (interactive()) {
#'   # Example 1
#'   library(shiny)
#'   shinyApp(
#'     ui = fluidPage(
#'       colourInput("col", "Choose colour", "red"),
#'       plotOutput("plot")
#'     ),
#'     server = function(input, output, session) {
#'       output$plot <- renderPlot({
#'         plot(1:10, col = input$col)
#'       })
#'     }
#'   )
#'
#'   # Example 2
#'   library(shiny)
#'   shinyApp(
#'     ui = fluidPage(
#'       strong("Selected colour:", textOutput("value", inline = TRUE)),
#'       colourInput("col", "Choose colour", "red"),
#'       h3("Update colour input"),
#'       textInput("text", "New colour: (colour name or HEX value)"),
#'       selectInput("showColour", "Show colour",
#'         c("both", "text", "background")),
#'       selectInput("palette", "Colour palette",
#'         c("square", "limited")),
#'       checkboxInput("allowTransparent", "Allow transparent", FALSE),
#'       checkboxInput("returnName", "Return R colour name", FALSE),
#'       actionButton("btn", "Update")
#'     ),
#'     server = function(input, output, session) {
#'       observeEvent(input$btn, {
#'         updateColourInput(session, "col",
#'           value = input$text, showColour = input$showColour,
#'           allowTransparent = input$allowTransparent,
#'           palette = input$palette,
#'           returnName = input$returnName)
#'       })
#'       output$value <- renderText(input$col)
#'     }
#'   )
#' }
#' @note See \href{https://daattali.com/shiny/colourInput/}{https://daattali.com/shiny/colourInput/}
#' for a live demo.
#' @export
colourInput <- function(inputId, label, value = "white",
                        showColour = c("both", "text", "background"),
                        palette = c("square", "limited"),
                        allowedCols = NULL, allowTransparent = FALSE,
                        returnName = FALSE, closeOnClick = FALSE) {
  # sanitize the arguments
  showColour <- match.arg(showColour)
  palette <- match.arg(palette)

  value <- restoreInput(id = inputId, default = value)

  # declare dependencies
  shiny::addResourcePath("colourpicker-binding",
                         system.file("srcjs", package = "colourpicker"))
  shiny::addResourcePath("colourpicker-lib",
                         system.file("www", "shared", "colourpicker", package = "colourpicker"))
  deps <- list(
    htmltools::htmlDependency(
      "colourpicker-binding", "0.1.0", c(href = "colourpicker-binding"),
      script = "input_binding_colour.js"),
    htmltools::htmlDependency(
      "colourpicker-lib", "0.1.0", c(href = "colourpicker-lib"),
      script = "js/colourpicker.min.js",
      stylesheet = "css/colourpicker.min.css"
    )
  )

  # build the colour input tag
  inputTag <-
    shiny::tags$input(
      id = inputId, type = "text",
      class = "form-control shiny-colour-input",
      `data-init-value` = value,
      `data-show-colour` = showColour,
      `data-palette` = palette
    )
  if (!is.null(allowedCols)) {
    allowedCols <- jsonlite::toJSON(allowedCols)
    inputTag <- shiny::tagAppendAttributes(
      inputTag,
      `data-allowed-cols` = allowedCols)
  }
  if (returnName) {
    inputTag <- shiny::tagAppendAttributes(
      inputTag,
      `data-return-name` = "true")
  }
  if (allowTransparent) {
    inputTag <- shiny::tagAppendAttributes(
      inputTag,
      `data-allow-alpha` = "true")
  }
  if (closeOnClick) {
    inputTag <- shiny::tagAppendAttributes(
      inputTag,
      `data-close-on-click` = "true")
  }

  inputTag <-
    shiny::div(
      class = "form-group shiny-input-container",
      `data-shiny-input-type` = "colour",
      label %AND% shiny::tags$label(label, class = "control-label", `for` = inputId),
      inputTag
    )

  htmltools::attachDependencies(inputTag, deps)
}

#' Change the value of a colour input
#'
#' Change the value of a colour input on the client.
#'
#' The update function sends a message to the client, telling it to change
#' the settings of a colour input object.\cr
#' This function works similarly to the update functions provided by shiny.\cr
#' Any argument with \code{NULL} values will be ignored.
#'
#' @inheritParams colourInput
#' @param session The \code{session} object passed to function given to \code{shinyServer}.
#' @param inputId The id of the colour input object.
#' @param label The label to set for the input object.
#' @param value The value to set for the input object.
#' @seealso \code{\link[colourpicker]{colourInput}}
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   shinyApp(
#'     ui = fluidPage(
#'       div("Selected colour:", textOutput("value", inline = TRUE)),
#'       colourInput("col", "Choose colour", "red"),
#'       h3("Update colour input"),
#'       textInput("text", "New colour: (colour name or HEX value)"),
#'       selectInput("showColour", "Show colour",
#'         c("both", "text", "background")),
#'       checkboxInput("allowTransparent", "Allow transparent", FALSE),
#'       checkboxInput("returnName", "Return R colour name", FALSE),
#'       actionButton("btn", "Update")
#'     ),
#'     server = function(input, output, session) {
#'       observeEvent(input$btn, {
#'         updateColourInput(session, "col",
#'           value = input$text, showColour = input$showColour,
#'           allowTransparent = input$allowTransparent,
#'           returnName = input$returnName)
#'       })
#'       output$value <- renderText(input$col)
#'     }
#'   )
#' }
#' @note See \href{https://daattali.com/shiny/colourInput/}{https://daattali.com/shiny/colourInput/}
#' for a live demo.
#' @export
updateColourInput <- function(session, inputId, label = NULL, value = NULL,
                              showColour = NULL, palette = NULL, allowedCols = NULL,
                              allowTransparent = NULL,
                              returnName = NULL, closeOnClick = NULL) {
  message <- dropNulls(list(
    label = label, value = value,
    showColour = showColour,
    palette = palette,
    allowedCols = allowedCols,
    allowAlpha = allowTransparent,
    returnName = returnName,
    closeOnClick = closeOnClick
  ))
  session$sendInputMessage(inputId, message)
}

# copied from shiny since it's not exported
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}

# copied from shiny since it's not exported
`%AND%` <- function(x, y) {
  if (!is.null(x) && !isTRUE(is.na(x)))
    if (!is.null(y) && !isTRUE(is.na(y)))
      return(y)
  return(NULL)
}
