#' Colour picker gadget
#'
#' This gadget lets you choose colours easily. You can select multiple colours,
#' and you can either choose any RGB colour, or browse through R colours.
#'
#' @param numCols The number of colours to select when the gadget launches (you
#' can add and remove more colours from the app itself too)
#' @note This gadget returns a vector of colours that can be assigned to
#' a variable. If instead you want to get a text representation of the colours
#' that can embedded into code, use the addin from the RStudio Addins menu.
#' @return Vector of selected colours
#' @export
#' @examples
#' if (interactive()) {
#'   cols <- colourPicker(5)
#' }
colourPicker <- function(numCols = 3) {
  colourPickerGadget(numCols)
}

colourPickerAddin <- function() {
  col <- colourPickerGadget()
  text <- paste0("c(\"", paste(col, collapse = "\", \""), "\")")
  invisible(rstudioapi::insertText(text = text))
}


#' @import shiny
#' @import miniUI
colourPickerGadget <- function(numCols = 3) {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("You must have RStudio v0.99.878 or newer to use the colour picker",
         call. = FALSE)
  }

  shiny::addResourcePath("cpg", system.file("gadgets", "colourpicker", package = "colourpicker"))

  ui <- miniPage(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(
      script = file.path("cpg", "js", "shinyjs-funcs.js"),
      functions = c()
    ),
    tags$head(
      tags$link(rel="stylesheet", href = file.path("cpg", "css", "app.css")),
    ),

    gadgetTitleBar(
      span(strong("Colour Picker"),
           span(id = "author", "By",
                a(href = "https://deanattali.com", "Dean Attali")))
    ),

    # Header section - shows the selected colours
    div(
      id = "header-section",
      div(
        id = "header-title",
        "Selected colours"
      ),
      div(
        id = "selected-cols-row",
        div(id = "addColBtn",
            icon("plus"),
            title = "Add another colour"
        ),
        div(id = "removeColBtn",
            icon("trash-alt"),
            title = "Remove selected colour"
        ),
        uiOutput("selectedCols", inline = TRUE)
      ),
      checkboxInput(
        "returnTypeName",
        "Return colour name (eg. \"white\") instead of HEX value (eg. #FFFFFF) when possible",
        width = "100%"
      ),
      actionLink("showShortcuts", "Show keyboard shortcuts")
    ),

    miniTabstripPanel(

      # Tab 1 - choose any colour
      miniTabPanel(
        "Any colour",
        icon = icon("globe"),
        miniContentPanel(
          div(
            id = "anycolarea",
            br(),
            colourpicker::colourInput(
              "anyColInput", "Select any colour", showColour = "both",
              value = "white", allowTransparent = TRUE)
          )
        )
      ),

      # Tab 2 - choose an R colour similar to a colour you choose
      miniTabPanel(
        "Find R colour",
        icon = icon("search"),
        miniContentPanel(
          fluidRow(
            column(
              6,
              colourpicker::colourInput(
                "rclosecolInput", "Show R colours similar to this colour",
                showColour = "both", value = "orange")
            ),
            column(
              6,
              div(id = "customSliderContainer",
                  sliderInput("numSimilar", "How many colours to show",
                              min = 1, max = 40, value = 8, step = 1)
              )
            )
          ),
          br(),
          strong("Click a colour to select it"),
          uiOutput("rclosecolsSection")
        )
      ),

      # Tab 3 - choose any R colour
      miniTabPanel(
        "All R colours",
        icon = icon("paint-brush"),
        miniContentPanel(
          strong("Click a colour to select it"),
          br(),
          img(id = "allcols-spinner",
              src = file.path("cpg", "img", "ajax-loader.gif")
          ),
          uiOutput("allColsSection")
        )
      )
    )
  )

  server <- function(input, output, session) {
    values <- reactiveValues(
      selectedCols = rep("#FFFFFF", numCols),
      selectedNum = 1,
      colUpdateSrc = 0
    )

    # User canceled
    observeEvent(input$cancel, {
      stopApp(stop("User canceled colour selection", call. = FALSE))
    })

    # Don't allow user to remove the last colour
    observe({
      shinyjs::toggleState("removeColBtn",
                           condition = length(values$selectedCols) > 1)
    })

    # User is done selecting colours
    observeEvent(input$done, {
      cols <- values$selectedCols
      shinyjs::disable(selector = "#cancel, #done")

      if (input$returnTypeName) {
        cols <- lapply(cols, getColNameOrHex)
        cols <- unlist(cols)
      }

      stopApp(cols)
    })

    # Add another colour to select
    shinyjs::onclick("addColBtn", {
      values$selectedCols <- c(values$selectedCols, "#FFFFFF")
    })

    # Remove the selected colour
    shinyjs::onclick("removeColBtn", {
      if (length(values$selectedCols) == 1) {
        return()
      }

      values$selectedCols <- values$selectedCols[-values$selectedNum]
      if (values$selectedNum > length(values$selectedCols)) {
        values$selectedNum <- length(values$selectedCols)
      }
    })

    # Render the chosen colours
    output$selectedCols <- renderUI({
      lapply(seq_along(values$selectedCols), function(colNum) {
        cls <- "col col-transparent-box"
        if (colNum == values$selectedNum) {
          cls <- paste0(cls, " selected")
        }
        if (isColDark(values$selectedCols[colNum])) {
          cls <- paste0(cls, " col-dark")
        }
        div(
          class = cls,
          div(
            style = paste0("background:",
                           hex2rgba_str(values$selectedCols[colNum])),
            class = "selected-col-inner",
            `data-colnum` = colNum,
            colNum
          )
        )
      })
    })

    # Receive event from JS: a different colour number was selected
    observeEvent(input$jsColNum, {
      newNum <- input$jsColNum[1]
      if (newNum < 1 || newNum > length(values$selectedCols)) {
        return()
      }
      values$selectedNum <- newNum
    })

    # A colour from the "any colour" input is chosen
    observeEvent(input$anyColInput, {
      if (values$colUpdateSrc == 1) {
        values$colUpdateSrc <- 0
        return()
      }
      # Make sure we don't get into a loop of the selected colour and the
      # colour input updating each other
      values$colUpdateSrc <- 2

      values$selectedCols[values$selectedNum] <- input$anyColInput
    })

    # Receive event from JS: an R colour was selected from one of the two tabs
    # Because of how Shiny works, the input from JS needs to also contain
    # a dummy random variable, so that when the user chooses the same colour
    # twice, it will register the second time as well
    observeEvent(input$jsCol, {
      values$selectedCols[values$selectedNum] <- input$jsCol[1]
    })

    # Update the colour input to the currently selected colour
    observeEvent(list(values$selectedCols, values$selectedNum), {
      if (values$colUpdateSrc == 2) {
        values$colUpdateSrc <- 0
        return()
      }
      values$colUpdateSrc <- 1
      # Make sure we don't get into a loop of the selected colour and the
      # colour input updating each other
      newCol <- values$selectedCols[values$selectedNum]
      if (!is.null(input$anyColInput) && input$anyColInput == newCol) {
        values$colUpdateSrc <- 0
      }
      colourpicker::updateColourInput(session, "anyColInput", value = newCol)
    })

    # Receive event from JS: navigate to the colour to the left/right
    observeEvent(input$jsColNav, {
      newNum <- values$selectedNum + input$jsColNav[1]
      if (newNum == 0) {
        newNum <- length(values$selectedCols)
      } else if (newNum == length(values$selectedCols) + 1) {
        newNum <- 1
      }
      values$selectedNum <- newNum
    })

    # Render all the R colours
    output$allColsSection <- renderUI({
      lapply(
        grDevices::colours(distinct = TRUE),
        function(x) {
          actionLink(
            paste0("rcol-", x),
            label = NULL,
            class = "rcol",
            style = paste0("background: ", col2hex(x)),
            title = x,
            `data-col` = col2hex(x)
          )
        }
      )
    })

    # After the user chooses a colour, show all the similar R colours
    output$rclosecolsSection <- renderUI({
      rcols <- closestColHex(input$rclosecolInput, n = input$numSimilar)

      tagList(
        div(
          id = "rcolsnames",
          lapply(
            seq_along(rcols),
            function(x) {
              div(
                class = "rcolbox",
                actionLink(
                  paste0("rclosecol-", x),
                  label = NULL,
                  class = "rcol rcolbig",
                  style = paste0("background: ", col2hex(rcols[x])),
                  title = rcols[x],
                  `data-col` = col2hex(rcols[x])
                ),
                span(rcols[x], class = "rcolname")
              )
            }
          )
        )
      )
    })

    # Show the keyboard shortcuts
    observeEvent(input$showShortcuts, {
      # If it's an old shiny version that doesn't support modals, use an alert
      if (utils::packageVersion("shiny") < "0.14") {
        shinyjs::alert(paste(
          sep = "\n",
          "Left/Right Arrows          Select previous/next colour",
          "Numbers 1-9          Select colour 1-9",
          "Spacebar          Add another colour",
          "Delete          Remove selected colour",
          "Enter          Done",
          "Esc          Close the colour helper"
        ))
        return()
      }

      showModal(modalDialog(
        easyClose = FALSE,
        title = "Keyboard shortcuts",
        div(
          class = "ksh",
          span(
            class = "ksh-left",
            span(class = "ksh-key", HTML("&larr;")),
            "/",
            span(class = "ksh-key", HTML("&rarr;"))
          ),
          span(class = "ksh-right", "Select previous/next colour")
        ),
        div(
          class = "ksh",
          span(
            class = "ksh-left",
            span(class = "ksh-key", "1"),
            "-",
            span(class = "ksh-key", "9")
          ),
          span(class = "ksh-right", "Select colour 1-9")
        ),
        div(
          class = "ksh",
          span(
            class = "ksh-left",
            span(class = "ksh-key", HTML("&nbsp;&nbsp;Spacebar&nbsp;&nbsp;"))
          ),
          span(class = "ksh-right", "Add another colour")
        ),
        div(
          class = "ksh",
          span(
            class = "ksh-left",
            span(class = "ksh-key", "Delete")
          ),
          span(class = "ksh-right", "Remove selected colour")
        ),
        div(
          class = "ksh",
          span(
            class = "ksh-left",
            span(class = "ksh-key", "Enter")
          ),
          span(class = "ksh-right",
               HTML("Done"))
        ),
        div(
          class = "ksh",
          span(
            class = "ksh-left",
            span(class = "ksh-key", "Esc")
          ),
          span(class = "ksh-right", "Close the colour helper")
        ),
        footer = "Press any key to dismiss"
      ))
    })

    # Close the keyboard shortcuts modal
    observeEvent(input$hideShortcuts, {
      removeModal()
    })
  }

  viewer <- shiny::dialogViewer("Colour Picker", width = 800, height = 700)
  shiny::runGadget(shiny::shinyApp(ui, server), viewer = viewer,
                   stopOnCancel = FALSE)
}
