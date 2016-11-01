#' @export
#' @import shiny
#' @import miniUI
plotHelper <- function(code, colours) {

  # Whether or not we attached ggplot2 and hence need to detach it at the end
  ggdetach <- FALSE

  # Use default code if none was given
  if (missing(code)) {
    code <- "ggplot(iris, aes(Sepal.Length, Petal.Length)) +
      geom_point(aes(col = Species)) +
      scale_colour_manual(values = CPCOLS)"

    # Load ggplot2 if it isn't attached
    if (!"ggplot2" %in% .packages()) {
      ggdetach <- TRUE
      code <- paste0("library(ggplot2)\n\n", code)
    }

    # If no arguments were given, default to three colours
    if (missing(colours)) {
      colours <- c("green", "blue", "red")
    }
  }
  # If code was given, parse it and save it
  else {
    if (!is.character(code)) {
      code <- paste(deparse(substitute(code)), collapse = " ")
    }

    # If no colours were given, try to guess how many colours are needed by
    # building a ggplot2 plot and seeing if an error about missing colours is
    # thrown
    if (missing(colours)) {
      colours <- "white"
      tempcode <- paste0("CPCOLS <- colours;", code)
      tryCatch({
        p <- eval(parse(text = tempcode))
        if (ggplot2::is.ggplot(p)) {
          ggplot2::ggplot_build(p)
        }
      }, error = function(err) {
        mainEnv <- parent.env(environment())
        regex <- "Insufficient values in manual scale\\. ([0-9]+) needed.*"
        if (grepl(regex, err$message)) {
          assign("colours", as.numeric(sub(regex, "\\1", err$message)),
                 envir = mainEnv)
        }
      })
    }
  }

  if (is.numeric(colours)) {
    colours <- rep("white", colours)
  }
  colours <- unlist(lapply(colours, col2hex))

  resourcePath <- system.file("gadgets", "colourpicker", package = "colourpicker")
  shiny::addResourcePath("cpg", resourcePath)

  ui <- miniPage(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(
      script = file.path(resourcePath, "js", "shinyjs-funcs.js"),
      functions = c()
    ),
    tags$head(
      includeCSS(file.path(resourcePath, "css", "app.css")),
      includeCSS(file.path(resourcePath, "css", "plotHelper.css"))
    ),

    gadgetTitleBar(span(strong("Plot Colour Helper"),
                        span(id = "author", "By",
                             a(href = "http://deanattali.com", "Dean Attali")))
    ),

    div(id = "plotArea",
        shinyjs::hidden(
          div(id = "plotErrorOut",
            strong("Error with the plot:"),
            textOutput("plotError")
          )
        ),
        div(id = "plot-container",
            tags$img(src = file.path("cpg", "img", "ajax-loader.gif"),
                     id = "plot-spinner"),
            plotOutput("plot", width = "100%")
        )
    ),

    # Header section - shows the selected colours
    div(
      id = "header-section",
      div(
        id = "header-title",
        "Selected colours (available in your code as \"CPCOLS\")"
      ),
      div(
        id = "selected-cols-row",style="",
        div(id = "addColBtn",
            icon("plus"),
            title = "Add another colour"
        ),
        div(id = "removeColBtn",
            icon("trash-o"),
            title = "Remove selected colour"
        ),
        uiOutput("selectedCols", inline = TRUE)
      ),
      checkboxInput(
        "returnTypeName",
        "Return colour name (eg. \"white\") instead of HEX value (eg. #FFFFFF) when possible",
        width = "100%"
      )
    ),

    miniTabstripPanel(

      miniTabPanel(
        "Plot code",
        icon = icon("code"),
        miniContentPanel(
          div(
            id = "codeArea",
            br(),
            "Enter valid R code for a plot. Use the variable name 'CPCOLS'", br(),
            "wherever you want to use the selected list of colours.",
            textAreaInput("code", NULL, code, rows = 8)
          )
        )
      ),

      # Tab 1 - choose any colour
      miniTabPanel(
        "Any colour",
        icon = icon("globe"),
        miniContentPanel(
          div(
            id = "anycolarea",
            br(),
            uiOutput("anyColInputPlaceholder")
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
                "rclosecolInput","Show R colours similar to this colour",
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

    # If we attached ggplot2, detach it
    session$onSessionEnded(function() {
      if (ggdetach) {
        detach("package:ggplot2", unload = TRUE)
      }
    })

    values <- reactiveValues(
      selectedCols = colours,
      selectedNum = 1,
      colUpdateSrc = 0,
      plotError = NULL
    )

    # Initialize the main colour picker to the first colour in the list
    output$anyColInputPlaceholder <- renderUI({
      colourpicker::colourInput(
        "anyColInput", "Select any colour", colours[1], showColour = "both")
    })
    outputOptions(output, "anyColInputPlaceholder", suspendWhenHidden = FALSE)

    cpcols <- reactive({
      values$selectedCols
    })

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

      stopApp(dput(cols))
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
        if (colNum == values$selectedNum) {
          cls <- "col selected"
        } else {
          cls <- "col"
        }
        if (isColDark(values$selectedCols[colNum])) {
          cls <- paste0(cls, " col-dark")
        }
        div(
          style = paste0("background:", values$selectedCols[colNum]),
          `data-colnum` = colNum,
          class = cls,
          colNum
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
      if(values$colUpdateSrc == 1) {
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
      if(values$colUpdateSrc == 2) {
        values$colUpdateSrc <- 0
        return()
      }
      values$colUpdateSrc <- 1
      # Make sure we don't get into a loop of the selected colour and the
      # colour input updating each other
      newCol <- values$selectedCols[values$selectedNum]
      if(!is.null(input$anyColInput) && input$anyColInput == newCol) {
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

    output$plot <- renderPlot({
      tryCatch({
        shinyjs::hide('plotErrorOut')
        code <- input$code
        code <- paste0("CPCOLS <- cpcols();", code)
        p <- eval(parse(text = code))

        # If it's a ggplot2 plot, we need to explicitly print it to see if there
        # are errors
        if (ggplot2::is.ggplot(p)) {
          print(p)
        } else {
          p
        }
      }, error = function(err) {
        values$plotError <- err$message
        shinyjs::show('plotErrorOut')
      })
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

    output$plotError <- renderText({
      values$plotError
    })
  }

  shiny::runGadget(shiny::shinyApp(ui, server), viewer = shiny::paneViewer(), stopOnCancel = FALSE)
}
