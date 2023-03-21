#' Launches the web-based GUI for visualizing time series
#'
#' @inheritParams plot_collection
#'
#' @return Nothing
#' @export
#'
#' @examples
#' # create a collection of two time series and visualize them
#' c <- list(ts_info(USAccDeaths), ts_info(ldeaths))
#' \dontrun{
#' GUI_collection(c)
#' }
GUI_collection <- function(collection) {
  r <- check_time_series_collection(collection)
  if (r != "OK")
    stop(paste("Error in 'collection' parameter:", r))

  ui <- shiny::fluidPage(
    shiny::titlePanel("Visualize time series"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::numericInput("number",
                            paste0("Time series number (max = ", length(collection), ")"),
                            value = 1,
                            min = 1,
                            max = length(collection)),
        shiny::checkboxInput("sdp", "Show data points?", value = TRUE),
        shiny::br(),
        shiny::uiOutput("models"),
        shiny::br(),
        shiny::uiOutput("pi")
      ),
      shiny::mainPanel(shiny::plotOutput("plot"))
    )
  )

  server <- function(input, output) {
    output$models <- shiny::renderUI({
      pred <- collection[[input$number]]
      if ("forecasts" %in% names(pred)) {
        print(input$model)
        names <- sapply(pred$forecasts, function(f) f$name)
        if (is.null(input$model)) {
          selected <- NULL
        } else if (all(input$model %in% names)) {
          selected <- input$model
        } else {
          selected <- names
        }
        shiny::checkboxGroupInput("model",
                                  "Select models",
                                   choices = names,
                                   selected = selected
        )
      }
    })
    output$pi <- shiny::renderUI({
      pred <- collection[[input$number]]
      if ("forecasts" %in% names(pred)) {
        if (!is.null(input$model) && length(input$model) == 1) {
          forecasting_names <- sapply(pred$forecasts, function(x) x$name)
          position <- which(input$model == forecasting_names)
          if ("pi" %in% names(pred$forecasts[[position]]))  {
            levels <- sapply(pred$forecasts[[position]]$pi, function(p) p$level)
            shiny::radioButtons("pi", "Select prediction interval", c("none", paste(levels)))
          }
        }
      }
    })
    output$plot <- shiny::renderPlot({
      if (is.null(input$model)) {
        collection[[input$number]]$forecasts <- NULL
        p <- plot_collection(collection, number = input$number, sdp = input$sdp)
      } else {
        level <- if(length(input$model) == 1 && !is.null(input$pi) && input$pi != "none") as.numeric(input$pi) else NULL
        p <- plot_collection(collection,
                             number = input$number,
                             methods = input$model,
                             level = level,
                             sdp = input$sdp
        )
      }
      p + ggplot2::ggtitle(paste("Time series", collection[[input$number]]$name))
    }, res = 96)
  }
  shiny::shinyApp(ui, server)
}

