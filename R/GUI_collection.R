#' Launches the web-based GUI for visualizing time series
#'
#' Launches the web-based GUI for visualizing a collection of time series in a
#' web browser.
#'
#' The **vctsfr** package provides a Shiny-based GUI to visualize collections of
#' time series and their forecasts. The main features of the GUI are:
#'
#' * It allows you to easily navigate through the different series.
#' * You can select which forecasting methods are displayed.
#' * In the case you display a single forecasting method with associated
#' prediction intervals, you can select the prediction interval to display.
#' * Forecasting accuracy measures are displayed.
#'
#' @inheritParams plot_collection
#'
#' @return Nothing
#' @export
#'
#' @examplesIf interactive()
#' # create a collection of two time series and visualize them
#' c <- list(ts_info(USAccDeaths), ts_info(ldeaths))
#' GUI_collection(c)
GUI_collection <- function(collection) {
  r <- check_time_series_collection(collection)
  if (r != "OK")
    stop(paste("Error in 'collection' parameter:", r))

  # accuracy measures
  am <- list(RMSE = function(fut, fore, historical = NULL) sqrt(mean((fut-fore)^2)),
             MAPE = function(fut, fore, historical = NULL) mean(abs((fut-fore)/fut))*100,
             MAE = function(fut, fore, historical = NULL) mean(abs(fut-fore)),
             ME = function(fut, fore, historical = NULL) mean(fut-fore),
             MPE = function(fut, fore, historical = NULL) mean((fut-fore)/fut)*100
  )

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
      shiny::mainPanel(shiny::plotOutput("plot"),
                       shiny::br(),
                       shiny::uiOutput("accu_message"),
                       shiny::tableOutput("accuracy"))
    )
  )

  server <- function(input, output) {
    output$models <- shiny::renderUI({
      pred <- collection[[input$number]]
      if ("forecasts" %in% names(pred)) {
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
      p + ggplot2::ggtitle(paste("Time series", collection[[input$number]]$name)) +
          ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
    }, res = 96)

    output$accu_message <- shiny::renderUI({
      pred <- collection[[input$number]]
      if (!is.null(pred$future) && !is.null(input$model)) {
        shiny::h4(shiny::strong("Forecast accuracy measures"))
      }
    })

    output$accuracy <- shiny::renderTable({
      pred <- collection[[input$number]]
      if (!is.null(pred$future) && !is.null(input$model)) {
        d <- NULL
        for(a_m in am) {
          d <- cbind(d, compute_error(a_m, pred, input$model))
        }
        d <- data.frame(d)
        colnames(d) <- names(am)
        row.names(d) <- input$model
        d
      }
    }, rownames = TRUE)
  }
  shiny::shinyApp(ui, server)
}

compute_error <- function(f, information, models) {
  result <- numeric(length = length(models))
  for (ind in seq_along(models)) {
    name <- models[ind]
    forecasting_names <- sapply(information$forecasts, function(x) x$name)
    position <- which(name == forecasting_names)
    result[ind] <- f(information$future, information$forecasts[[position]]$forecast)
  }
  result
}

