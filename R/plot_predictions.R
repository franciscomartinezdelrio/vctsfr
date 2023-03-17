#'Creates a ggplot object with a time series and some forecasts
#'
#'Create a `ggplot` object with a time series and, optionally, some future
#'values of the time series and several forecast for those future values.
#'
#'If \code{future} or the forecasts in the \code{prediction} list are vectors
#'then they are supposed to start after the last data of the time series.
#'
#'@inheritParams plot_ts
#'@param predictions NULL (default) or a named list containing the predictions
#'  for the future values. Each component of the list should contain a vector or
#'  an object of class \code{ts} representing a forecast, the name of the
#'  component should be the name of the forecasting method.
#'
#'@return The `ggplot` object representing the time series and its forecast.
#'@export
#'
#' @examples
#' # plot a time series, its future values and two forecasts
#' ts <- window(USAccDeaths, end = c(1977, 12))
#' f <- window(USAccDeaths, start = c(1978, 1))
#' prediction1 <- rep(mean(ts), 12)
#' prediction2 <- as.vector(window(ts, start = c(1977, 1)))
#' p <- list(Mean = prediction1, Naive = prediction2)
#' plot_predictions(ts, future = f, predictions = p)
plot_predictions <- function(ts, future = NULL, predictions = NULL, sdp = TRUE) {
  # check ts parameter
  if(! stats::is.ts(ts))
    stop("Parameter ts should be of class ts")

  check_vector_ts(future, "future")         # check future parameter

  # check predictions parameter
  if (!is.null(predictions)) {
    if (!is.list(predictions))
      stop("Predictions parameter should be a named list with the different forecasts")
    if (is.null(names(predictions)) || any(names(predictions) == ""))
      stop("All the elements in the list predictions should have a name")

    for (ind in seq_along(predictions)) {
      if(! (stats::is.ts(predictions[[ind]]) || is.numeric(predictions[[ind]]) ||
            is.integer(predictions[[ind]]))) {
        msg <- paste("Forecast", names(predictions)[ind],
                     "should be a numeric vector or an object of class ts")
        stop(msg)
      }
    }
  }

  # check sdp parameter
  if(! is.logical(sdp))
    stop("Parameter sdp should be a logical value")

  df <- data.frame(
    x = as.vector(stats::time(ts)),
    y = as.vector(ts),
    type = "Historical"
  )
  df <- rbind(df, add_ts(future, ts, "Future"))

  for (ind in seq_along(predictions))
    df <- rbind(df, add_ts(predictions[[ind]], ts, names(predictions)[ind]))

  x <- y <- type <- NULL # to avoid notes
  p <- ggplot2::ggplot(df, mapping = ggplot2::aes(x, y)) +
    ggplot2::geom_line(ggplot2::aes(color = type))
  if (sdp)
    p <- p + ggplot2::geom_point(mapping = ggplot2::aes(color = type), size = 1)
  p <- p + ggplot2::labs(color = "Series", x = "Time", y = NULL)
  breaks <- c("Historical", "Future", names(predictions))
  my_col <- c("#000000", "#0000DD", "#E69F00",  "#009E73", "#F0E442", "#D55E00",
              "#CC79A7", "#56B4E9")
  colours <- my_col[seq_along(breaks)]
  names(colours) <- breaks
  p <- p + ggplot2::scale_colour_manual(values = colours, breaks = breaks)

  p
}
