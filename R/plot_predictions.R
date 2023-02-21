#' Creates a ggplot object with a time series, some future values and several
#' forecasts over that future values.
#'
#' @inheritParams plot_ts
#' @param future time series of class \code{ts} or a vector. Future values of
#'   the time series.
#' @param predictions a named list containing the predictions for the future
#'   values. Each element of the list should contain a vector or an object of
#'   class \code{ts} representing a forecast, the name of the element should be
#'   the name of the forecasting method.
#'
#' @return The ggplot object representing the time series and its forecast.
#' @export
#'
#' @examples
#' # plot a time series, its future values and two forecasts
#' ts <- window(USAccDeaths, end = c(1977, 12))
#' f <- window(USAccDeaths, start = c(1978, 1))
#' prediction1 <- rep(mean(ts), 12)
#' prediction2 <- as.vector(window(ts, start = c(1977, 1)))
#' p <- list(mean = prediction1, naive = prediction2)
#' plot_predictions(ts, future = f, predictions = p)
plot_predictions <- function(ts, future, predictions, sdp = TRUE) {
  # check ts parameter
  if(! stats::is.ts(ts))
    stop("Parameter ts should be of class ts")

  check_vector_ts(future, "future")         # check future parameter

  # check predictions parameter
  if (!is.list(predictions))
    stop("predictions parameter should be a named list with the different forecasts")
  if (any(names(predictions) == ""))
    stop("all the elements in the list predictions should have a name")

  # habría que comprobar cada predicción

  #check_vector_ts(prediction, "prediction") # check prediction parameter

  df <- data.frame(
    x = as.vector(stats::time(ts)),
    y = as.vector(ts),
    type = "Historical"
  )
  df <- rbind(df, add_ts(future, ts, "Future"))

  for (ind in seq_along(predictions))
    df <- rbind(df, add_ts(predictions[[ind]], ts, names(predictions)[ind]))

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
