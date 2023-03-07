#' Create a ggplot object with a time series and forecast
#'
#' A ggplot object associated with the time series (and, optionally, its future
#' values, prediction and prediction intervals) is created and plotted.
#'
#' If \code{future} or \code{prediction} are vectors then they are supposed to
#' start after the time series.
#'
#' @param ts a time series of class \code{ts}.
#' @param future NULL (default) or a time series of class \code{ts} or a vector.
#'   Future values of the time series.
#' @param prediction NULL (default) or a time series of class \code{ts} or a
#'   vector. Prediction of the time series.
#' @param upi NULL (default) or a time series of class \code{ts} or a vector.
#'   Upper limit of a prediction interval.
#' @param lpi NULL (default) or a time series of class \code{ts} or a vector.
#'   Lower limit of a prediction interval.
#' @param level NULL (default) a number in the interval (0, 100) indicating the
#'   level of the prediction interval.
#' @param sdp logical. Should data points be shown?
#'
#' @return The ggplot object representing the time series and its forecast.
#' @export
#'
#' @examples
#' library(ggplot2)
#' plot_ts(USAccDeaths) # plot a time series
#'
#' # plot a time series, not showing data points
#' plot_ts(USAccDeaths, sdp = FALSE)
#'
#' # plot a time series, its future values and a prediction
#' ts <- window(USAccDeaths, end = c(1977, 12))
#' f <- window(USAccDeaths, start = c(1978, 1))
#' p <- ts(window(USAccDeaths, start = c(1976, 1), end = c(1976, 12)),
#'         start = c(1978, 1),
#'         frequency = 12
#' )
#' plot_ts(ts, future = f, prediction = p)
#'
#' # plot a time series and a prediction
#' plot_ts(USAccDeaths, prediction = rep(mean(USAccDeaths), 12))
#'
#' # plot a time series, a prediction and a prediction interval
#' t <- ts(rnorm(40, 0, 3)) # time series
#' f <- rnorm(6, 0, 3)      # future values
#' p <- rep(mean(t), 6)     # prediction
#' upi <- p + qnorm(0.975)*sd(t)
#' lpi <- p - qnorm(0.975)*sd(t)
#' plot_ts(t, future = f, prediction = p, upi = upi, lpi = lpi, level = 95)
plot_ts <- function(ts, future = NULL, prediction = NULL, upi = NULL, lpi = NULL, level = NULL, sdp = TRUE) {
  # check ts parameter
  if(! stats::is.ts(ts))
    stop("Parameter ts should be of class ts")

  check_vector_ts(future, "future")         # check future parameter
  check_vector_ts(prediction, "prediction") # check prediction parameter

  # check different lengths of future and prediction
  if (!is.null(future) && !is.null(prediction) && length(future) != length(prediction))
    warning("Length of prediction and future parameters are different")

  check_vector_ts(upi, "upi") # check upi parameter
  check_vector_ts(lpi, "lpi") # check lpi parameter

  # check different lengths of upi and lpi
  if (length(upi) != length(lpi))
    warning("upi and lpi parameters should have the same length")

  # check different lengths of prediction and upi
  if (!is.null(upi) && length(upi) != length(prediction))
    warning("prediction and upi parameters should have the same length")

  # check different lengths of prediction and lpi
  if (!is.null(lpi) && length(lpi) != length(prediction))
    warning("prediction and lpi parameters should have the same length")

  # Check level parameter
  if(!is.null(level) && (!is.numeric(level) || length(level) > 1 || level <= 0 || level >= 100))
     stop("Parameter level should be a scalar number between 0 and 1")

  # check sdp parameter
  if(! is.logical(sdp))
    stop("Parameter sdp should be a logical value")

  df <- data.frame(
    x = as.vector(stats::time(ts)),
    y = as.vector(ts),
    type = "Historical"
  )

  name_PI <- paste0(if (is.null(level)) "" else level, "% PI")
  df_f <- add_ts(future, ts, "Future")
  df_p <- add_ts(prediction, ts, "Forecast")
  df_upi <- add_ts(upi, ts, name_PI)
  df_lpi <- add_ts(lpi, ts, "Lower PI")

  df <- rbind(df, df_f, df_p, df_upi)

  p <- ggplot2::ggplot(df, ggplot2::aes(x, y))
  p <- p + ggplot2::geom_line(ggplot2::aes(color = type))
  # Lower pi
  if (!is.null(lpi)) {
    p <- p + ggplot2::geom_line(ggplot2::aes(x, y), data = df_lpi, colour = "pink")
    if (sdp)
      p <- p + ggplot2::geom_point(ggplot2::aes(x, y), data = df_lpi, colour = "pink", size = 1)
  }

  if (!is.null(upi) && !is.null(lpi)) {
    limits <- data.frame(x = df_upi$x, y = df_p$y, upi = upi, lpi = lpi)
    p <- p + ggplot2::geom_ribbon(data = limits, ggplot2::aes(x = x, ymax = upi, ymin = lpi), fill = "pink", alpha = 0.25)
  }
  if (sdp) {
    p <- p + ggplot2::geom_point(size = 1, ggplot2::aes(color = type))
  }
  p <- p + ggplot2::labs(x = "Time", y = NULL, color = "Series")
  breaks <- c("Historical", "Future", "Forecast", name_PI)
  colours <- c("black", my_colours("blue"), my_colours("red"), "pink")
  names(colours) <- c("Historical", "Future", "Forecast", name_PI)
  p <- p + ggplot2::scale_colour_manual(values = colours, breaks = breaks)
  p
}

# Check if a parameter is a vector or object of class ts,
check_vector_ts <- function(v, nombre) {
  if(! (is.null(v) || stats::is.ts(v) || is.numeric(v) || is.integer(v))) {
    msg <- paste("Parameter", nombre, "should be a numeric vector or an object of class ts")
    stop(msg)
  }
}

# Add time series v (after ts) of type type to a data frame
add_ts <- function(v, ts, type) {
  if (is.null(v))
    return(NULL)
  if(!stats::is.ts(v))
    v <- v2ts(ts, v)

  data.frame(
    x = as.vector(stats::time(v)),
    y = as.vector(v),
    type = type
  )
}

# Convert a vector into a time series
# The conversion is such that v starts right after time series ts
v2ts <- function(ts, v) {
  temp <- stats::ts(1:2,
                    start = stats::end(ts),
                    frequency = stats::frequency(ts)
  )
  stats::ts(v,
            start = stats::end(temp),
            frequency = stats::frequency(ts)
  )
}

my_colours <- function(name) {
  col_l <- list("blue" = "#000099",
                       "red" = "#CC0000",
                       "green" = "#339900",
                       "orange" = "#CC79A7"
  )
  return(col_l[[name]])
}

