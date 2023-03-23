#' Create an object with information about a time series
#'
#' The information about the time series is compounded of the time series and,
#' optionally, its future values and forecasts for those future values (and
#' prediction intervals for those forecasts).
#'
#' @param historical a time series of class \code{ts} with the historical values
#'   of the series.
#' @param ... forecasts for the future values of the time series. A forecast
#'   must have been built with the [prediction_info()] function. See the
#'   examples section.
#' @param future NULL (default) or a time series of class \code{ts} or a vector.
#'   The future values of the time series (possibly to be forecast).
#' @param name NULL (default) or a character string with information about the
#'   time series. Typically, its name.
#'
#' @return An object of class `ts_info`. It is a list containing all the
#'   information supplied to the function.
#' @export
#'
#' @seealso [prediction_info()] for how to create forecasts.
#' @examples
#' # only information about a time series
#' info <- ts_info(USAccDeaths)
#'
#' # Information about a time series and its future values
#' info2 <- ts_info(ts(rnorm(50)), future = rnorm(10))
#'
#' # Information about a time series, its future values and a forecast
#' if (require("forecast")) {
#'   t <- ts(rnorm(50))
#'   f <- rnorm(10)
#'   mf <- meanf(t, level = 95)
#'   info3 <- ts_info(t, future = f,
#'                    prediction_info("mean", mf$mean,
#'                                    pi_info(95, mf$lower, mf$upper)
#'                    )
#'   )
#' }
ts_info <- function(historical, ..., future = NULL, name = NULL) {
  # check historical parameter
  if(! stats::is.ts(historical))
    stop("Parameter historical should be of class ts")

  # check future parameter
  if(! (is.null(future) || stats::is.ts(future) || is.numeric(future) ||
        is.integer(future))) {
    stop("Parameter future should be a numeric vector or an object of class ts")
  }

  # check ... parameter
  l <- list(...)
  for (x in l) {
    if (! methods::is(x, "pred_info"))
      stop("All the ... parameters should be of class pred_info")
  }

  # check name parameter
  if (! (is.null(name) || (is.character(name) && length(name) == 1)))
    stop("name parameter should be a character string")

  info <- list(historical = historical, name = name, future = future)
  if (length(l) > 0)
    info$forecasts <- l
  class(info) <- "ts_info"
  info
}

#' Create an object with a prediction about the future values of a time series
#'
#' The object created contains a forecast and, optionally, prediction intervals
#' for the forecast.
#'
#' @param name a character indicating the name of the method used to forecast.
#' @param forecast a time series of class \code{ts} or a vector. It is a
#'   prediction for the future values of a time series.
#' @param ... prediction intervals for the forecast. These prediction intervals
#'   must have been built with the [pi_info()] function.
#'
#' @return an object of class `pred_info`. A list with the information supplied
#'   to the function.
#' @export
#'
#' @seealso [pi_info()] for how to create prediction intervals.
#' @examples
#' if (require("forecast")) {
#'   time_series <- ts(rnorm(40))
#'   f <- meanf(time_series, level = 95)
#'   info <- prediction_info("mean", f$mean, pi_info(95, f$lower, f$upper))
#' }
prediction_info <- function(name, forecast, ...) {
  # check name parameter
  if (! (is.character(name) && length(name) == 1))
    stop("Parameter name should be a character")

  # check prediction parameter
  if(! (is.null(forecast) || stats::is.ts(forecast) || is.numeric(forecast) ||
        is.integer(forecast))) {
    stop("Parameter forecast should be a numeric vector or an object of class ts")
  }

  # check ... parameter
  l <- list(...)
  for (x in l) {
    if (! methods::is(x, "pi_info"))
      stop("All the ... parameters should be of class pi_info")
    if (length(forecast) != length(x$lpi))
      stop("The length of a prediction interval is different from the length of the forecast")
  }

  pred <- list(name = name, forecast = forecast)
  if (length(l) > 0)
    pred$pi <- l
  class(pred) <- "pred_info"
  pred
}

#' Create a prediction interval object
#'
#' The object created represents a prediction interval for the forecast of the
#' future values of a time series.
#'
#' @param level a number in the interval (0, 100) indicating the level of the
#'   prediction interval.
#' @param lpi a time series of class \code{ts} or a vector. Lower limit of a
#'   prediction interval.
#' @param upi a time series of class \code{ts} or a vector. Upper limit of a
#'   prediction interval.
#'
#' @return An object of class `pi_info`. It is a list containing all the
#'   information supplied to the function.
#'
#' @seealso [prediction_info()] which uses this function to specify prediction
#'   intervals.
#' @export
#'
#' @examples
#' if (require("forecast")) {
#'   time_series <- ts(rnorm(40))
#'   f <- meanf(time_series, level = 95)
#'   info <- pi_info(95, f$lower, f$upper)
#' }
pi_info <- function(level, lpi, upi) {
  # Check level parameter
  if(!is.numeric(level) || length(level) > 1 || level <= 0 || level >= 100)
    stop("Parameter level should be a scalar number in the range (0, 100)")

  # Check lpi parameter
  if(! (stats::is.ts(lpi) || is.numeric(lpi) || is.integer(lpi)))
    stop("Parameter lpi should be a numeric vector or an object of class ts")

  # Check upi parameter
  if(! (stats::is.ts(upi) || is.numeric(upi) || is.integer(upi)))
    stop("Parameter upi should be a numeric vector or an object of class ts")

  # check different lengths of prediction and upi
  if (length(upi) != length(lpi))
    stop("Lower and upper prediction interval should have the same length")

  structure(list(level = level, lpi = lpi, upi = upi), class = "pi_info")
}
