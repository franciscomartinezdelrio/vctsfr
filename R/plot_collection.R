#' Create a `ggplot` object associated with a time series belonging to a
#' collection.
#'
#' Apart from the time series, future values and forecasts for the
#' future values form part of the `ggplot` object.
#'
#' The `collection` parameter must be a list. Each component of the list stores
#' a time series and, optionally, its future values, forecasts for the future
#' values and prediction intervals for the forecasts. Each component should have
#' been created using the [ts_info()] function.
#'
#' In the example section you can see an example of a collection of time series.
#' If the `collection` parameter is not specified correctly, a proper message is
#' shown.
#'
#' @param collection a list with the collection of time series. Each component
#'   of the list must have been built with the [ts_info()] function.
#' @param number an integer. The number of the time series. It should be a value
#'   between 1 and `length(collection)`.
#' @param methods NULL (default) or a character vector indicating the names of
#'   the forecasting methods to be displayed.
#' @param level NULL (default) or a number in the interval (0, 100) indicating
#'   the level of the prediction interval to be shown. This parameter in
#'   considered only when just one forecasting method is plotted and the
#'   forecasting method has a prediction interval with the specified level.
#' @param sdp logical. Should data points be shown in the plot? (default value
#'   `TRUE`)
#'
#' @return The `ggplot` object representing the time series and its forecast.
#' @export
#'
#' @seealso [ts_info()] function to see how to build the components of the
#'   `collection` parameter.
#' @examples
#' # create a collection of two time series and plot both time series
#' c <- list(ts_info(USAccDeaths), ts_info(ldeaths))
#' plot_collection(c, number = 1)
#' plot_collection(c, number = 2, sdp = FALSE)
#'
#' # create a collection of one time series with future values and forecasts
#' if (require(forecast)) {
#'   c <- vector(2, mode = "list")
#'   timeS <- window(USAccDeaths, end = c(1977, 12))
#'   f <- window(USAccDeaths, start = c(1978, 1))
#'   ets_fit   <- ets(timeS)
#'   ets_pred  <- forecast(ets_fit, h = length(f), level = 90)
#'   mean_pred <- meanf(timeS, h = length(f), level = 90)
#'   c[[1]] <- ts_info(timeS, future = f,
#'             prediction_info("ES", ets_pred$mean,
#'                             pi_info(90, ets_pred$lower, ets_pred$upper)),
#'             prediction_info("Mean", mean_pred$mean,
#'                             pi_info(90, mean_pred$lower, mean_pred$upper))
#'   )
#'   timeS <- ts(rnorm(30, sd = 3))
#'   f <- rnorm(5, sd = 3)
#'   rw <- rwf(timeS, h = length(f), level = 80)
#'   mean <- meanf(timeS, h = length(f), level = 90)
#'   c[[2]] <- ts_info(timeS, future = f,
#'             prediction_info("Random Walk", rw$mean,
#'                             pi_info(80, rw$lower, rw$upper)),
#'             prediction_info("Mean", mean$mean,
#'                             pi_info(90, mean$lower, mean$upper))
#'   )
#'   plot_collection(c, number = 1)
#' }
#' if (require("forecast"))
#'   plot_collection(c, number = 2)
#' if (require("forecast"))
#'   plot_collection(c, number = 2, methods = "Mean") # just plot a forecasting method
#' if (require("forecast"))
#'   plot_collection(c, number = 2, methods = "Random Walk", level = 80)
plot_collection <- function(collection, number, methods = NULL, level = NULL, sdp = TRUE) {
  # check collection parameter
  r <- check_time_series_collection(collection)
  if (r != "OK")
    stop(paste("Error in 'collection' parameter:", r))

  # Check number parameter
  if (! (is.numeric(number) && number >= 1 && number <= length(collection)))
    stop("'number' parameter should be a valid index in collection")

  # Check methods parameter
  if (! (is.null(methods) || is.character(methods)))
    stop("methods parameter should be a character vector")
  if(!is.null(methods) && !("forecasts" %in% names(collection[[number]]))) {
    m <- paste("methods parameter should contain names of forecasting methods in series number", number)
    stop(m)
  }
  if(!is.null(methods) && ("forecasts" %in% names(collection[[number]]))) {
    forecasting_names <- sapply(collection[[number]]$forecasts, function(x) x$name)
    if (!all(methods %in% forecasting_names)){
      m <- paste("all the names of method parameter should be existing forecasting methods in series number", number)
      stop(m)
    }
  }

  # check level parameter
  if (!is.null(level) && (!is.numeric(level) || length(level) > 1 || level <= 0 || level >= 100))
    stop("Parameter level should be a scalar number in the interval (0, 100)")
  # is there only one forecasting method to plot?
  only_one_method <- ("forecasts" %in% names(collection[[number]])) &&
                     ((!is.null(methods) && length(methods) == 1) ||
                      is.null(methods) && length(collection[[number]]$forecast) == 1)
  if (!is.null(level) && !only_one_method)
    stop("level parameter should only be used when plotting just one forecasting method")

  if (!is.null(level) && only_one_method) {
    position <- if (is.null(methods)) 1 else which(methods == forecasting_names)
    if (!("pi" %in% names(collection[[number]]$forecasts[[position]]))) {
        stop("level parameter is used and the forecasting method has no prediction intervals")
    } else {
      levels <- sapply(collection[[number]]$forecasts[[position]]$pi, function(p) p$level)
      if (!(level %in% levels)) {
        m <- paste0("level ", level,
                    " is not included in the prediction interval levels of the forecasting method")
        m <- paste0(m, "\n  current levels: ", paste(levels, collapse = " "))
        stop(m)
      }
    }
  }
  if (only_one_method && !is.null(level)) {
    position <- if (is.null(methods)) 1 else which(methods == forecasting_names)
    levels <- sapply(collection[[number]]$forecasts[[position]]$pi, function(p) p$level)
    position2 <- which(level == levels)
    return(plot_ts(collection[[number]]$historical,
                   future = collection[[number]]$future,
                   prediction = collection[[number]]$forecast[[position]]$forecast,
                   method = collection[[number]]$forecast[[position]]$name,
                   lpi = collection[[number]]$forecast[[position]]$pi[[position2]]$lpi,
                   upi = collection[[number]]$forecast[[position]]$pi[[position2]]$upi,
                   level = level
    ))
  }

  if ("forecasts" %in% names(collection[[number]])) {
    p <- list()
    for (pred in collection[[number]]$forecasts) {
      if (!is.null(methods) && !(pred$name %in% methods)) next
      p[[length(p) + 1]] <- pred$forecast
      names(p)[[length(p)]] <- pred$name
    }
  } else {
    p <- NULL
  }
  plot_predictions(collection[[number]]$historical,
                   future = collection[[number]]$future,
                   predictions = p,
                   sdp = sdp
  )
}

#' Check that a collection of time series is properly formatted
#'
#' This function checks that an object holding a collection of time series,
#' their future values and their forecasts has the correct format. This kind of
#' objects are used in function [plot_collection()]. A collection of time series
#' should be a list compounded of objects of class `ts_info`, which are built
#' using the [ts_info()] function.
#'
#' @param collection a list representing a collection of time series as
#'   described in [plot_collection()].
#'
#' @return a character string with value `"OK"` if the object is properly
#'   formatted. Otherwise, the character string indicates the first error found
#'   in the object's format.
#' @export
#'
#' @examples
#' c <- list(ts_info(USAccDeaths), ts_info(ldeaths))
#' check_time_series_collection(c)
check_time_series_collection <- function(collection) {
  if (!is.list((collection)))
    return("A time series collection should be a list")
  for (ind in seq_along(collection)) {
    if (! methods::is(collection[[ind]], "ts_info")) {
      return(paste0("Component [[", ind, "]] of collection should be of class ts_info"))
    }
  }
  return("OK")
}
