#' Create the associated ggplot object of a time series belonging to a
#' collection
#'
#' Create a `ggplot` object associated with a time series belonging to a
#' collection. Apart from the time series, future values and forecasts for the
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
#' @param sdp logical. Should data points be shown in the plot? (default value `TRUE`)
#'
#' @return The ggplot object representing the time series and its forecast.
#' @export
#'
#' @seealso [ts_info()] function to see how to build the components of the
#'   `collection` parameter.
#' @examples
#' # create a collection of two time series and plot both time series
#' c <- list(
#'           ts_info(USAccDeaths),
#'           ts_info(ldeaths)
#' )
#' plot_collection(c, number = 1)
#' plot_collection(c, number = 2)
plot_collection <- function(collection, number, sdp = TRUE) {
  # check collection parameter
  r <- check_time_series_collection(collection)
  if (r != "OK")
    stop(paste("Error in 'collection' parameter:", r))

  # Check number parameter
  if (! (is.numeric(number) && number >= 1 && number <= length(collection)))
    stop("'number' parameter should be a valid index in collection")

  # check sdp parameter
  if(! is.logical(sdp))
    stop("Parameter sdp should be a logical value")

  if ("forecasts" %in% names(collection[[number]])) {
    p <- list()
    for (pred in collection[[number]]$forecasts) {
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
#' objects are used in function [plot_collection].
#'
#' @param collection a list representing a collection of time series as
#'   described in [plot_collection].
#'
#' @return a character string with value `"OK"` if the object is properly
#'   formatted. Otherwise, the character string indicates the first error found
#'   in the object format.
#' @export
#'
#' @examples
#' c <- list(
#'    list(Historical = window(USAccDeaths, end = c(1977, 12)),
#'         Future = window(USAccDeaths, start = c(1978, 1)),
#'         Forecasts = list(mean = rep(mean(window(USAccDeaths, end = c(1977, 12))), 12),
#'                          naive = rep(tail(window(USAccDeaths, end = c(1977, 12)), 1), 12)
#'         )
#'    ),
#'    list(Historical = window(UKDriverDeaths, end = c(1983, 12)),
#'         Future = window(UKDriverDeaths, start = c(1984, 1)),
#'         Forecasts = list(mean = rep(mean(window(UKDriverDeaths, end = c(1983, 12))), 12),
#'                          naive = rep(tail(window(UKDriverDeaths, end = c(1983, 12)), 1), 12)
#'         )
#'    )
#' )
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

