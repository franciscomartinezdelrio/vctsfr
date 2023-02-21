#' Create the associated ggplot object of a time series belonging to a
#' collection
#'
#' Create the associated ggplot object of a time series belonging to a
#' collection. Apart from the time series, future values and forecasts for the
#' future values form part of the ggplot object.
#'
#' The `collection` parameter must be a list. Each component of the list
#' stores a time series, its future values and its forecasts. Each component
#' should have the following fields with the same names and in the same order:
#'
#' * Historical: an object of class `ts` with the historical
#' values of the time series.
#' * Future: a vector or object of class `ts` with the future values to be predicted.
#' * Forecasts: a named list. Each component contains a vector or
#'  object of class `ts` with a forecast for the future values of the
#'  series.
#'
#' In the example section you can see an example of a collection of time series.
#'
#' @param collection a list with the collection of time series. See details.
#' @param number an integer. The number of the time series. It should be a value
#'   between 1 and `length(collection)`.
#' @param sdp logical. Should data points be shown in the plot?
#'
#' @return The ggplot object representing the time series and its forecast.
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
#' plot_collection(c, 1)
#' plot_collection(c, 2)
plot_collection <- function(collection, number, sdp = TRUE) {
  # check collection parameter
  if (!is.list((collection)))
    stop("collection parameter should be a list")
  for (ind in seq_along(collection)) {
    r <- check_element(collection[[ind]])
    if (r != "OK") {
      stop(paste("Element", ind, "of collection:", r))
    }
  }

  # Check number parameter
  if (! (is.numeric(number) && number >= 1 && number <= length(collection)))
    stop("'number' parameter should be an index in collection")

  # check sdp parameter
  if(! is.logical(sdp))
    stop("Parameter sdp should be a logical value")
  plot_predictions(collection[[number]]$Historical,
                   collection[[number]]$Future,
                   collection[[number]]$Forecast,
                   sdp
  )
}

# Check one of the elements of a collection of series and forecasts
check_element <- function(e) {
  if (!is.list(e))
    return("It is not a list")
  if (length(e) != 3)
    return("The number of components is not 3")
  if (is.null(names(e)))
    return("The components have no names")

  # Historical values
  if (is.null(names(e)[1]) || names(e)[1] != "Historical")
    return("The name of the first component is not 'Historical'")
  if(! stats::is.ts(e$Historical))
    return("The Historical component should be of class ts")

  # Future values
  if (is.null(names(e)[2]) || names(e)[2] != "Future")
    return("The name of the second component is not 'Future'")
  if(! (stats::is.ts(e$Future) || is.numeric(e$Future) || is.integer(e$Future)))
    return("The Future component should be an object of class ts or a vector")

  # Forecasts values
  if (is.null(names(e)[3]) || names(e)[3] != "Forecasts")
    return("The name of the third component is not 'Forecasts'")
  if (!is.list(e$Forecast))
    return("The 'Forecasts' component should be a list")
  if (is.null(names(e$Forecasts)) || any(names(e$Forecasts) == ""))
    return("All the component of list 'Forecast' should have a name")
  for (f in e$Forecasts) {
    if(! (stats::is.ts(f) || is.numeric(f) || is.integer(f)))
      return("Each forecast should be an object of class ts or a vector")
  }
  return("OK")
}
