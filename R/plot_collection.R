#' Create the associated ggplot object of a time series belonging to a
#' collection
#'
#' Create the associated ggplot object of a time series belonging to a
#' collection. Apart from the time series, future values and forecasts for the
#' future values form part of the ggplot object.
#'
#' The `collection` parameter must be a list. Each component of the list stores
#' a time series, its future values and its forecasts. Each component should
#' have the following fields with the same names and in the same order:
#'
#' * Historical: an object of class `ts` with the historical
#' values of the time series.
#' * Future: a vector or object of class `ts` with the future values to be predicted.
#' * Forecasts: a named list. Each component contains a vector or
#' object of class `ts` with a forecast for the future values of the series.
#'
#' In the example section you can see an example of a collection of time series.
#' If the `collection` parameter is not specified correctly a proper message is
#' shown.
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
  r <- check_time_series_collection(collection)
  if (r != "OK")
    stop(paste("Error in 'collection' parameter:", r))

  # Check number parameter
  if (! (is.numeric(number) && number >= 1 && number <= length(collection)))
    stop("'number' parameter should be a valid index in collection")

  # check sdp parameter
  if(! is.logical(sdp))
    stop("Parameter sdp should be a logical value")
  plot_predictions(collection[[number]]$Historical,
                   collection[[number]]$Future,
                   collection[[number]]$Forecast,
                   sdp
  )
}

check_time_series_collection <- function(collection) {
  if (!is.list((collection)))
    return("A time series collection should be a list")
  for (ind in seq_along(collection)) {
    r <- check_element(collection[[ind]], ind)
    if (r != "OK")
      return(r)
  }
  return("OK")
}

# Check one of the elements of a collection of series and forecasts
check_element <- function(e, index) {
  if (!is.list(e))
    return(paste0("Component [[", index, "]] of collection should be a list"))
  if (length(e) != 3)
    return(paste0("Component [[", index,
                  "]] of collection should have 3 components. Currently it has ", length(e),
                  " components")
    )
  if (is.null(names(e)))
    return(paste0("The components of component [[", index, "]] of collection have no names"))

  # Historical values
  if (is.null(names(e)[1]) || names(e)[1] != "Historical") {
    if (names(e)[1] == "")
      return(paste0("The first component of component [[", index, "]] of collection has no name and it should be 'Historical'"))
    else
      return(paste0("The name of the first component of component [[", index, "]] of collection should be 'Historical', not '", names(e[1]), "'"))
  }
  if(! stats::is.ts(e$Historical))
    return(paste0("The Historical component of component [[", index, "]] of collection should be of class ts"))

  # Future values
  if (is.null(names(e)[2]) || names(e)[2] != "Future") {
    if (names(e)[2] == "")
      return(paste0("The first component of component [[", index, "]] of collection has no name and it should be 'Future'"))
    else
      return(paste0("The name of the first component of component [[", index, "]] of collection should be 'Future', not '", names(e[2]), "'"))
  }
  if(! (stats::is.ts(e$Future) || is.numeric(e$Future) || is.integer(e$Future)))
    return(paste0("The Future component of component [[", index, "]] of collection should be an object of class ts or a vector"))

  # Forecasts values
  if (is.null(names(e)[3]) || names(e)[3] != "Forecasts") {
    if (names(e)[3] == "")
      return(paste0("The first component of component [[", index, "]] of collection has no name and it should be 'Forecasts'"))
    else
      return(paste0("The name of the first component of component [[", index, "]] of collection should be 'Forecasts', not '", names(e[3]), "'"))
  }
  if (!is.list(e$Forecast))
    return(paste0("The 'Forecasts' component of component [[", index, "]] of collection should be a list"))
  if (is.null(names(e$Forecasts)) || any(names(e$Forecasts) == ""))
    return(paste0("All the components of component 'Forecasts' of component [[", index, "]] of collection should have a name"))
  for (f in e$Forecasts) {
    if(! (stats::is.ts(f) || is.numeric(f) || is.integer(f)))
      return(paste0("Each forecast of component [[", index, "]] should be an object of class ts or a vector"))
  }
  return("OK")
}
