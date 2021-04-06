#' Interactive map of earthquakes
#'
#' \code{eq_map} creates an interactive map
#' (\code{\link[leaflet]{leaflet}}) of earthquakes.
#'
#' @param .df (data.frame) earthquake data (NOAA)
#' @param annot_col (chr) name of the column used for the annotation.
#'
#' @return (leaflet) of earthquakes and annotations.
#' @export
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#'
#' @examples
#' \dontrun{
#'     library(dplyr)
#'     library(lubridate)
#'     library(devrcap)
#'
#'     library(lubridate)
#'         data(noaa)
#'
#'     noaa %>%
#'         eq_clean_data() %>%
#'         filter(
#'             country %in% c("ITALY", "GREECE", "PORTUGAL"),
#'             year(date) >= 1900
#'         ) %>%
#'         eq_map("location_name")
#'
#'
#'     noaa %>%
#'         eq_clean_data %>%
#'         dplyr::filter(country == "MEXICO", year(date) >= 2000) %>%
#'         eq_map("date")
#' }
eq_map <- function(.df, annot_col) {

  if (!all(
    c("longitude", "latitude", "eq_primary", annot_col) %in%
    tolower(names(.df))
  )) {
    stop(paste(
      "longitude, latitude, eq_primary, and", annot_col,
      "must be all present in the .df data provided.",
      "They don't. Please, provide correct data."
    ))
  }

  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      lng    = .df[["longitude"]],
      lat    = .df[["latitude"]],
      radius = .df[["eq_primary"]],
      weight = 1,
      popup  = .df[[annot_col]]
    )
}
