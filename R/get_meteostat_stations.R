#' Download the full list set of stations and convert to sf
#'
#' Download the full list of Meteostat's stations (updated daily)
#'
#' This function downloads and parses the file with the full list of meteo stations
#' and returns a simple features to feed data to Meteostat's API.
#' See: https://dev.meteostat.net/bulk/stations.html#endpoints
#' @return sf with the following columns:
#' @export
get_meteostat_stations <- function() {

  # 1. download the file -------------------------------------------------------
  cSourceFilePath <- "https://bulk.meteostat.net/stations/full.json.gz"
  cDestinationFilePath <- file.path(getwd(), "full.json.gz")
  utils::download.file(url = cSourceFilePath, destfile = cDestinationFilePath)

  # 2. unpack the file ---------------------------------------------------------
  R.utils::gunzip(cDestinationFilePath)

  # 3. parse JSON to list ------------------------------------------------------
  res <- jsonlite::fromJSON(txt = file.path(getwd(), "full.json"))

  # 4. create dictionary table of weather stations from -----------------------
  sf.Stations <- data.frame(
    meteostat_station_id = res$id,
    name = res$name$en,
    country = res$country,
    region_name = res$region,
    wmo_station_id = res$identifiers$wmo,
    station_latitude = res$location$latitude,
    station_longitude = res$location$longitude,
    station_elevation = res$location$elevation,
    station_timezone = res$timezone,
    inventory_start_date = as.Date(res$inventory$daily$start),
    inventory_end_date = as.Date(res$inventory$daily$end)) %>%
   sf::st_as_sf(coords = c("station_longitude", "station_latitude"), crs = 4326, agr = "constant")

  # 5. get rid of redundant downloaded file ------------------------------------
  message("removing downloaded & unpacked full.json file...")
  file.remove(file.path(getwd(), "full.json"))
  message("successfully tidied up")

  return(sf.Stations)
}


