#' Fetch hourly Meteostat data for a Lat/Lon Point
#'
#' Fetch hourly Meteostat data for a Point for at most 30 day interval
#'
#' This function fetches data made available via Meteostat's API. Please note that
#' due to the cap on the size of the window for which the data can be fetched,
#' this function can only handle requests that ask for data for up to 30 days.
#' See \url{https://dev.meteostat.net/api/point/hourly.html#endpoint} for more details
#'
#' @param lat - latitude of point
#' @param lon - longitude of point
#' @param cApiKey - character scalar, our yMeteostat's api key
#' @param dateStartDate - Date class scalar, first date of the interval for which
#' the data is to be obtained
#' @param dateEndDate - Date class scalar, last date of the interval for which
#' the data is to be obtained
#' @param tz - timezone to return, will also include utc.  Defaults to your \code{Sys.timezone()}
#' otherwise use one of \code{OlsonNames()}
#' @param model - logical scalar. Substitute missing records with statistically optimized model data
#' @return A \code{data.frame} with the data fetched; the col names are self-explanatory,
#' See API's documentation: \code{https://dev.meteostat.net/api/point/hourly.html#response} for more information
#' @export
#' @examples
#' \dontrun{
#' api_key = "------my key------"
#' lat = 39.463274533806
#' lon = -75.058413698121
#' start = as.Date("2022-05-01")
#' end = as.Date("2022-05-07")4
#' df.hourly.point <- get_meteostat_hourly_point(lat, lon, start, end, cApiKey = api_key)
#' }
#'
#'
get_meteostat_hourly_point <- function(lat, lon, dateStartDate, dateEndDate, tz = Sys.timezone(), model = TRUE, cApiKey) {

  # 1. parameters validation ---------------------------------------------------
  # 1.1. iMeteostatStationId
  if (!is.numeric(lat)) {
    stop("Error inside dtGetDailyStationDataOverUpToOneYear call: the parameter lat ",
         "is not an numeric scalar! ")
  }

  if (!is.numeric(lon)) {
    stop("Error inside dtGetDailyStationDataOverUpToOneYear call: the parameter lon ",
         "is not an numeric scalar! ")
  }

  # 1.2. cApiKey
  if (!bIsScalarOfType(objIn = cApiKey, cTypeName = "character")) {
    stop("Error inside dtGetDailyStationDataOverUpToOneYear call: the parameter cApiKey ",
         "is not a character scalar! ")
  }
  # 1.3. dateStartDate
  if (!lubridate::is.Date(x = dateStartDate) | length(x = dateStartDate) != 1) {
    stop("Error inside dtGetDailyStationDataOverUpToOneYear call: the parameter dateStartDate ",
         "is not a Date class scalar! ")
  }
  # 1.4. dateEndDate
  if (!lubridate::is.Date(x = dateEndDate) | length(x = dateEndDate) != 1) {
    stop("Error inside dtGetDailyStationDataOverUpToOneYear call: the parameter dateEndDate ",
         "is not a Date class scalar! ")
  }
  # 1.5. check consistency of the dateStartDate and dateEndDate
  if (dateStartDate > dateEndDate) {
    stop("Error inside dtGetDailyStationDataOverUpToOneYear call: the parameters dateStartDate and  ",
         "dateEndDate are inconsistent: dateStartDate precedes dateEndDate!")
  }
  # 1.6. check if the period that the data is queried for is shorter than 370 day
  iPeriodLengthInDays <- (difftime(time1 = dateEndDate, time2 = dateStartDate, units = "days") %>% as.integer()) + 1L
  if (iPeriodLengthInDays > 370) {
    stop("Error inside dtGetDailyStationDataOverUpToOneYear call: the parameters dateStartDate and  ",
         "dateEndDate indicate an interval that is longer than 370 days!!")
  }

  # 2. make query URL ----------------------------------------------------------

  base_url <- "https://api.meteostat.net/v2/point/hourly?"

  cQueryUrl <- paste0(base_url,"lat=",lat,"&lon=",lon, "&start=", dateStartDate, "&end=", dateEndDate, "&tz=", tz, "&model=", model)

  # 3. run the query to fetch the data -----------------------------------------
  # 3.1. run the API request
  res <- tryCatch(expr = {
    message("Running GET on ", cQueryUrl)
    httr::GET(cQueryUrl, config = httr::add_headers("x-api-key" = cApiKey))
  }, error = function(er) {
    stop("Error occurred during call to fetch the daily Meteostat's data from ",
         cQueryUrl, "; specific error message: ", er)
  }, finally = {
    message("after fetching via GET call")
  })

  # 3.2. retrieve the status code and print info
  iStatusCode <- as.integer(res$status_code)
  cStatusMessage <- cGetMeteostatStatusCodeMessage(iStatusCode = iStatusCode)
  message("status message: ", cStatusMessage)

  # 4. process retrieved response ----------------------------------------------
  # 4.1. parse the JSON to data.frame
  lData <- jsonlite::fromJSON(txt = httr::content(res, "text"))

  # 4.2. retrieve data
  df.data <- lData$data
  cNewColnames <- c(
    "time", "time_local", "temp_deg_C", "dewpoint_deg_C", "relative_humidity_%",
    "total_1hr_precipitation_mm", "snow_depth_mm",
    "wind_direction_deg", "wind_speed_kmph", "wind_peak_gust_kmph",
    "pressure_avg_sea_level_hPa", "daily_sunshine_min", "weather_condition_code")

  names(df.data) <- cNewColnames

  df.data$time <- lubridate::ymd_hms(df.data$time)
  df.data$time_local <- lubridate::ymd_hms(df.data$time_local, tz = tz)

  # all others as numeric
  df.data[,3:13] <- sapply(df.data[,3:13], as.numeric)

  return(df.data)
}
