# ------------------------------------------------------------------------------
# This script contains function for fetching of daily Meteostat's data - for the API
# description of the data cf.: https://dev.meteostat.net/api/point/daily.html
# ------------------------------------------------------------------------------


#' Fetch daily Meteostat data for a station
#'
#' Fetch daily Meteostat data for a station for at most yearly interval
#'
#' This function fetches data made available via Meteostat's API. Please note that
#' due to the cap on the size of the window for which the data can be fetched,
#' this function can only handle requests that ask for data for up to 370 days.
#' For generalized version of this function capable of handling arbitrary
#' time intervals cf. dtGetDailyStationData
#' @param iMeteostatStationId - integer scalar, ID of the station as given by
#' Meteostat's dictionary table of weather stations
#' @param cApiKey - character scalar, Meteostat's key to use to fetch the data
#' @param dateStartDate - Date class scalar, first date of the interval for which
#' the data is to be obtained
#' @param dateEndDate - Date class scalar, last date of the interval for which
#' the data is to be obtained
#' @param model - Substitute missing records with statistically optimized model data
#' @return dataframe with the data fetched; the colnames are self-explanatory,
#' in case of doubt cf. the implementation and API's documentation:
#' https://dev.meteostat.net/api/stations/daily.html#response
#' @export
get_meteostat_daily_station <- function(iMeteostatStationId, dateStartDate, dateEndDate, model = TRUE, cApiKey) {

  # 1. parameters validation ---------------------------------------------------
  # 1.1. iMeteostatStationId
  if (!bIsScalarOfType(objIn = iMeteostatStationId, cTypeName = "character")) {
    stop("Error inside dtGetDailyStationDataOverUpToOneYear call: the parameter iMeteostatStationId ",
         "is not an integer scalar! ")
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

    base_url <- "https://api.meteostat.net/v2/stations/daily?"

  cQueryUrl <- paste0(base_url,"station=",iMeteostatStationId, "&start=", dateStartDate, "&end=", dateEndDate, "&model=", model)


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
 # dtData <- lData$data %>% data.table::as.data.table()
  df.data <- lData$data
  # 4.3. rename the columns

  cNewColnames <- c(
    "date", "temp_avg_deg_C", "temp_min_deg_C", "temp_max_deg_C",
    "total_precipitation_mm", "snow_depth_mm",
    "wind_direction_deg", "wind_speed_kmph", "wind_peak_gust_kmph",
    "pressure_avg_sea_level_hPa", "daily_sunshine_min")

  names(df.data) <- cNewColnames

  df.data$date <- as.Date(df.data$date)

  # all others as numeric
  df.data[,2:11] <- sapply(df.data[,2:11], as.numeric)

  return(df.data)
}
