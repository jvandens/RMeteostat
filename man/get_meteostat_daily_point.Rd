% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_meteostat_daily_point.R
\name{get_meteostat_daily_point}
\alias{get_meteostat_daily_point}
\title{Fetch daily Meteostat data for a Lat/Lon Point}
\usage{
get_meteostat_daily_point(
  lat,
  lon,
  alt = NULL,
  dateStartDate,
  dateEndDate,
  model = TRUE,
  cApiKey
)
}
\arguments{
\item{lat}{- latitude of point (required)}

\item{lon}{- longitude of point (required)}

\item{alt}{- altitude of point}

\item{dateStartDate}{- Date class scalar, first date of the interval for which
the data is to be obtained (required)}

\item{dateEndDate}{- Date class scalar, last date of the interval for which
the data is to be obtained (required)}

\item{model}{- logical scalar. Substitute missing records with statistically optimized model data}

\item{cApiKey}{- character scalar, your meteostat api key to use to fetch the data}
}
\value{
A \code{data.frame} with the data fetched; the column names are self-explanatory,
See API's documentation: \code{https://dev.meteostat.net/api/point/daily.html#response}
}
\description{
Fetch daily Meteostat data for a Point for at most 10-year interval
}
\details{
This function fetches data made available via Meteostat's API. Please note that
due to the cap on the size of the window for which the data can be fetched,
this function can only handle requests that ask for data for up to 10 years.
See \url{https://dev.meteostat.net/api/point/daily.html} for more details
}
\examples{
\dontrun{
api_key = "------my key------"
lat = 40.65
lon = -73.75
start = as.Date("2022-03-01")
end = as.Date("2022-04-17")
df.daily.point <- get_meteostat_daily_point(lat, lon,
dateStartDate = start, dateEndDate = end, cApiKey = api_key)
}


}
