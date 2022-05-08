# RMeteostat

`RMeteostat` is a wrapper around the [Meteostat.net](https://dev.meteostat.net/api/) API for getting data from Meteostat API into R

## Origin

This package is a fork of [okeanos.meteostat](https://github.com/wegar-2/okeanos.meteostat) the primary changes include:

-   Adding additional endpoints
-   Converting to returning `data.frame` rather than `data.table` workflows

## Installation

Install from github with `devtools`:

```{r}
devtools::install_github("jvandens/RMeteostat")
```

## Functions Overview

`RMeteostat` includes the following functions, sorted by the order they would generally be applied:

-   `get_meteostat_stations()` returns an `sf` object of all the current stations meteostat has. The returned station IDs are used with the `get_*_station()` family of functions

-   `get_meteostat_daily_station()` returns a `data.frame` of daily observations at the supplied station ID

-   `get_meteostat_hourly_station()` returns a `data.frame` of hourly observations at the supplied station ID with option to convert timezone

-   `get_meteostat_daily_point()` returns a `data.frame` of daily observations at a single lat/lon location using meteostat's model interpolation

-   `get_meteostat_hourly_point()` returns a `data.frame` of hourly observations at a single lat/lon location using meteostat's model interpolation with option to convert timezone

## Getting Started

The following examples shows a typical workflow to download data from a single location.

```{r}

library(RMeteostat)
library(mapview)

api_key <- "-------my key------------"

# Get station list and map them
stations <- get_meteostat_stations()
mapview(stations)

# get daily station data
station = "KMIV0"
start = as.Date("2021-05-01")
end = as.Date("2021-05-31")

df.daily.station <- get_meteostat_daily_station(station, start, end, cApiKey = api_key)

# get daily point
lat = 40.65
lon = -73.75
start = as.Date("2022-03-01")
end = as.Date("2022-04-17")

df.daily.point <- get_meteostat_daily_point(lat, lon, dateStartDate = start, dateEndDate = end, cApiKey = api_key)

# get hourly point
lat = 39.463274533806
lon = -75.058413698121
start = as.Date("2022-05-01")
end = as.Date("2022-05-07")

df.hourly.point <- get_meteostat_hourly_point(lat, lon, start, end, cApiKey = api_key)

# get hourly station
station = "KMIV0"
start = as.Date("2022-05-01")
end = as.Date("2022-05-07")

df.hourly.stn <- get_meteostat_hourly_station(station, start, end, cApiKey = api_key)

```

## Legal disclaimer

This package is developed to facilitate work with meteorological data made available by the Meteostat API (<https://dev.meteostat.net/>).

I hereby acknowledge that Meteostat, c/o Christian Lamprecht, Plattenseestraße 5, 69168 Wiesloch, Germany is the provider of the data and that the legal ownership of data fetched using this package has to be checked on the case-by-case basis, depending on the provider of the data.

This complexity is due to the fact that Meteostat acts as an aggregator of the data. For more background information please see <https://meteostat.net/en/legal>

## Contact

Author: Jaak Van den Sype ([jaak.vandensype\@hdrinc.com](mailto:jaak.vandensype@hdrinc.com))

Company: HDR Inc.
