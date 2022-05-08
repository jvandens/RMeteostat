
library(RMeteostat)
library(mapview)

api_key <- "j8ZnpuQu96KewFs4y4vL7TDoj7YFQKhg"
cApiKey <- api_key

# Get station list
stations <- get_meteostat_stations()
mapview(stations)

# get daily station data
station="KMIV0"
start= as.Date("2021-05-01")
end= as.Date("2021-05-31")

df.daily.station <- get_meteostat_daily_station(station, start, end, cApiKey = api_key)


# get daily point
lat = 40.65
lon = -73.75
start= as.Date("2022-03-01")
end= as.Date("2022-04-17")

df.daily.point <- get_meteostat_daily_point(lat, lon, dateStartDate = start, dateEndDate = end, cApiKey = api_key)


# get hourly point
lat = 39.463274533806
lon = -75.058413698121
start= as.Date("2022-05-01")
end= as.Date("2022-05-07")
#tz="America/New_York"

df.hourly.point <- get_meteostat_hourly_point(lat, lon, start, end, cApiKey = api_key)

# get hourly station
station="KMIV0"
start= as.Date("2022-05-01")
end= as.Date("2022-05-07")
#tz="America/New_York"

df.hourly.stn <- get_meteostat_hourly_station(station, start, end, cApiKey = api_key)
