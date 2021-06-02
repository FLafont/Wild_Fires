

#install.packages("pacman")
#library(pacman)
pacman::p_load(tigris, sf, tidyverse)


########################################  Get a list of counties with latitude and longitude.
counties_shp<-counties(year=2011)
head(arrange(counties_shp,GEOID))
head(counties)

counties <- counties_shp$GEOID

counties<-ifelse(nchar(counties)==4,paste0("0",counties),counties)

counties_shp2<-counties_shp%>%
  filter(GEOID %in% counties)%>%
  st_point_on_surface()

########################################  We want to make the following table:
########  GEOID - Lat - Lon - weather

############start by retrieving one county's weather data to initiate a table.

query <- "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/history?aggregateHours=24&combinationMethod=aggregate&startDateTime=2000-01-01T00%3A00%3A00&endDateTime=2016-01-01T00%3A00%3A00&maxStations=-1&maxDistance=-1&contentType=csv&unitGroup=metric&locationMode=array&key=ES5M5PAK79SJNAARWWBSJD98W&dataElements=default&locations=+32.5363818%2C%20-086.6444901"
test<- read.csv(query)
test$lat <- counties_shp2$INTPTLAT[1]
test$long <- counties_shp2$INTPTLON[1]
test$GEOID <- counties_shp2$GEOID[1]

working_d <- getwd()


write.csv(test, "weather.csv")



