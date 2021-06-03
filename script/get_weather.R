

#install.packages("pacman")
#library(pacman)
pacman::p_load(tigris, sf, tidyverse)


########################################  Get a list of counties with latitude and longitude.
#counties_shp<-counties(year=2011)
#head(arrange(counties_shp,GEOID))
#head(counties)

#counties <- counties_shp$GEOID

#counties<-ifelse(nchar(counties)==4,paste0("0",counties),counties)

#counties_shp2<-counties_shp%>%
  #filter(GEOID %in% counties)%>%
  #st_point_on_surface()

####save that county file locally because it takes time to do
working_d <- getwd()
working_d <- str_sub(working_d, end = -7)
yuio <- paste0(working_d, "data/counties_shp2.rds")
#write_rds(counties_shp2, file = yuio)

counties_shp2 <- read_rds(yuio)

########################################  Download data from API
############################################First batch
for (i in 1:5) {
  lat <- counties_shp2$INTPTLAT[i]
  long <- counties_shp2$INTPTLON[i]
  gid <- counties_shp2$GEOID[i]
  query_1 <- "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/history?aggregateHours=24&combinationMethod=aggregate&startDateTime=2000-01-01T00%3A00%3A00&endDateTime=2016-01-01T00%3A00%3A00&maxStations=-1&maxDistance=-1&contentType=csv&unitGroup=metric&locationMode=array&key=ES5M5PAK79SJNAARWWBSJD98W&dataElements=default&locations="
  query_2 <- lat
  query_3 <- "%2C%20"
  query_4 <- long
  
  query <- paste0(query_1,
                  query_2,
                  query_3,
                  query_4)
  
  test <- read.csv(query)
  test$lat <- counties_shp2$INTPTLAT[i]
  test$long <- counties_shp2$INTPTLON[i]
  test$GEOID <- counties_shp2$GEOID[i]
  
  
  writ2me <- paste0(working_d, "data/weather/GEOID_", gid, ".csv")
  
  write.csv(test, writ2me)
  print(i)
}



#################################Second batch

for (i in 6:100) {
  lat <- counties_shp2$INTPTLAT[i]
  long <- counties_shp2$INTPTLON[i]
  gid <- counties_shp2$GEOID[i]
  query_1 <- "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/history?aggregateHours=24&combinationMethod=aggregate&startDateTime=2000-01-01T00%3A00%3A00&endDateTime=2016-01-01T00%3A00%3A00&maxStations=-1&maxDistance=-1&contentType=csv&unitGroup=metric&locationMode=array&key=ES5M5PAK79SJNAARWWBSJD98W&dataElements=default&locations="
  query_2 <- lat
  query_3 <- "%2C%20"
  query_4 <- long
  
  query <- paste0(query_1,
                  query_2,
                  query_3,
                  query_4)
  
  test <- read.csv(query)
  test$lat <- counties_shp2$INTPTLAT[i]
  test$long <- counties_shp2$INTPTLON[i]
  test$GEOID <- counties_shp2$GEOID[i]
  
  
  writ2me <- paste0(working_d, "data/weather/GEOID_", gid, ".csv")
  
  write.csv(test, writ2me)
  print(i)
}

#################################third batch

for (i in 101:300) {
  lat <- counties_shp2$INTPTLAT[i]
  long <- counties_shp2$INTPTLON[i]
  gid <- counties_shp2$GEOID[i]
  query_1 <- "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/history?aggregateHours=24&combinationMethod=aggregate&startDateTime=2000-01-01T00%3A00%3A00&endDateTime=2016-01-01T00%3A00%3A00&maxStations=-1&maxDistance=-1&contentType=csv&unitGroup=metric&locationMode=array&key=ES5M5PAK79SJNAARWWBSJD98W&dataElements=default&locations="
  query_2 <- lat
  query_3 <- "%2C%20"
  query_4 <- long
  
  query <- paste0(query_1,
                  query_2,
                  query_3,
                  query_4)
  
  test <- read.csv(query)
  test$lat <- counties_shp2$INTPTLAT[i]
  test$long <- counties_shp2$INTPTLON[i]
  test$GEOID <- counties_shp2$GEOID[i]
  
  
  writ2me <- paste0(working_d, "data/weather/GEOID_", gid, ".csv")
  
  write.csv(test, writ2me)
  print(i)
}


########################################whatis wrong with 278?
i <- 278
lat <- counties_shp2$INTPTLAT[i]
long <- counties_shp2$INTPTLON[i]
gid <- counties_shp2$GEOID[i]
query_1 <- "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/history?aggregateHours=24&combinationMethod=aggregate&startDateTime=2000-01-01T00%3A00%3A00&endDateTime=2016-01-01T00%3A00%3A00&maxStations=-1&maxDistance=-1&contentType=csv&unitGroup=metric&locationMode=array&key=ES5M5PAK79SJNAARWWBSJD98W&dataElements=default&locations="
query_2 <- lat
query_3 <- "%2C%20"
query_4 <- long

query <- paste0(query_1,
                query_2,
                query_3,
                query_4)

test <- read.csv(query)
test$lat <- counties_shp2$INTPTLAT[i]
test$long <- counties_shp2$INTPTLON[i]
test$GEOID <- counties_shp2$GEOID[i]


writ2me <- paste0(working_d, "data/weather/GEOID_", gid, ".csv")

write.csv(test, writ2me)
print(i)

query

###########################################finish up the rest of batch up to 300 lines

for (i in 279:300) {
  lat <- counties_shp2$INTPTLAT[i]
  long <- counties_shp2$INTPTLON[i]
  gid <- counties_shp2$GEOID[i]
  query_1 <- "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/history?aggregateHours=24&combinationMethod=aggregate&startDateTime=2000-01-01T00%3A00%3A00&endDateTime=2016-01-01T00%3A00%3A00&maxStations=-1&maxDistance=-1&contentType=csv&unitGroup=metric&locationMode=array&key=ES5M5PAK79SJNAARWWBSJD98W&dataElements=default&locations="
  query_2 <- lat
  query_3 <- "%2C%20"
  query_4 <- long
  
  query <- paste0(query_1,
                  query_2,
                  query_3,
                  query_4)
  
  test <- read.csv(query)
  test$lat <- counties_shp2$INTPTLAT[i]
  test$long <- counties_shp2$INTPTLON[i]
  test$GEOID <- counties_shp2$GEOID[i]
  
  
  writ2me <- paste0(working_d, "data/weather/GEOID_", gid, ".csv")
  
  write.csv(test, writ2me)
  print(i)
}

###########################################  Batch 4 up to 500.  
#lines that don't work: i = 379, 383
for (i in 378:500) {
  lat <- counties_shp2$INTPTLAT[i]
  long <- counties_shp2$INTPTLON[i]
  gid <- counties_shp2$GEOID[i]
  query_1 <- "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/history?aggregateHours=24&combinationMethod=aggregate&startDateTime=2000-01-01T00%3A00%3A00&endDateTime=2016-01-01T00%3A00%3A00&maxStations=-1&maxDistance=-1&contentType=csv&unitGroup=metric&locationMode=array&key=ES5M5PAK79SJNAARWWBSJD98W&dataElements=default&locations="
  query_2 <- lat
  query_3 <- "%2C%20"
  query_4 <- long
  
  query <- paste0(query_1,
                  query_2,
                  query_3,
                  query_4)
  
  test <- read.csv(query)
  test$lat <- counties_shp2$INTPTLAT[i]
  test$long <- counties_shp2$INTPTLON[i]
  test$GEOID <- counties_shp2$GEOID[i]
  
  
  writ2me <- paste0(working_d, "data/weather/GEOID_", gid, ".csv")
  
  write.csv(test, writ2me)
  print(i)
}

for (i in 380:382) {
  lat <- counties_shp2$INTPTLAT[i]
  long <- counties_shp2$INTPTLON[i]
  gid <- counties_shp2$GEOID[i]
  query_1 <- "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/history?aggregateHours=24&combinationMethod=aggregate&startDateTime=2000-01-01T00%3A00%3A00&endDateTime=2016-01-01T00%3A00%3A00&maxStations=-1&maxDistance=-1&contentType=csv&unitGroup=metric&locationMode=array&key=ES5M5PAK79SJNAARWWBSJD98W&dataElements=default&locations="
  query_2 <- lat
  query_3 <- "%2C%20"
  query_4 <- long
  
  query <- paste0(query_1,
                  query_2,
                  query_3,
                  query_4)
  
  test <- read.csv(query)
  test$lat <- counties_shp2$INTPTLAT[i]
  test$long <- counties_shp2$INTPTLON[i]
  test$GEOID <- counties_shp2$GEOID[i]
  
  
  writ2me <- paste0(working_d, "data/weather/GEOID_", gid, ".csv")
  
  write.csv(test, writ2me)
  print(i)
}

for (i in 384:500) {
  lat <- counties_shp2$INTPTLAT[i]
  long <- counties_shp2$INTPTLON[i]
  gid <- counties_shp2$GEOID[i]
  query_1 <- "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/history?aggregateHours=24&combinationMethod=aggregate&startDateTime=2000-01-01T00%3A00%3A00&endDateTime=2016-01-01T00%3A00%3A00&maxStations=-1&maxDistance=-1&contentType=csv&unitGroup=metric&locationMode=array&key=ES5M5PAK79SJNAARWWBSJD98W&dataElements=default&locations="
  query_2 <- lat
  query_3 <- "%2C%20"
  query_4 <- long
  
  query <- paste0(query_1,
                  query_2,
                  query_3,
                  query_4)
  
  test <- read.csv(query)
  test$lat <- counties_shp2$INTPTLAT[i]
  test$long <- counties_shp2$INTPTLON[i]
  test$GEOID <- counties_shp2$GEOID[i]
  
  
  writ2me <- paste0(working_d, "data/weather/GEOID_", gid, ".csv")
  
  write.csv(test, writ2me)
  print(i)
}


####################Batch 500 to 800

for (i in 501:571) {
  lat <- counties_shp2$INTPTLAT[i]
  long <- counties_shp2$INTPTLON[i]
  gid <- counties_shp2$GEOID[i]
  query_1 <- "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/history?aggregateHours=24&combinationMethod=aggregate&startDateTime=2000-01-01T00%3A00%3A00&endDateTime=2016-01-01T00%3A00%3A00&maxStations=-1&maxDistance=-1&contentType=csv&unitGroup=metric&locationMode=array&key=ES5M5PAK79SJNAARWWBSJD98W&dataElements=default&locations="
  query_2 <- lat
  query_3 <- "%2C%20"
  query_4 <- long
  
  query <- paste0(query_1,
                  query_2,
                  query_3,
                  query_4)
  
  test <- read.csv(query)
  test$lat <- counties_shp2$INTPTLAT[i]
  test$long <- counties_shp2$INTPTLON[i]
  test$GEOID <- counties_shp2$GEOID[i]
  
  
  writ2me <- paste0(working_d, "data/weather/GEOID_", gid, ".csv")
  
  write.csv(test, writ2me)
  print(i)
}

####################Batch 500 to 800

for (i in 573:800) {
  lat <- counties_shp2$INTPTLAT[i]
  long <- counties_shp2$INTPTLON[i]
  gid <- counties_shp2$GEOID[i]
  query_1 <- "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/history?aggregateHours=24&combinationMethod=aggregate&startDateTime=2000-01-01T00%3A00%3A00&endDateTime=2016-01-01T00%3A00%3A00&maxStations=-1&maxDistance=-1&contentType=csv&unitGroup=metric&locationMode=array&key=ES5M5PAK79SJNAARWWBSJD98W&dataElements=default&locations="
  query_2 <- lat
  query_3 <- "%2C%20"
  query_4 <- long
  
  query <- paste0(query_1,
                  query_2,
                  query_3,
                  query_4)
  
  test <- read.csv(query)
  test$lat <- counties_shp2$INTPTLAT[i]
  test$long <- counties_shp2$INTPTLON[i]
  test$GEOID <- counties_shp2$GEOID[i]
  
  
  writ2me <- paste0(working_d, "data/weather/GEOID_", gid, ".csv")
  
  write.csv(test, writ2me)
  print(i)
}

####################Batch 801 to 1023

for (i in 801:1023) {
  lat <- counties_shp2$INTPTLAT[i]
  long <- counties_shp2$INTPTLON[i]
  gid <- counties_shp2$GEOID[i]
  query_1 <- "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/history?aggregateHours=24&combinationMethod=aggregate&startDateTime=2000-01-01T00%3A00%3A00&endDateTime=2016-01-01T00%3A00%3A00&maxStations=-1&maxDistance=-1&contentType=csv&unitGroup=metric&locationMode=array&key=ES5M5PAK79SJNAARWWBSJD98W&dataElements=default&locations="
  query_2 <- lat
  query_3 <- "%2C%20"
  query_4 <- long
  
  query <- paste0(query_1,
                  query_2,
                  query_3,
                  query_4)
  
  test <- read.csv(query)
  test$lat <- counties_shp2$INTPTLAT[i]
  test$long <- counties_shp2$INTPTLON[i]
  test$GEOID <- counties_shp2$GEOID[i]
  
  
  writ2me <- paste0(working_d, "data/weather/GEOID_", gid, ".csv")
  
  write.csv(test, writ2me)
  print(i)
}

####################Batch 1025 to 1613

for (i in 1025:1613) {
  lat <- counties_shp2$INTPTLAT[i]
  long <- counties_shp2$INTPTLON[i]
  gid <- counties_shp2$GEOID[i]
  query_1 <- "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/history?aggregateHours=24&combinationMethod=aggregate&startDateTime=2000-01-01T00%3A00%3A00&endDateTime=2016-01-01T00%3A00%3A00&maxStations=-1&maxDistance=-1&contentType=csv&unitGroup=metric&locationMode=array&key=ES5M5PAK79SJNAARWWBSJD98W&dataElements=default&locations="
  query_2 <- lat
  query_3 <- "%2C%20"
  query_4 <- long
  
  query <- paste0(query_1,
                  query_2,
                  query_3,
                  query_4)
  
  test <- read.csv(query)
  test$lat <- counties_shp2$INTPTLAT[i]
  test$long <- counties_shp2$INTPTLON[i]
  test$GEOID <- counties_shp2$GEOID[i]
  
  
  writ2me <- paste0(working_d, "data/weather/GEOID_", gid, ".csv")
  
  write.csv(test, writ2me)
  print(i)
}

####################Batch 1615 to 3234

for (i in 1614:3234) {
  lat <- counties_shp2$INTPTLAT[i]
  long <- counties_shp2$INTPTLON[i]
  gid <- counties_shp2$GEOID[i]
  query_1 <- "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/history?aggregateHours=24&combinationMethod=aggregate&startDateTime=2000-01-01T00%3A00%3A00&endDateTime=2016-01-01T00%3A00%3A00&maxStations=-1&maxDistance=-1&contentType=csv&unitGroup=metric&locationMode=array&key=ES5M5PAK79SJNAARWWBSJD98W&dataElements=default&locations="
  query_2 <- lat
  query_3 <- "%2C%20"
  query_4 <- long
  
  query <- paste0(query_1,
                  query_2,
                  query_3,
                  query_4)
  
  test <- read.csv(query)
  test$lat <- counties_shp2$INTPTLAT[i]
  test$long <- counties_shp2$INTPTLON[i]
  test$GEOID <- counties_shp2$GEOID[i]
  
  
  writ2me <- paste0(working_d, "data/weather/GEOID_", gid, ".csv")
  
  write.csv(test, writ2me)
  print(i)
}






