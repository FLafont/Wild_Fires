library(tigris)
library(sf)

counties_shp<-counties(year=2011)
head(arrange(counties_shp,GEOID))

counties<-ifelse(nchar(counties)==4,paste0("0",counties),counties)

counties_shp2<-counties_shp%>%
  filter(GEOID %in% counties)%>%
  st_point_on_surface()


