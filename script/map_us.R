########### US MAP #############
library(raster)
library(leaflet)
library(sf)
library(rgeos)
us <- getData('GADM', country='USA', level=2)
us_states <- getData('GADM', country='USA', level=1)

## convert 
us_states <- st_as_sf(us_states) %>%
  filter(NAME_1!="Alaska")%>%
  st_transform(crs=4326)



# sans alaska 
us_states %>%
  ggplot()+
  geom_sf()


map_us <- leaflet() %>%
  addPolygons(data=us_states,
              popupOptions = list(maxHeight=150,maxWidth=200))

map_us
