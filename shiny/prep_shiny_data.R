#### PREP Shiny
#library(ggmap)
library(ggplot2)
#library(tigris)
library(leaflet)
#library(maps)
library(tidyverse)
library(sf)

# 
# 
# states <- tigris::states() %>%
#   filter(!(NAME %in% c("Hawaii","United States Virgin Islands","Commonwealth of the Northern Mariana Islands",
#                        "Alaska","Puerto Rico","Guam","American Samoa"))) %>%
#   select(STATEFP,NAME,geometry)
# 
#st_write(states,"data/sf_selected_states.shp") 
states <- st_read("data/sf_selected_states.shp") 

don <- read_csv("data/table_clustering_magic_train.csv")

etats_selec <- don %>% mutate(state = str_sub(county,1,2))%>%
  group_by(state)%>%
  count()%>%
  arrange(desc(n)) %>%
  head(5)

states <- states %>%
  mutate(etats_selec = ifelse(STATEFP %in%  etats_selec$state,
                              "Etats sélectionnés","Autre"),
         etats_selec = as_factor(etats_selec))

states <- st_transform(states,crs = "+proj=longlat +datum=WGS84")

# creating colors
factpal <- colorFactor(topo.colors(2), states$etats_selec)
