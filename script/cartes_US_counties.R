library(mapdata)
library(readxl)

state_map <- map_data('state')
counties <- map_data('county')

###on récupère les abbréviations et les noms des états de mapdata
state.abb <- append(state.abb, c("DC", "PR"))
state.name <- append(state.name, c("District of Columbia", "Puerto Rico"))

#création d'un vecteur de fires avec les noms des états en minuscule
fires$region <- map_chr(fires$STATE, function(x) { tolower(state.name[grep(x, state.abb)]) })


fires %>%
  group_by(region, subregion = tolower(FIPS_NAME)) %>%
  summarize(n_fires = n()) %>%
  right_join(counties, by = c('region', 'subregion')) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = n_fires)) + 
  geom_polygon() + 
  geom_path(color = 'white', size = 0.1) + 
  scale_fill_continuous(low = "orange", 
                        high = "darkred",
                        name = 'Number of fires') + 
  theme_map() + 
  coord_map('albers', lat0=30, lat1=40) + 
  ggtitle("US Wildfires by County 1992-2015") + 
  theme(plot.title = element_text(hjust = 0.5))


####Nos counties sélectionnés
liste_counties <- read_xlsx("data/liste_counties_selectionnes.xlsx")
liste_counties$region<-tolower(liste_counties$NAME_1)
liste_counties$subregion<-tolower(liste_counties$NAME_2)

counties_tot <- counties %>% 
  group_by(region,subregion) %>% 
  right_join(liste_counties,by=c('region','subregion')) %>%
  ggplot(aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill='light blue') + 
  geom_path(color = 'white', size = 0.1) + 
  theme_map() + 
  coord_map('albers', lat0=30, lat1=40) + 
  ggtitle("US Counties selected") + 
  theme(plot.title = element_text(hjust = 0.5))
  

ggsave(plot = counties_tot,filename="plots/UScounties_selected.png",dpi = 600)


