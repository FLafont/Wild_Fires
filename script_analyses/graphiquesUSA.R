library(RSQLite)
library(dbplyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(xts)
library(ggfortify)
library(ggthemes)
library(maps)
library(mapdata)
library(leaflet)

con <- dbConnect(drv=RSQLite::SQLite(), 
                        dbname="data/FPA_FOD_20170508.sqlite")
dbDisconnect(con)

tables <- dbListTables(con)
fires <- dbReadTable(con, tables[2])

fires %>% 
  group_by(FIRE_YEAR) %>%
  summarize(n_fires = n()) %>%
  ggplot(aes(x = FIRE_YEAR, y = n_fires/1000)) + 
  geom_bar(stat = 'identity', fill = 'orange') +
  geom_smooth(method = 'lm', se = FALSE, linetype = 'dashed', size = 0.4, color = 'red') + 
  labs(x = '', y = 'Number of wildfires (thousands)', title = 'US Wildfires by Year')

fires %>% 
  group_by(STAT_CAUSE_DESCR) %>%
  summarize(mean_size = mean(FIRE_SIZE, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(STAT_CAUSE_DESCR, mean_size), y = mean_size)) +
  geom_bar(stat = 'identity', fill = 'orange') + 
  coord_flip() + 
  labs(x = '', y = 'Acres', title = 'Average Wildfire Size by Cause')

state.abb <- append(state.abb, c("DC", "PR"))
state.name <- append(state.name, c("District of Columbia", "Puerto Rico"))

fires$region <- map_chr(fires$STATE, function(x) { tolower(state.name[grep(x, state.abb)]) })

state_map <- map_data('state')

fires %>% 
  select(region) %>%
  group_by(region) %>%
  summarize(n = n()) %>%
  right_join(state_map, by = 'region') %>%
  ggplot(aes(x = long, y = lat, group = group, fill = n)) + 
  geom_polygon() + 
  geom_path(color = 'white') + 
  scale_fill_continuous(low = "orange", 
                        high = "darkred",
                        name = 'Number of fires') + 
  theme_map() + 
  coord_map('albers', lat0=30, lat1=40) + 
  ggtitle("US Wildfires, 1992-2015") + 
  theme(plot.title = element_text(hjust = 0.5))

###Carte par county

counties <- map_data('county')

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