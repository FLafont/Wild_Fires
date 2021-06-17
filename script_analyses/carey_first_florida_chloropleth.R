

##################################################################  Manage packages
pacman::p_load(maps, mapdata, leaflet, ggplot2, ggthemes, tidyverse, mapproj)

################################################################## Inputs

state <- "north carolina"
year <- 2015
week <- 30


################################################################## Get data
State <- str_to_title(state)

if (state == "florida") {
  threshold <- 0.4236217
  df <- read.csv("C:/Users/Carey Suehs/Documents/GitHub/Wild_Fires/models/florida_pred.csv") %>% 
    select(county, date_semaine, annee, semaine, FIRE, pred) %>% 
    filter(annee == year, semaine == week) %>% 
    rename(fips = county) %>% 
    mutate(predtrue = ifelse(pred>threshold,1,0)) %>%
    mutate(fourgroups = as.numeric(FIRE)+ predtrue) %>% 
    mutate(fourgroups = ifelse(fourgroups == 1 & FIRE == TRUE, "fn", fourgroups)) %>% 
    mutate(fourgroups = ifelse(fourgroups == 1 & FIRE == FALSE, "fp", fourgroups))
  
}

if (state == "north carolina") {
  threshold <- 0.374
  df <- read.csv("C:/Users/Carey Suehs/Documents/GitHub/Wild_Fires/models/nc_pred.csv") %>% 
    select(county, date_semaine, annee, semaine, FIRE, pred) %>% 
    filter(annee == year, semaine == week) %>% 
    rename(fips = county) %>% 
    mutate(predtrue = ifelse(pred>threshold,1,0)) %>%
    mutate(fourgroups = as.numeric(FIRE)+ predtrue) %>% 
    mutate(fourgroups = ifelse(fourgroups == 1 & FIRE == TRUE, "fn", fourgroups)) %>% 
    mutate(fourgroups = ifelse(fourgroups == 1 & FIRE == FALSE, "fp", fourgroups))
  
}



qsdf <- county.fips %>% 
  mutate(fips = ifelse(nchar(fips) == 4, paste0(0,fips), fips))

counties <- map_data('county') %>% 
  filter(region == state) %>% 
  mutate(polyname = paste0(region,",",subregion))

counties <- merge(counties, qsdf, by = "polyname", all.x = T)
counties <- merge(counties, df, by = "fips", all.x = T)

counties <- counties %>% 
  arrange(order)

dateweek <- unique(counties$date_semaine[!is.na(counties$date_semaine)])

################################################################ Risk = pred

ggplot(data = counties, aes(x = long, y = lat, group = group, fill = pred))+
  geom_polygon()+ 
  geom_path(color = 'white', size = 0.1)+ 
  scale_fill_continuous(low = "orange", 
                        high = "darkred",
                        name = 'Risk level') + 
  theme_map() + 
  coord_map('albers', lat0=30, lat1=40) + 
  ggtitle(paste0(State, " (week: ",  dateweek,  "): Wildfire risk level per county")) + 
  theme(plot.title = element_text(hjust = 0.5))


#################################################################  2 risk groups

cols <- c("1" = "red", "0" = "white", "NA" = "grey40")

ggplot(data = counties, aes(x = long, y = lat, group = group, fill = as.character(predtrue)))+
  geom_polygon()+ 
  geom_path(color = 'grey40', size = 0.1)+ 
  scale_fill_manual(values = c("1" = "red", "0" = "#D3EAF4", "NA" = "grey40"),
                      name = "Fire risk")+
  theme_map() + 
  coord_map('albers', lat0=30, lat1=40) + 
  ggtitle(paste0(State, " (week: ",  dateweek,  "): Wildfire risk level per county")) + 
  theme(plot.title = element_text(hjust = 0.5))

########################################################################## Chloropleth with four colors

cols <- c("2" = "red", "0" = "#D3EAF4", "NA" = "grey40", "fn" = "orange", "fp" = "#F182A4")

ggplot(data = counties, aes(x = long, y = lat, group = group, fill = as.character(fourgroups)))+
  geom_polygon()+ 
  geom_path(color = 'grey40', size = 0.1)+ 
  scale_fill_manual(values = cols,
                    name = "Fire risk",
                    labels = c("TN; low risk",
                               "VP: high risk + fire",
                               "FN: low risk + fire",
                               "FP: high risk"))+
  theme_map() + 
  coord_map('albers', lat0=30, lat1=40) + 
  ggtitle(paste0(State, " (week: ",  dateweek,  "): Wildfire risk level per county")) + 
  theme(plot.title = element_text(hjust = 0.5))
