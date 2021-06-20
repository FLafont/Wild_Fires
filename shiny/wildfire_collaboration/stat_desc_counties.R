### Statistiques descriptives sur les counties
library(tidyverse)


counties_selec <- read_csv("counties_selectionnes_stats_desc_shiny.csv")
df_lc <- read_csv("landcover_stat_desc_county_shiny.csv")

pie_chart_lc <- function(state,name_county,year){
  temp <- df_lc %>%
    filter(NAME_1==state,NAME_2==name_county,annee==year)%>%
    arrange(desc(broad_type_lc))%>%
    mutate(prop = round(surface/sum(surface)*100,1)) %>%
    filter(prop>.1)%>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  
  # print(temp)
  ggplot(temp, aes(x = "", y = prop, fill = broad_type_lc)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    theme_void()+
    #geom_text(aes(y = ypos, label = prop), color = "white", size=6)+
    labs(fill="Land cover")+
    scale_fill_brewer(palette = "Set3")
  
}

#### Stats DESC METEO 

counties_meteo <- read_csv("counties_meteo.csv") %>% 
  mutate(mois2=fct_reorder(mois2,mois))

## plot meteo counties
plot_meteo_counties <- function(state,year,name_county,var){
  variable <- ensym(var)
  counties_meteo %>%
    filter(annee==year,
           NAME_1==state,
           NAME_2==name_county)%>%
    ggplot(aes(x=fct_reorder(mois2,mois),y=!!variable,group=name_county))+
    geom_line(color="grey",size=2)+
    geom_point(color="blue")+
    theme_light()+
    labs(y="degés (C°)",x="")
    
}

## plot meteo counties
plot_meteo_counties2 <- function(state,year,name_county,var,unite){
  variable <- ensym(var)
  counties_meteo %>%
    filter(annee==year,
           NAME_1==state,
           NAME_2==name_county)%>%
    ggplot(aes(x=fct_reorder(mois2,mois),y=!!variable,group=name_county))+
    geom_col(fill="blue",alpha=.5,color="yellow")+
    theme_light()+
    labs(y=unite,x="")
    
}


plot_counties <- function(state,year,name_county){
  # fips <- filter(counties_selec, NAME_2 == county) %>%
  #          pull(fips)
  p1 <- pie_chart_lc(state,name_county,ifelse(year<2015,2010,2015))
  p2 <- plot_meteo_counties(state,year, name_county,var = temp_moyenne)
  p3 <- plot_meteo_counties2(state,year, name_county,var = humidite_moyenne,"humiditée (g/m^3)")
  p4 <- plot_meteo_counties2(state,year, name_county,var = precip_moyenne,"précipitations (mm)")
  
  plot <-  ggarrange(p1,p2,
                     p3,p4,
                     ncol = 2, nrow = 2)
  annotate_figure(
    plot,
    top =text_grob(name_county, color = "darkgrey", face = "bold", size = 15) 
  )
}


## CARTRE COUNTIES 
# map_county_selec <- function(state_name,county_name){
#   tigris::counties(state="California") %>%
#     filter(NAME=="Plumas") %>%
#     ggplot()+geom_sf()
# }
# map_county_selec <- function(state_name,county_name){
#   x <- tigris::counties(state=state_name) %>%
#     filter(NAME==county_name) %>%
#     st_transform(crs='+proj=longlat +datum=WGS84')
#   
#   leaflet(x) %>% 
#     addTiles()%>%
#     addPolygons(
#       stroke = FALSE,
#       label =   ~NAME)
# }

map_county_selec <- function(state_name,county_name){
  x <- counties %>%
    filter(NAME_1==state_name,
           NAME==county_name) %>%
    #filter(NAME==county_name) %>%
    st_transform(crs='+proj=longlat +datum=WGS84')
  
  leaflet(x) %>% 
    addTiles()%>%
    addPolygons(
      stroke = FALSE,
      label =   ~NAME)
}

