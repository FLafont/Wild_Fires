### Statistiques descriptives sur les counties
library(tidyverse)
# don <- read_csv("data/table_clustering_magic_train.csv")
# 
# don %>% 
#   mutate(state = str_sub(county,1,2))%>%
#   group_by(state)%>%
#   count()%>%
#   arrange(desc(n)) %>%
#   head(5)
# 
# counties_selec <- read_csv("data/liste_counties_selectionnes.csv") %>%
#   mutate(fips = ifelse(nchar(fips)<5,paste0("0",fips),fips)) %>%
#   filter(NAME_1 %in% c("California","North Carolina","Florida",
#                        "Georgia","Mississippi"))
# 
# write_csv(counties_selec,"data/counties_selectionnes_stats_desc_shiny.csv")
# train <- read_csv("data/magic_train_feature_eng.csv") %>%
#   filter(county %in% counties_selec$fips)
# 
# test <- read_csv("data/magic_test_feature_eng.csv") %>%
#   filter(county %in% counties_selec$fips)
# 
# ### STATS DESC LAND COVER 
# counties_lc <- bind_rows(
#   train %>% select(county,annee,X21_Developed_open_space:X12_Perennial_ice_snow)%>%
#     distinct(.keep_all = TRUE),
#   test %>% select(county,annee,X21_Developed_open_space:X12_Perennial_ice_snow)%>%
#     distinct(.keep_all = TRUE)
# ) %>%
#   filter(annee %in% c("2010","2015")) %>%
#   inner_join(select(counties_selec,NAME_1,NAME_2,county=fips))
# 
# # Passage format long
# 
# unique(df_lc$type_lc)
# 
# df_lc <- counties_lc %>%
#   pivot_longer(c(X21_Developed_open_space:X12_Perennial_ice_snow),
#                names_to ="type_lc" ,
#                values_to = "surface")%>%
#   mutate(broad_type_lc = case_when(
#     type_lc %in% c("X21_Developed_open_space","X22_Developed_low_intensity",
#                    "X23_Developed_medium_intensity","X24_Developed_high_intensity")~"Developped",
#     type_lc =="X31_Barren_land_rock_sand_clay"~"Barren",
#     type_lc %in% c("X41_Deciduous_forest","X42_Evergreen_forest","X43_Mixed_forest")~"Forest",
#     type_lc %in% c("X52_Shrub_scrub")~"Schrub Scrub",
#     type_lc %in% c("X71_Grassland_herbaceous")~"Herbaceous",
#     type_lc %in% c("X81_Pasture_hay","X82_Cultivated_crops" )~"Cultivated",
#     type_lc %in% c("X90_Woody_wetlands","X95_Emergent_herbaceous_wetlands") ~"Wetlands",
#     TRUE ~"Other")
#   ) %>%
#   group_by(annee,NAME_1,NAME_2,county,broad_type_lc)%>%
#   summarise(surface = sum(surface))

# 
# write_csv(df_lc,"data/landcover_stat_desc_county_shiny.csv")






counties_selec <- read_csv("data/counties_selectionnes_stats_desc_shiny.csv")
df_lc <- read_csv("data/landcover_stat_desc_county_shiny.csv")



pie_chart_lc <- function(fips,year){
  temp <- df_lc %>%
    filter(county==fips,annee==year)%>%
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
    ggrepel::geom_text_repel(aes(y = ypos, label = paste0(prop," %")), color = "black", size=6)+
    labs(fill="Land cover")+
    scale_fill_brewer(palette = "Set3")

}


pie_chart_lc("06001","2010")
pie_chart_lc("06001","2015")

# Graphiques PIE Interactifs

library(plotly)
# Je peux pas le tester mon R studio bug

pie_chart_plotly <- function(fips,year){
  
  temp <- df_lc %>%
    filter(county==fips,annee==year)%>%
    arrange(desc(broad_type_lc))%>%
    mutate(prop = round(surface/sum(surface)*100,1)) %>%
    filter(prop>.1)%>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  
  plot_ly(data=temp,labels=~broad_type_lc, values=~prop, type="pie") %>% 
    layout(title = "Occupation et utilisation du sol",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
}


## Vecteurs pour shiny
state_names <- unique(counties_selec$NAME_1)
years <- c("2010","2015")



#### Stats DESC METEO 
train <- read_csv("data/magic_train_feature_eng.csv") %>%
  filter(county %in% counties_selec$fips)

test <- read_csv("data/magic_test_feature_eng.csv") %>%
  filter(county %in% counties_selec$fips)

counties_meteo <- bind_rows(
  train %>% select(county,annee,date_semaine,av_temp_hi:av_humidity),
  test%>% select(county,annee,date_semaine,av_temp_hi:av_humidity)
) %>%
  inner_join(select(counties_selec,NAME_1,NAME_2,county=fips))

# Trois dernières années de météo ça suffit
counties_meteo <- counties_meteo %>%
  filter(annee %in% c("2013","2014","2015")) %>%
  mutate(mois = str_sub(date_semaine,6,7),
         mois2 = case_when(
           mois=="01"~"janvier",
           mois=="02"~"février",
           mois=="03"~"mars",
           mois=="04"~"avril",
           mois=="05"~"mai",
           mois=="06"~"juin",
           mois=="07"~"juillet",
           mois=="08"~"août",
           mois=="09"~"septembre",
           mois=="10"~"octobre",
           mois=="11"~"novembre",
           mois=="12"~"décembre"
         ),
         mois2=fct_reorder(.f=mois2,.x=as.numeric(mois))) %>%
  group_by(annee,mois,mois2,NAME_2,NAME_1,county)%>%
  summarise(
    max_temp = max(av_temp_hi),
    min_temp = min(av_temp_lo),
    temp_moyenne = mean(av_temp_mean),
    precip_moyenne = mean(av_precip),
    couv_nuageuse_moyenne = mean(av_cloud_cover),
    humidite_moyenne = mean(av_humidity)
  )


## plot meteo counties
plot_meteo_counties <- function(year,fips,var){
  variable <- ensym(var)
  counties_meteo %>%
    filter(annee==year,
           county==fips)%>%
    ggplot(aes(x=mois2,y=!!variable,group=fips))+
    geom_line(color="grey",size=2)+
    geom_point(color="blue")+
    theme_light()+
    labs(y="degrés (C°)",x="")
    
}
plot_meteo_counties("2014","06001",temp_moyenne)
## plot meteo counties
plot_meteo_counties2 <- function(year,fips,var){
  variable <- ensym(var)
  counties_meteo %>%
    filter(annee==year,
           county==fips)%>%
    ggplot(aes(x=mois2,y=!!variable,group=fips))+
    geom_col(fill="blue",alpha=.5,color="yellow")+
    theme_light()+
    labs(y="degrés (C°)",x="")
    
}
plot_meteo_counties2("2014","06001",temp_moyenne)


## Plusieurs lignes sur un graphique 
counties_meteo_long <- counties_meteo %>%
  pivot_longer(c(max_temp:temp_moyenne),
               names_to = "temperature",
               values_to = "degré") %>%
  pivot_longer(c(precip_moyenne:humidite_moyenne),
               names_to = "indicateurs_humidite",
               values_to = "valeur") %>%
  mutate(temperature = recode(temperature,
                "max_temp"="Température maximale",
                "min_temp" = "Température minimale",
                "temp_moyenne"="Température moyenne"),
         indicateurs_humidite = recode(indicateurs_humidite,
                                       "precip_moyenne"="Précipitations moyennes",
                                       "couv_nuageuse_moyenne"="Couverture nuageuse moyenne",
                                       "humidite_moyenne"="Humidité "))

plot_temperature_counties <- function(year,fips){

  counties_meteo_long %>%
    filter(annee==year,
           county==fips)%>%
    ggplot(aes(x=mois2,y=degré,group=temperature,color=temperature))+
    geom_line()+
    geom_point(aes(shape=temperature))+
    theme_light()+
    labs(y="degrés (C°)",x="",color="",shape="")
  
}
plot_temperature_counties("2014","06001")

# Le dernier n'est pas bon probablement car pas la même unité 
## pour les précipitations

plot_humidite_counties <- function(year,fips){
  
  counties_meteo_long %>%
    filter(annee==year,
           county==fips)%>%
    ggplot(aes(x=mois2,y=valeur,group=indicateurs_humidite,color=indicateurs_humidite))+
    geom_line()+
    geom_point(aes(shape=indicateurs_humidite))+
    theme_light()+
    labs(y="degrés (C°)",x="",color="",shape="")
  
}
plot_humidite_counties("2014","06001")
