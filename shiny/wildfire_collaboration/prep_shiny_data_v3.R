#### PREP Shiny
#library(ggmap)
library(ggplot2)
#library(tigris)
library(leaflet)
#library(maps)
library(tidyverse)
library(sf)
library(ggpubr)

# 
# 
# states <- tigris::states() %>%
#    filter(!(NAME %in% c("Hawaii","United States Virgin Islands","Commonwealth of the Northern Mariana Islands",
#                         "Alaska","Puerto Rico","Guam","American Samoa"))) %>%
#    select(STATEFP,NAME,geometry)
#  
# st_write(states,"data/sf_selected_states.shp") 


states <- st_read("sf_selected_states.shp") 
don <- read_csv("table_clustering_magic_train.csv")

# counties <- tigris::counties(state = c("North Carolina","Florida","Georgia","California","Mississippi"))
# counties <- counties %>% filter(GEOID %in% don$county)
counties <- st_read("selected_counties.shp") %>%
  mutate(NAME_1 = case_when(
    STATEFP =="06"~"California",
    STATEFP =="12"~"Florida",
    STATEFP =="13"~"Georgia",
    STATEFP =="28"~"Mississippi",
    STATEFP == "37"~"North Carolina"
  ))

fires_per_year_state <- read_csv("fires_per_year_state.csv")
df <-  read_csv("df_desc.csv")

plot_var_state <- function(state_name,var){
  
  variable <- ensym(var)
  y_title <- case_when(
    as.name(variable)=="number_of_fires"~"Nombre de feux",
    as.name(variable)=="number_of_ha_burned"~"Surface brûlée totale (ha)"
    #as.name(variable)=="average_size"~"Surface brûlée moyenne (ha)"
  )
  print(paste0("variable choisie: ",y_title))
  print(paste0("Etat choisi: ",state_name))
  
  fires_per_year_state %>% 
    filter(NAME_1 ==state_name)%>%
    ggplot(aes(x = FIRE_YEAR,y=!!variable))+
    geom_col(fill = "red",alpha =.4,colour="orange")+
    theme_light()+
    labs(title = y_title,x="",y="")+
    theme(plot.title = element_text(size=12,hjust = .5))
  
}


# PLOT DES CAUSES 
df_main_cause_cause <- df %>%
  group_by(NAME_1,STAT_CAUSE_DESCR) %>%
  summarise(N = n_distinct(OBJECTID)) %>%
  mutate(STAT_CAUSE_DESCR = fct_reorder(STAT_CAUSE_DESCR,desc(N)))

plot_main_cause <- function(state_name){
  
  
  
  print(paste0("Etat choisi: ",state_name))
  df_main_cause_cause %>%
    filter(NAME_1==state_name)%>%
    mutate()%>%
    ggplot(aes(x=reorder(STAT_CAUSE_DESCR,N),y=N)) +
    geom_col(fill="lightblue",color="yellow") +
    coord_flip()+
    theme_light()+
    labs(x="",y="",title = "Causes des feux")+
    theme(plot.title = element_text(size=12,hjust = .5))
  
}




### Plots par catégories de tailles de feux 
fires_per_class <- df %>%
  group_by(FIRE_YEAR,NAME_1,FIRE_SIZE_CLASS) %>%
  summarise(number_of_fires = n_distinct(OBJECTID),
            number_of_ha_burned = sum(FIRE_SIZE,na.rm = TRUE),
            average_size =number_of_ha_burned/number_of_fires)

plots_classe_feux <- function(state_name,var){
  variable <- ensym(var)
  y_title <- case_when(
    as.name(variable)=="number_of_fires"~"Nombre de feux",
    as.name(variable)=="number_of_ha_burned"~"Surface brûlée totale (ha)",
    as.name(variable)=="average_size"~"Surface brûlée moyenne (ha)",
  )
  print(paste0("variable choisie: ",y_title))
  print(paste0("Etat choisi: ",state_name))
  fires_per_class %>%
    filter(NAME_1==state_name)%>%
    filter(as.numeric(FIRE_YEAR)%%2==1)%>% # On ne garde que les années impaires pour l'esthétique
    ggplot(aes(x=FIRE_YEAR,y = !!variable, fill = FIRE_SIZE_CLASS))+
    geom_col(position = "dodge")+
    theme_light()+
    scale_y_log10()+
    labs(x="",y="Echelle logarithmique", title=y_title,
         fill = "Catégorie de taille")+
    scale_fill_discrete()+
    theme(plot.title = element_text(size=12,hjust = .5))
}



############## FONCTION POUR CES 4 GRAPHIQUES DE PRSENTATION PAR ETAT ####

presentation_etat_feux <- function(state_name){
  p1 <- plot_var_state(state_name,var=number_of_ha_burned)
  p2 <- plots_classe_feux(state_name, var = number_of_fires)
  p3 <- plots_classe_feux(state_name,var = average_size)
  p4 <- plot_main_cause(state_name)
  
  plot <-  ggarrange(p1,p2, 
                     p3,p4,
                     #labels = c("A", "B", "C"),
                     ncol = 2, nrow = 2)
  annotate_figure(
    plot,
    top =text_grob(state_name, color = "darkgrey", face = "bold", size = 15) 
  )
  
}


#### DONNEES POUR CARTE LEAFLET 
etats_selectionnes <- don %>% mutate(state = str_sub(county,1,2))%>%
  group_by(state)%>%
  count()%>%
  arrange(desc(n)) %>%
  head(5)

states <- states %>%
  mutate(etats_selec = ifelse(STATEFP %in%  etats_selectionnes$state,
                              "Etats sélectionnés","Autre"),
         etats_selec = as_factor(etats_selec))

states <- st_transform(states,crs = "+proj=longlat +datum=WGS84") 
# stat sur les feux
fires_state_over_period <- fires_per_year_state %>%
  group_by(NAME_1)%>%
  summarise(number_of_fires = sum(number_of_fires),
            number_of_ha_burned = sum(number_of_ha_burned))

centers <- st_point_on_surface(states)%>%
  filter(STATEFP %in% etats_selectionnes$state) %>%
  select(-etats_selec) %>%
  rename(state=STATEFP) %>%
  inner_join(etats_selectionnes) %>%
  inner_join(fires_state_over_period,by=c("NAME"="NAME_1"))

# creating colors
factpal <- colorFactor(topo.colors(2), states$etats_selec)





##########################################################
