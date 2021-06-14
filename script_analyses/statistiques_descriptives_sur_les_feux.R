library(readr)
library(tidyverse)
library(dbplyr)
library(lubridate)
library(readxl)

## Statistiques descriptives sur les counties et les Etats sélectionnés

fires <- read_xlsx("data/counties_merge_avec_raster.xlsx")
fires_test <- read_xlsx("data/counties_merge_avec_raster_test.xlsx")
counties_selec <- read_csv("data/liste_counties_selectionnes.csv") %>%
  mutate(fips = ifelse(nchar(fips)<5,paste0("0",fips),fips))


don <- read_csv("data/table_clustering_magic_train.csv")

state_selec <- don %>% 
  mutate(state = str_sub(county,1,2))%>%
  group_by(state)%>%
  count()%>%
  arrange(desc(n)) %>%
  head(5)

# Selection counties 
fires_selec <- inner_join(fires,counties_selec) %>%
  mutate(fips_state = as.character(fips_state),
         fips_state = ifelse(nchar(fips_state)==1,
                                   paste0("0",fips_state),
                                   fips_state))%>%
  filter(NAME_1 %in% c("California","North Carolina","Florida",
                       "Georgia","Mississippi"))

  #filter(STATE %in% c("CA","NC","FL","GA","MS"))

fires_selec_test <- inner_join(fires_test,counties_selec) %>%
  mutate(fips_state = as.character(fips_state),
         fips_state = ifelse(nchar(fips_state)==1,
                             paste0("0",fips_state),
                             fips_state))%>%
  filter(NAME_1 %in% c("California","North Carolina","Florida",
                       "Georgia","Mississippi"))


df <- bind_rows(fires_selec,fires_selec_test)%>%
  mutate(FIRE_YEAR = as.character(FIRE_YEAR),
         FIRE_SIZE = FIRE_SIZE*.4047) %>%
  select(-STATE) # jamais utilisée et confusing 
  

###### Nombre de Feux par an Pour les states ##########
# J'ai enlevé la moyenne car elle est enitièrement définie par les extrêmes
# Idem pour les boxplots
fires_per_year_state <- df %>%
  group_by(FIRE_YEAR,NAME_1) %>%
  summarise(number_of_fires = n_distinct(OBJECTID),
            number_of_ha_burned = sum(FIRE_SIZE,na.rm = TRUE),
            average_size =round(number_of_ha_burned/number_of_fires,1))%>%
  mutate(fires_frequency_group = ntile(number_of_fires,5))


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

#plot_var_state("CA",var=number_of_fires)
plot_surf_brulee_totale_CA <- plot_var_state("California",var=number_of_ha_burned)


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

plot_cause_principale_CA <- plot_main_cause("California")


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

plot_nb_feux_classe_CA <- plots_classe_feux("California",number_of_fires)
plot_mean_size_classe_CA <- plots_classe_feux("California",average_size)


### Essai plusieurs grapphiques d'un coup pour un état 
library(ggpubr)
ggarrange(plot_surf_brulee_totale_CA, 
          plot_nb_feux_classe_CA, 
          plot_mean_size_classe_CA,
          plot_cause_principale_CA,
          #labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)


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

# Des graphiques qui pourraient apparaître sur un onglet du shiny 
presentation_etat_feux("Mississippi")





# Représentation alternative de la taille par feu

# fires_per_class %>%
#   filter(STATE=="CA")%>%
#   ggplot(aes(x=as.numeric(FIRE_YEAR),y = average_size, fill = FIRE_SIZE_CLASS))+
#   geom_col(position = "identity",alpha=3/4)+
#   theme_light()+
#   #coord_flip()+
#   facet_wrap(.~FIRE_SIZE_CLASS,scales = "free")+
#   labs(x="",y="Surface en ha", title="Taille des feux par catégorie" )


