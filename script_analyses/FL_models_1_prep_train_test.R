library(tidymodels)
library(tidyverse)
library(workflows)
library(tune)
library(ranger)
library(sf)
## add county size
counties_sf <- tigris::counties() 
counties_sf <- counties_sf %>% filter(GEOID %in% don$county) %>%
  mutate(size = round(st_area(geometry)/1e4))
counties_sf <- tibble(counties_sf) %>% select(county=GEOID,county_size = size)

# Test sur la Californie 
don <- read_csv("data/magic_train_feature_eng.csv") %>%
  inner_join(counties_sf) %>%
  mutate(FIRE = ifelse(FIRE,"FEU","NON_FEU"),
         FIRE = as_factor(FIRE)) %>%
  mutate(nb_feux_lag = lag(nb),
         percentage_area_burned = area_burned/county_size,
         percentage_area_burned_prev_week = lag(percentage_area_burned))


# Start with california 
ca <- don %>%
  filter(state=="06")

# Selection des variables d'intérêt
df <- ca %>%
  select(-c(state,date_semaine,nb,area_burned,semaine,annee,
            nb_feux_main_cause,nb_feux_voisins,X0_0))%>%
  mutate(XX_Developed = X21_Developed_open_space+X22_Developed_low_intensity+
           X23_Developed_medium_intensity+X24_Developed_high_intensity)%>%
  select(-c(X21_Developed_open_space:X24_Developed_high_intensity,county,
            nb_feux_lag,percentage_area_burned,percentage_area_burned_prev_week))

df <- df %>% 
  select(-cause_feu) %>%
  na.omit()
  
# Création jeu de training et jeu de test 
# 400 lignes qui partent sur un na.omit
df_split <- initial_split(df,prob = .8)

train <- training(df_split) 
test <- testing(df_split)

#### JEUX avec VARIABLE EXTRA
df_extra_var <- ca %>%
  select(-c(state,date_semaine,nb,area_burned,semaine,annee,
            nb_feux_main_cause,nb_feux_voisins,X0_0))%>%
  mutate(XX_Developed = X21_Developed_open_space+X22_Developed_low_intensity+
           X23_Developed_medium_intensity+X24_Developed_high_intensity)%>%
  select(-c(X21_Developed_open_space:X24_Developed_high_intensity,county))

df_extra_var <- df_extra_var %>% 
  select(-cause_feu) %>%
  na.omit()

 