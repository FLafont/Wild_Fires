library(tidyverse)
## Feature engineering # 1 

don <- read_csv("data/magic_train.csv")

# main_fire_cause <- don %>% 
#   select(county,date_semaine,nb,Debris.Burning:Structure) %>%
#   pivot_longer(c(Debris.Burning:Structure),
#                names_to = "cause_feu",
#                values_to = "nb_feux") %>%
#   mutate(pc_feu = nb_feux/nb) %>%
#   group_by(county,date_semaine)%>%
#   filter(pc_feu == max(pc_feu))

main_fire_cause <- don %>% 
  select(county,date_semaine,nb,Debris.Burning:Structure) %>%
  pivot_longer(c(Debris.Burning:Structure),
               names_to = "cause_feu",
               values_to = "nb_feux_main_cause") %>%
  group_by(county,cause_feu)%>%
  summarise(nb_feux_cause= sum(nb_feux_main_cause)) %>%
  group_by(county)%>%
  mutate(tot_feu = sum(nb_feux_cause),
         pc_cause = nb_feux_cause/tot_feu)%>%
  filter(pc_cause==max(pc_cause))

## test de garder ces data là

don_long <- don %>%
  pivot_longer(c(Debris.Burning:Structure),
               names_to = "cause_feu",
               values_to = "nb_feux_main_cause")

don_long <- don_long %>%
  group_by(county)%>%
  group_split()


selec_main_cause_feu <- function(df1){
  
  temp <- main_fire_cause %>%
    filter(county==first(df1$county))
  
  res <- df1 %>%
    group_by(date_semaine)%>%
    filter(cause_feu==temp$cause_feu)
  
  return(res)
}
don_long <- map_df(don_long,selec_main_cause_feu)

rm(don)

#### Gestion des NA 
# On remplace les feux voisins manquant par une moyenne des feux voisins 
# on se débarasse des trois premières semaines pour lesquelles on a pas la météo
don_long_2<- don_long %>% 
  mutate(nb_feux_voisins = ifelse(is.na(nb_feux_voisins),
                                        mean(nb_feux_voisins,na.rm = T),
                                        nb_feux_voisins),
         nb_feux_voisins_lag = ifelse(is.na(nb_feux_voisins_lag),
                                            mean(nb_feux_voisins_lag, na.rm = T),
                                            nb_feux_voisins_lag)) %>%
  filter(!is.na(av_precip))


write_csv(don_long_2,"data/magic_train_feature_eng.csv")



