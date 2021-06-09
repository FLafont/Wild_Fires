library(tidyverse)
# CLASSIFICATION NON SUPERVISEE 
# DEMARRAGE AVEC CAH 

# Etape 1 
# Retrouver une table avec un county par ligne 
# Choix metholodogiques qui vont suivre très discutable 

# Transformations pour summarize: 
# nb => Somme de tous les feux 
# area_burned => somme de toutes les area burned
# Fire : enlevé 
# X11:X12_perennial ice snow : => moyenne 
# pop_year ==> moyenne
# + ajout croissance 2010-2000
# nb_feux_voisins = somme
# nb_feux_voisins_lag = enlevé 
# av_temp_mean => moyenne 
# + croissance 2010-2000
# avg_temp_low = min 
# avg_temp_max = max + croissance 2010-2000
# avg_cloud_cover = mean 
# avg_precip = mean 
# avg_humidity = mean
# avg_wind_speed = mean 
# cause_feu = Unique
# nb_feux_main_cause = mean 

don <- read_csv("data/magic_train_feature_eng.csv")

df <- don %>%
  group_by(county)%>%
  summarise(nb_feux = round(sum(nb,na.rm = TRUE)),
            area_burned = round(sum(area_burned, na.rm = TRUE)),
            pop_year = round(mean(pop_year)),
            nb_feux_voisins = round(sum(nb_feux_voisins,na.rm = TRUE)),
            cause_feu = unique(cause_feu),
            nb_feux_main_cause = sum(nb_feux_main_cause,na.rm = TRUE))

# On perd two counties de plus pour arriver à 845 mais ils n'ont presque aucune donnée météo 
df_temp <- don %>% 
  select(annee,county,av_temp_hi:av_humidity) %>%
  group_by(county,annee)%>%
  summarise(av_temp_mean = mean(av_temp_mean,na.rm = T),
            av_temp_hi = max(av_temp_hi,na.rm = T),
            av_temp_lo = min(av_temp_lo,na.rm=T),
            av_precip = mean(av_precip,na.rm = T),
            av_wind_speed = mean(av_wind_speed,na.rm = T),
            av_cloud_cover = mean(av_cloud_cover,na.rm = T),
            av_humidity = mean(av_humidity,na.rm = T)) %>%
  group_by(county)%>%
  filter(annee %in% c(min(annee),max(annee))) %>%
  #group_by(county)%>%
  mutate(av_temp_mean_growth = (av_temp_mean-lag(av_temp_mean))/lag(av_temp_mean),
         av_temp_hi_growth = (av_temp_hi-lag(av_temp_hi))/lag(av_temp_hi)) %>%
  filter(annee==max(annee))
  

df_land_cover <- don %>%
  select(county,X11_Open_water:X12_Perennial_ice_snow) %>%
  group_by(county)%>%
  summarise_at(vars(X11_Open_water:X12_Perennial_ice_snow),
               ~mean(.x))

df <- inner_join(df,df_land_cover)%>%
  inner_join(df_temp)


write_csv(df,"data/table_clustering_magic_train.csv")
