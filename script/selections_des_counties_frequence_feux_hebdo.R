## wildfires us 

library(readr)
library(DBI)
library(RSQLite)
library(tidyverse)
library(dbplyr)
library(lubridate)
library(sf)
library(readxl)

# data_test <- df_selec %>%
#   filter(between(FIRE_YEAR,2011,2015))

counties_match <- read_xlsx("data/counties_merge_avec_raster.xlsx")

df_train <- counties_match %>%
  filter(between(FIRE_YEAR,2000,2010))%>%
  mutate(discov_week= ifelse(discov_week==53,52,discov_week))%>%
  drop_na(STATE)

### PAR COUNTY PAR SAISON NOMBRE DE PAR CATEGORIE ## 
### HECTARES BRULES 
### % DE SURFACE QUI A BRULE 
### INCLURE CAUSE OU NON ? 
## par semaine sur X année
## Par county: combien de feux dans la semaine
df_train$HASC_2 <-as_factor(df_train$HASC_2)

# fires_hebdo <- df_train %>%
#   mutate(discov_week= ifelse(discov_week==53,52,discov_week))%>%
#   group_by(HASC_2,FIRE_YEAR,discov_week,.drop=FALSE)%>%
#   summarise(n =n())


fires_hebdo <- df_train %>%
  mutate(discov_week= ifelse(discov_week==53,52,discov_week))%>%
  group_by(FIRE_YEAR,discov_week,HASC_2,.drop=FALSE)%>%
  summarise(n =n())

# Verification que les états et counties sont cohérents:
state_county <- df_train %>% select(NAME_1,HASC_2)%>% unique() # a l'air ok 
state_county %>% group_by(HASC_2)%>%
  count(NAME_1)%>%
  arrange(desc(n))%>%
  head()# un seul état par county: c'est bon

# On va enlever les counties NA 
fires_hebdo_clean <- fires_hebdo %>% 
  drop_na(HASC_2)

## On regarde le % de semaines par an où les counties ont des feux
fires_hebdo_clean <- fires_hebdo_clean %>%
  mutate(dummy_fire = ifelse(n > 0, TRUE,FALSE))

fires_hebdo_clean <- fires_hebdo_clean %>% inner_join(state_county)

temp <- fires_hebdo_clean %>%
  group_by(NAME_1,HASC_2,FIRE_YEAR)%>%
  summarise(pc_fire_hebdo = sum(dummy_fire)/n_distinct(discov_week))

## Selection des counties avec au moins 10% des semaines avec un feu par an
temp_with_fire <- temp %>%
  filter(pc_fire_hebdo>.10)

## HISTOGRAM FREQUENCE 
temp %>%
  filter(FIRE_YEAR==2010)%>%
  ggplot()+
  geom_histogram(aes(x=pc_fire_hebdo),bins = 18,
                 fill="grey",alpha=.7)+
  theme_bw()+
  geom_vline(xintercept = 0.1,color="red")+
  labs(y="Nombre de counties",
       x="% de semaines avec feux par an")

ggsave(filename="plots/Histogramme_pc_semaine_avec_feux_par_an.png",dpi=600)

#  Nombre de counties restants par an
temp_with_fire %>% group_by(FIRE_YEAR)%>% summarise(n = n_distinct(HASC_2)) %>% arrange(FIRE_YEAR,desc(n))

# Nombre de counties sélectionnés par état 
etats_avec_feux <- temp_with_fire %>% group_by(NAME_1)%>% summarise(n = n_distinct(HASC_2)) %>% arrange(desc(n))


#### SELECTION COUNTIES PRESENTS SUR TOUTES LES ANNEES 
nb_annees_counties <- temp_with_fire %>%
  group_by(HASC_2)%>%
  mutate(nb_annee_present=n_distinct(FIRE_YEAR))

presence_complete <- nb_annees_counties %>%
  filter(nb_annee_present==11)

# Reste 861 Counties au total
#presence_complete %>% group_by(FIRE_YEAR)%>% summarise(n = n_distinct(HASC_2)) %>% arrange(FIRE_YEAR,desc(n))


# counties selectionnees
counties_selec <- df_train %>%
  filter(HASC_2 %in% presence_complete$HASC_2,
         NAME_1 !="Alaska") %>%
  mutate(NAME_2 = str_replace(NAME_2,"Saint","St"),
         NAME_2 = case_when(
           NAME_2=="McKinley"~"Mckinley",
           NAME_2=="McKenzie"~"Mckenzie",
           NAME_2=="Desoto"~"De Soto",
           NAME_2=="Lake of the Woods"~"Lake of The Woods",
           TRUE~NAME_2
         ))

## fiPs CODE 
library(rnoaa)

fips <- fipscodes 

counties_selectionnes <- inner_join(counties_selec,fips, by =c("NAME_1"="state",
                                                    "NAME_2"="county"))


writexl::write_xlsx(counties_selectionnes,"data/counties_selectionnes.csv")
write_rds(counties_selectionnes,"data/counties_selectionnes.RDS")

liste_counties_selectionnes <- counties_selectionnes %>%
  select(NAME_1,NAME_2,fips,fips_state,fips_county) %>%
  unique()

writexl::write_xlsx(liste_counties_selectionnes,"data/liste_counties_selectionnes.csv")

## Une fois enlevé les counties de l'Alaska et un qui ne semble plus exister
# Reste 854 counties selectionnés

counties_selectionnes %>% 
  group_by(FIRE_YEAR)%>% 
  summarise(n = n_distinct(HASC_2)) %>%
  arrange(FIRE_YEAR,desc(n))





