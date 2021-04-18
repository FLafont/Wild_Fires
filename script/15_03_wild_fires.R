## wildfires us 

library(readr)
library(DBI)
library(RSQLite)
library(tidyverse)
library(dbplyr)
library(lubridate)

date

rep <- "data/"
con <- dbConnect(drv=RSQLite::SQLite(), 
                 dbname=paste0(rep,"FPA_FOD_20170508.sqlite"))

tables <- dbListTables(con)
fires <- dbReadTable(con, tables[2])


### 1. GESTION DES DATES ET CREATION DUNE VARIABLE SEMAINE 
fires <- fires %>%
  # Retrouver la date  
  mutate(discov_date = as.Date(DISCOVERY_DOY-1, origin=paste0(FIRE_YEAR,"-01-01")),
         cont_date = case_when(
           CONT_DOY>=DISCOVERY_DOY~as.Date(CONT_DOY-1, origin=paste0(FIRE_YEAR,"-01-01")),
           CONT_DOY<DISCOVERY_DOY~as.Date(CONT_DOY-1, origin=paste0(FIRE_YEAR+1,"-01-01"))
         )) %>%
  # Construire trois variables:
  # Date complète (avec heure et minute) de découverte et controle du feu
  # semaine de découverte et de contrôle du feu 
  # Temps (en minute) pour contrôler le feu 
  ## Problème:
  # Beaucoup de valeurs manquantes
  # 0 pour DISCOVERY_DOY
  # 882638 pour DISCOVERY_TIME
  # 891531 CONT_DOY
  # 972173 DISCOVERY_DOY
  mutate(discov_hour = str_sub(DISCOVERY_TIME,1,2),
         discov_min = str_sub(DISCOVERY_TIME,3,4),
         cont_hour = str_sub(CONT_TIME,1,2),
         cont_min = str_sub(CONT_TIME,3,4),
         discov_time = paste0(discov_hour,":",discov_min),
         cont_time = paste0(cont_hour,":",cont_min),
         discov_date_full = ymd_hm(str_c(discov_date,discov_time)),
         cont_date_full = ymd_hm(str_c(cont_date,cont_time)),
         discov_week = week(discov_date),
         cont_week = week(cont_date),
         time_to_control = cont_date_full-discov_date_full) 

df <- fires %>%
  ## On supprime les variables utilisées pour la construction 
  select(-DISCOVERY_DATE,-CONT_DATE,-discov_hour,-discov_min,
         -cont_hour,-cont_min,-discov_time,-cont_time,-discov_date,
         -cont_date,-CONT_TIME,-CONT_DOY,-DISCOVERY_DOY,-DISCOVERY_TIME)%>%
  rename(DISCOVERY_DATE = discov_date_full,
         CONT_DATE = cont_date_full)

## 3 observations incohérentes où la date de découverte est 
## ultérieure à la date de contrôle du feu (on peut les enlever? )
df_time <- fires %>%
  select(FIRE_YEAR,DISCOVERY_DOY,DISCOVERY_TIME,CONT_DOY,CONT_TIME,
         discov_date:time_to_control)
# on regarde les variables manquantes 
# sum(is.na(fires$DISCOVERY_DOY))
# sum(is.na(fires$DISCOVERY_TIME))
# 
# ## Beaucoup plus de donneés manquantes 
# ## dans les variables sur le contrôle 
# 
# sum(is.na(fires$CONT_TIME))
# sum(is.na(fires$CONT_DOY))

#### 2. SELECTION DUNE TABLE PLUS PETITE AVEC 
#### JUSTE LA PRESENCE D'UN FEU, LA TAILLE DU FEU, LA CAUSE, ET LA DATE 

df_selec <- df %>%
  select(FIRE_YEAR,OBJECTID,FIRE_NAME,STAT_CAUSE_DESCR,
         FIRE_SIZE,FIRE_SIZE_CLASS,LATITUDE,LONGITUDE,
         COUNTY,STATE,OWNER_CODE,OWNER_DESCR,STATE,COUNTY,
         FIPS_CODE,DISCOVERY_DATE,CONT_DATE,discov_week,time_to_control)

# combien de feux
nb_fires <- df_selec %>%
  group_by(FIRE_YEAR)%>%
  summarise(nb_fires=n_distinct(OBJECTID))
## COMBIEN DE SEMAINES ?
# Au moins un feu par semaine chaque année 
semaines <- df_selec %>%
  group_by(FIRE_YEAR)%>%
  summarise(nb_semaines = n_distinct(discov_week))
## COMBIEN DE COUNTIES ? 
# De plus en plus:
# Entre 20% et 50% par an 
# Pourtant le nombre de feux n'augmente pas de manière linéaire:
# strange 
nb_counties <- df_selec %>%
  group_by(FIRE_YEAR)%>%
  summarise(nb_counties = n_distinct(COUNTY))

## Presque tous les états représentés chaque année 
nb_states <- df_selec %>%
  group_by(FIRE_YEAR)%>%
  summarise(nb_state = n_distinct(STATE))


##############

idx_fires_shape <- dbReadTable(con,tables[12])
idx_fires_shape_node <- dbReadTable(con,tables[13])
idx_fires_shape_parent <- dbReadTable(con,tables[14])
idx_fires_shape_rowid <- dbReadTable(con,tables[15])


temp <- head(select(fires,20:28))
temp_full <- select(fires,20:28)
max(temp_full$DISCOVERY_DOY)



### MAP WITH FIRES PER YEAR PER COUNTRY 
df_fire <- fires %>% select(OBJECTID,FIRE_NAME,FIRE_YEAR,COUNTY,
                            FIRE_SIZE_CLASS,STAT_CAUSE_DESCR,FIRE_SIZE,
                            STATE,LATITUDE,LONGITUDE,OWNER_DESCR,OWNER_CODE) 


fires_per_year_state <- df_fire %>%
  group_by(FIRE_YEAR,STATE) %>%
  summarise(number_of_fires = n_distinct(OBJECTID),
            number_of_acres_burned = sum(FIRE_SIZE,na.rm = TRUE))%>%
  mutate(fires_frequency_group = ntile(number_of_fires,5))

# Variables for the app
state_names <- unique(df_fire$STATE)
years <- sort(unique(df_fire$FIRE_YEAR))

# indicateur à choisir 
indicateurs <- c("number_of_fires","number_of_acres_burned")
names(indicateurs) <- c("Nombre de feux","Superficie du feu (acres)")

######### CAUSE DES FEUX ###############""
causes <- df_fire %>% 
  group_by(STATE,FIRE_YEAR,STAT_CAUSE_DESCR)%>%
  summarise(n=n_distinct(OBJECTID))

fires_per_year_state %>% arrange(desc(number_of_fires))
causes %>% arrange(desc(n))
#################################################""
####### EDA DES AUTRES TABLES ############""
NWCG_UnitIDActive <- dbReadTable(con, tables[4])
# #spatialindex <- dbReadTable(con, tables[5]) # nope
# geom_cols_ref_sys <- dbReadTable(con, tables[6]) # META ON SHAPEFILES 
# geom_cols_field_info <- dbReadTable(con, tables[8]) 
# geom_cols_stats <- dbReadTable(con, tables[9])
tables[12]
