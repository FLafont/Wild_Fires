### CREATE TRAIN AND TEST DATA 
library(sf)
library(readr)
library(DBI)
library(RSQLite)
library(tidyverse)
library(dbplyr)
library(lubridate)


rep <- "data/"
con <- dbConnect(drv=RSQLite::SQLite(), 
                 dbname=paste0(rep,"FPA_FOD_20170508.sqlite"))

tables <- dbListTables(con)
# Table initiale
fires <- dbReadTable(con, tables[2])



### Premier nettoyage
### GESTION DES DATES ET CREATION DUNE VARIABLE SEMAINE 
fires <- fires %>%
  select(-Shape)%>%
  filter(between(FIRE_YEAR,2000,2015))%>%
  # Retrouver la date  
  mutate(discov_date= str_replace(as_date(DISCOVERY_DOY),"1970",as.character(FIRE_YEAR)),
         cont_date = str_replace(as_date(CONT_DOY),"1970",as.character(FIRE_YEAR)),
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
         discov_hour = str_sub(DISCOVERY_TIME,1,2),
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
  ## On supprime les variables utilisées pour la construction 
  select(-DISCOVERY_DATE,-CONT_DATE,-discov_hour,-discov_min,
         -cont_hour,-cont_min,-discov_time,-cont_time,
         -CONT_TIME,-CONT_DOY,-DISCOVERY_DOY,-DISCOVERY_TIME)%>%
  rename(DISCOVERY_DATE = discov_date,
         CONT_DATE = cont_date)


## Observations incohérentes où la date de découverte est 
## ultérieure à la date de contrôle du feu (on peut les enlever? )

#### 2. SELECTION DUNE TABLE PLUS PETITE AVEC 
#### JUSTE LA PRESENCE D'UN FEU, LA TAILLE DU FEU, LA CAUSE, ET LA DATE 

df_selec <- df %>%
  select(FIRE_YEAR,OBJECTID,FIRE_NAME,STAT_CAUSE_DESCR,
         FIRE_SIZE,FIRE_SIZE_CLASS,LATITUDE,LONGITUDE,
         COUNTY,STATE,OWNER_CODE,OWNER_DESCR,STATE,COUNTY,
         FIPS_CODE,DISCOVERY_DATE,CONT_DATE,
         discov_week,time_to_control) 

### SEPARATION TABLE 
data_test <- df_selec %>%
  filter(between(FIRE_YEAR,2011,2015))
write_csv(data_test,"data/data_test.csv")

df_train <- df_selec %>%
  filter(between(FIRE_YEAR,2000,2010))

write_csv(df_train,"data/data_train.csv")
