## wildfires us 

library(readr)
library(DBI)
library(RSQLite)
library(tidyverse)

rep <- "data/"
con <- dbConnect(drv=RSQLite::SQLite(), 
                 dbname=paste0(rep,"FPA_FOD_20170508.sqlite"))


tables <- dbListTables(con)

fires <- dbReadTable(con, tables[2])

shape <- idx_fires_shape %>% filter(pkid=="211297")

idx_fires_shape <- dbReadTable(con,tables[12])
idx_fires_shape_node <- dbReadTable(con,tables[13])
idx_fires_shape_parent <- dbReadTable(con,tables[14])
idx_fires_shape_rowid <- dbReadTable(con,tables[15])

temp <- head(fires)

var_names <- names(fires)


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
