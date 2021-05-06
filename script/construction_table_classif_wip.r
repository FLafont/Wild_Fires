library(dplyr)
library(sf)


df_train  <- read_csv("data/data_train.csv")
df_train$COUNTY <-as_factor(df_train$COUNTY)

##### TROUVER LES COUNTRIES PAR CORRESPONDANCE GEO ###
# On transforme les données en objet SF, avec le bon CRS WGS84 (4326)
sf_train <- st_as_sf(df_train, coords = c("LONGITUDE","LATITUDE"))
st_crs(sf_train) <- 4326 


usa_counties <- raster::getData("GADM",country="US",level=2) %>%
  st_as_sf() %>%
  select(NAME_1,GID_2,NAME_2,HASC_2,geometry) 

usa_counties <- usa_counties %>%
  mutate(county_size = as.numeric(st_area(geometry)/1e4)) # divise par 10,000 car calculé en m^2

start <- Sys.time()
counties_match <- st_join(sf_train,usa_counties)
end <- Sys.time()
end-start # juste pour savoir le temps que ça prend 

## ICI ON PEUT SAUVEGARDER LA TABLE AVEC LES COUNTIES SI CEST LONG 
# A MATCHER 

# df_counties_match <- tibble(counties_match)
# writexl::write_xlsx(df_counties_match,"data/counties_merge_avec_raster.xlsx")


### PAR COUNTY PAR SAISON NOMBRE DE FEUX PAR CATEGORIE ## 
### HECTARES BRULES 
### % DE SURFACE QUI A BRULE 
### INCLURE CAUSE OU NON ? 

acres_to_hectare <- .404685642 

counties_match <- tibble(counties_match) %>%
  select(-geometry)%>%
  mutate(saison = case_when(
    str_sub(DISCOVERY_DATE,6,7) %in% c("01","02","03")~"hiver",
    str_sub(DISCOVERY_DATE,6,7) %in% c("04","05","06")~"printemps",
    str_sub(DISCOVERY_DATE,6,7) %in% c("07","08","09")~"ete",
    str_sub(DISCOVERY_DATE,6,7) %in% c("10","11","12")~"hiver"),
    FIRE_SIZE = FIRE_SIZE * acres_to_hectare)

table_classif <- counties_match %>%
  group_by(NAME_1,saison,GID_2,NAME_2,FIRE_SIZE_CLASS)%>%
  summarise(nb_fires = n())%>%
  pivot_wider(names_from = FIRE_SIZE_CLASS, values_from = nb_fires) %>%
  
  inner_join(
    counties_match %>%
  group_by(NAME_1,saison,GID_2,NAME_2)%>%
  summarise(mean_fire_size = mean(FIRE_SIZE,na.rm = TRUE),
            total_fire_size = sum(FIRE_SIZE,na.rm = TRUE),
            fire_size_county_area_ratio = total_fire_size/first(county_size),
            total_nb_fire = n()),
  by=c("NAME_1","saison","GID_2","NAME_2"))

table_classif_wide <- table_classif %>%
  pivot_wider(names_from = saison,
              values_from = c(A:total_nb_fire))


# 
# 
# ## Par county: combien de feux dans la semaine
# fires_hebdo <- df_selec %>%
#   mutate(discov_week= ifelse(discov_week==53,52,discov_week))%>%
#   group_by(FIRE_YEAR,discov_week,COUNTY,.drop=FALSE)%>%
#   summarise(n =n())
# 
# # On va enlever les counties NA 
# fires_hebdo_clean <- fires_hebdo %>% 
#   drop_na(COUNTY)
# 
# 
# #########################
# 
# #### Nombre moyen de feux hebdomadaire
# ### sur la période 2000-2010 
# df_selec <- df_selec %>%
#   mutate(discov_week= ifelse(discov_week==53,52,discov_week))%>%
#   filter(STATE!="PR")
# 
# mean_fire_week <- df_selec %>%
#   filter(between(FIRE_YEAR,2000,2010))%>%
#   mutate(discov_week= ifelse(discov_week==53,52,discov_week))%>%
#   group_by(discov_week,STATE)%>%
#   summarise(weekly_mean = n_distinct(OBJECTID)/11)
# 
# mean_fire_week <- mean_fire_week %>%
#   group_by(STATE)%>%
#   summarise(weekly_mean = mean(weekly_mean))
# 
# 
# 
# ## link with raster 
# usa_state <- raster::getData("GADM",country="US",level=1)
# sf_usa_state <- st_as_sf(usa_state) 
# sf_usa_state <- sf_usa_state %>%
#   mutate(STATE_ID = str_sub(HASC_1,4,5))
# 
# meta_sf <- sf_usa_state %>% 
#   tibble()%>%
#   dplyr::select(-geometry)
# 
# sf_fire_weekly <- inner_join(temp,sf_usa_state,by=c("STATE"="STATE_ID")) 
# 
# st_geometry(sf_fire_weekly) <- sf_fire_weekly$geometry
# 
# sf_fire_weekly %>%
#   filter(!(STATE %in% c("AK","HI")))%>%
#   ggplot(aes(fill=desc(weekly_mean)))+
#   geom_sf()+
#   labs(fill="% de semaines avec feux")
# 
# 
# ### Nombre d'hectares brûlés par an 
# 
# 
# burn_ha_yearly <- df_selec %>%
#   filter(between(FIRE_YEAR,2000,2010))%>%
#   group_by(STATE,FIRE_YEAR)%>%
#   summarise(ha_burn = sum(FIRE_SIZE,na.rm = TRUE)/1e3)%>%
#   pivot_wider(names_from = FIRE_YEAR,
#               values_from=ha_burn)

