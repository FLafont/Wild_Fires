
library(tidyverse)
library(sf)


df_test  <- read_csv("data/data_test.csv")
df_test$COUNTY <-as_factor(df_test$COUNTY)

##Création de la table counties_match_test avec les donnees test (l'autre n'était faite qu'avec les données de 2000 à 2010)
##### TROUVER LES COUNTIES PAR CORRESPONDANCE GEO ###
# On transforme les données en objet SF, avec le bon CRS WGS84 (4326)

sf_test <- st_as_sf(df_test, coords = c("LONGITUDE","LATITUDE"))
st_crs(sf_test) <- 4326 


usa_counties <- raster::getData("GADM",country="US",level=2) %>%
  st_as_sf() %>%
  select(NAME_1,GID_2,NAME_2,HASC_2,geometry) 

usa_counties <- usa_counties %>%
  mutate(county_size = as.numeric(st_area(geometry)/1e4)) # divise par 10,000 car calcul? en m^2

start <- Sys.time()
counties_match_test <- st_join(sf_test,usa_counties)
end <- Sys.time()
end-start

df_counties_match_test <- tibble(counties_match_test)
writexl::write_xlsx(df_counties_match_test,"data/counties_merge_avec_raster_test.xlsx")

##même travail que pour df_train mais en utilisant test

counties_match_test <- read_xlsx("data/counties_merge_avec_raster_test.xlsx")

#on retire les NA sur les states et on regroupe les semaines 52 et 53
df_test <- counties_match_test %>%
  mutate(discov_week= ifelse(discov_week==53,52,discov_week))%>%
  drop_na(STATE)

#à partir des counties selectionnés grâce aux données test, on créé la table counties_selec_test
#(attention, il nous faut la table presence_complete du script "selections_des_counties_frequence_feu.R" pour avoir les même counties dans test et train)
#hasc_selec <- counties_selectionnes$HASC_2


# counties_selec_test <- df_test %>%
#   filter(HASC_2 %in% presence_complete$HASC_2,
#          NAME_1 !="Alaska") %>%
#   mutate(NAME_2 = str_replace(NAME_2,"Saint","St"),
#          NAME_2 = case_when(
#            NAME_2=="McKinley"~"Mckinley",
#            NAME_2=="McKenzie"~"Mckenzie",
#            NAME_2=="Desoto"~"De Soto",
#            NAME_2=="Lake of the Woods"~"Lake of The Woods",
#            TRUE~NAME_2
#          ))
counties_selec_test <- df_test %>%
  filter(HASC_2 %in% hasc_selec)

## fiPs CODE 
library(rnoaa)

fips <- fipscodes 
counties_selectionnes_test <- inner_join(counties_selec_test,
                                         fips, by =c("NAME_1"="state",
                                                     "NAME_2"="county"))

#on prend le même vecteur "counties" que pour la matrice semaine_test

counties<-unique(liste_counties_selectionnes$fips)

donnees_test <- data.frame(date_semaine=seq.Date(as.Date("2011-01-01"), as.Date("2015-12-31"), by="week"))
donnees_test$annee<-format(donnees_test$date_semaine, format = "%Y")
donnees_test$semaine<-data.table::week(donnees_test$date_semaine)
donnees_test$county<-NA

#boucles pour faire 1 ligne par semaine et par county
#261 semaines
#854 counties

nb_semaines<-261
n<-nb_semaines*length(counties)
donnees_test2<-donnees_test

i<-nrow(donnees_test2)

i

while (i < n) {
  donnees_test2<-rbind.data.frame(donnees_test2,donnees_test)
  i<-nrow(donnees_test2)
}

for (i in 1:length(counties)){
  donnees_test2[(((i-1)*nb_semaines)+1):((i*nb_semaines)+1),4]<-counties[i]
}

donnees_test2<-donnees_test2[-(n+1),]

#ajout des données de feux à test:
#nombre de feux et superficie brulée, et nombre de feux par cause.

#summary(counties_selectionnes_test)

counties_selectionnes_test$FIRE_YEAR <- as.character(counties_selectionnes_test$FIRE_YEAR )
info_fires<-counties_selectionnes_test%>%
  group_by(FIRE_YEAR,discov_week,fips)%>%
  summarise(nb=n(),
            area_burned=sum(FIRE_SIZE))

info_fires2<-counties_selectionnes_test%>%
  group_by(FIRE_YEAR,discov_week,fips,STAT_CAUSE_DESCR)%>%
  summarise(nb=n())%>%
  pivot_wider(names_from = STAT_CAUSE_DESCR,
              values_from = nb)

#donnees_test2$annee=as.numeric(donnees_test2$annee)

don_test<-left_join(donnees_test2,info_fires, by =c("annee"="FIRE_YEAR",
                                                      "semaine"="discov_week",
                                                      "county"="fips"))

don_test<-left_join(don_test,info_fires2,by=c("annee"="FIRE_YEAR",
                                                "semaine"="discov_week",
                                                "county"="fips"))
don_test[is.na(don_test)]<-0
summary(don_test)

don_test<-don_test%>%
  mutate(FIRE=ifelse(nb==0,FALSE,TRUE),
         county= ifelse(nchar(county)>4,county,paste0("0",county))) %>%
  group_by(county)

# Add HASC2 and area by county
# Reste que 846? Pas sûr de pourquoi. 
hasc_fips <- counties_selectionnes_test %>%
  select(HASC_2,county=fips)%>%
  unique()
don_testf <- inner_join(hasc_fips,don_test) 

don_testf <- inner_join(don_testf,
                        tibble(usa_counties)%>%
                          select(NAME_1,county_size,HASC_2))
###sauver la table don_test
write_xlsx(don_testf,"data/don_test.xlsx")
#write.csv(don_test,"data/don_test.csv")






