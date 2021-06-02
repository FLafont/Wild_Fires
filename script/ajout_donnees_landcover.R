library(readxl)
library(tidyverse)
#######ajout des données Land Cover 2016 à la table test#######
###############################################################

lc2016 <- read_xlsx("data/landcover_repartition_by_count_nlcd_2016.xlsx")

###si on veut, on peut ajouter la légende en texte (plus lisible pour la suite ?)

lc<-sort(unique(as.numeric(lc2016$landcover)))
text <- c("0","Open_water","Perennial_ice_snow","Developed_open_space","Developed_low_intensity","Developed_medium_intensity","Developed_high_intensity","Barren_land_rock_sand_clay","Deciduous_forest","Evergreen_forest","Mixed_forest","Shrub_scrub","Grassland_herbaceous","Pasture_hay","Cultivated_crops","Woody_wetlands","Emergent_herbaceous_wetlands")
legend <- data.frame(code=lc,text=text) 

lc2016v2 <- inner_join(lc2016,legend, by =c("landcover"="code"))
#summary(lc2016v2)


lc2016_pivot <- lc2016v2 %>%
  unite(landcover,text,col="land",sep="_") %>% 
  select(land,area,fips) %>% 
  pivot_wider(names_from = land,
              values_from = area)

lc2016_pivot[is.na(lc2016_pivot)]<-0

don_test$county<-ifelse(nchar(don_test$county)==4,paste0("0",don_test$county),don_test$county)

semaine_test<-inner_join(don_test,lc2016_pivot,by=c("county"="fips"))

##sauvegarde

write.csv(semaine_test,"data/semaine_test.csv")

#######ajout des données Land Cover 2011 à la table train#######
###############################################################

lc2011 <- read_xlsx("data/landcover_repartition_by_count_nlcd_2011.xlsx")

###si on veut, on peut ajouter la légende en texte (plus lisible pour la suite ?)

lc<-sort(unique(as.numeric(lc2016$landcover)))
text <- c("0","Open_water","Perennial_ice_snow","Developed_open_space","Developed_low_intensity","Developed_medium_intensity","Developed_high_intensity","Barren_land_rock_sand_clay","Deciduous_forest","Evergreen_forest","Mixed_forest","Shrub_scrub","Grassland_herbaceous","Pasture_hay","Cultivated_crops","Woody_wetlands","Emergent_herbaceous_wetlands")
legend <- data.frame(code=lc,text=text) 

lc2011v2 <- inner_join(lc2011,legend, by =c("landcover"="code"))

#summary(lc2011v2)


lc2011_pivot <- lc2011v2 %>%
  unite(landcover,text,col="land",sep="_") %>% 
  select(land,area,fips) %>% 
  pivot_wider(names_from = land,
              values_from = area)

lc2011_pivot[is.na(lc2011_pivot)]<-0

don_train$county<-ifelse(nchar(don_train$county)==4,paste0("0",don_train$county),don_train$county)
don_train$county=factor(don_train$county)
lc2011_pivot$fips=factor(lc2011_pivot$fips)
semaine_train<-inner_join(don_train,lc2011_pivot,by=c("county"="fips"))

##sauvegarde

write.csv(semaine_train,"data/semaine_train.csv")
