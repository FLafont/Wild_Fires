library(data.table)
library(tidyverse)
library(readxl)
library(writexl)
##cr?ation table train
#table train : toutes les semaines de 2000 ? 2011 avec date de d?but de la semaine et num?ros 
#(pour donn?es m?t?o et merger avec donn?es de feux)

donnees_train <- data.frame(date_semaine=seq.Date(as.Date("2000-01-01"), as.Date("2010-12-31"), by="week"))

donnees_train$annee<-format(donnees_train$date_semaine, format = "%Y")
donnees_train$semaine<-data.table::week(donnees_train$date_semaine)
donnees_train$county<-NA

liste_counties_selectionnes <- read_xlsx("data/liste_counties_selectionnes.xlsx") 
counties<-unique(liste_counties_selectionnes$fips)
counties<-ifelse(nchar(counties)==4,paste0("0",counties),counties)

# df_test<-tibble(county = counties) %>%
#   group_by(county)%>%
#   mutate(annee = format(donnees_train$date_semaine, format = "%Y"))
# 
# df_test<-donnees_train%>%
#   select(-date_semaine)%>%
#   pivot_wider(names_from=semaine,
#               values_from=semaine)%>%
#   filter(annee=="2010")
# df_test%>%
#   mutate(county=counties)%>%
#   pivot_longer(cols=-county,names_to = "semaine",values_to = "semaine")

#boucles pour faire 1 ligne par semaine et par county que l'on a s?lectionn? auparavent
#574 semaines
#854 counties

nb_semaines<-nrow(donnees_train)
n<-nb_semaines*length(counties)

donnees_train2<-donnees_train
i<-nrow(donnees_train2)
i
start <- Sys.time()
while (i < n) {
  donnees_train2<-rbind.data.frame(donnees_train2,donnees_train)
  i<-nrow(donnees_train2)
  }

for (i in 1:length(counties)){
  donnees_train2[(((i-1)*nb_semaines)+1):((i*nb_semaines)+1),4]<-counties[i]
}

donnees_train2<-donnees_train2[-(n+1),]
Sys.time()-start
#ajout des données de feux :
#nombre de feux et superficie brul?e, et nombre de feux par cause.
counties_selectionnes <- read_xlsx("data/counties_selectionnes.xlsx")%>%
  mutate(fips = ifelse(nchar(fips)>4,fips,paste0("0",fips)),
         FIRE_YEAR = as.character(FIRE_YEAR))
#summary(counties_selectionnes)

info_fires<-counties_selectionnes%>%
  group_by(FIRE_YEAR,discov_week,fips)%>%
  summarise(nb=n(),
            area_burned=sum(FIRE_SIZE))

info_fires2<-counties_selectionnes%>%
  group_by(FIRE_YEAR,discov_week,fips,STAT_CAUSE_DESCR)%>%
  summarise(nb=n())%>%
  pivot_wider(names_from = STAT_CAUSE_DESCR,
              values_from = nb)





don_train<-left_join(donnees_train2,info_fires, by =c("annee"="FIRE_YEAR",
                                                      "semaine"="discov_week",
                                                      "county"="fips"))

don_train<-left_join(don_train,info_fires2,by=c("annee"="FIRE_YEAR",
                                                "semaine"="discov_week",
                                                "county"="fips"))
don_train[is.na(don_train)]<-0
summary(don_train)

don_train<-don_train%>%
  mutate(FIRE=ifelse(nb==0,FALSE,TRUE))

##on sauvegarde

write_xlsx(don_train,"data/don_train.xlsx")

