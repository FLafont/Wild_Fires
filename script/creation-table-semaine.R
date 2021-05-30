library(data.table)
library(tidyverse)

##création table train
#table train : toutes les semaines de 2000 à 2011 avec date de début de la semaine et numéros 
#(pour données météo et merger avec données de feux)

donnees_train <- data.frame(date_semaine=seq.Date(as.Date("2000-01-01"), as.Date("2010-12-31"), by="week"))

donnees_train$annee<-format(donnees_train$date_semaine, format = "%Y")
donnees_train$semaine<-week(donnees_train$date_semaine)
donnees_train$county<-NA

counties<-unique(liste_counties_selectionnes$fips)

# df_test<-donnees_train%>%
#   pivot_wider(names_from=date_semaine,
#               values_from=date_semaine)%>%
#   mutate(county=counties)%>%
#   pivot_longer(cols=-county,names_to = "semaine",values_to = "semaine")

#boucles pour faire 1 ligne par semaine et par county que l'on a sélectionné auparavent
#574 semaines
#854 counties

donnees_train2<-donnees_train
colnames(donnees_train2)<-colnames(donnees_train)
i<-nrow(donnees_train2)
i

while (i <490196) {
  donnees_train2<-rbind.data.frame(donnees_train2,donnees_train)
  i<-nrow(donnees_train2)
  }

for (i in 1:length(counties)){
  donnees_train2[(((i-1)*574)+1):((i*574)+1),4]<-counties[i]
}

donnees_train2<-donnees_train2[-490197,]

#ajout des données de feux :
#nombre de feux et superficie brulée, et nombre de feux par cause.

summary(counties_selectionnes)

info_fires<-counties_selectionnes%>%
  group_by(FIRE_YEAR,discov_week,fips)%>%
  summarise(nb=n(),
            area_burned=sum(FIRE_SIZE))

info_fires2<-counties_selectionnes%>%
  group_by(FIRE_YEAR,discov_week,fips,STAT_CAUSE_DESCR)%>%
  summarise(nb=n())%>%
  pivot_wider(names_from = STAT_CAUSE_DESCR,
              values_from = nb)

donnees_train2$annee=as.numeric(donnees_train2$annee)


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


##même traitement pour test
donnees_test <- data.frame(date_semaine=seq.Date(as.Date("2000-01-01"), as.Date("2010-12-31"), by="week"))

donnees_test$annee<-format(donnees_test$date_semaine, format = "%Y")
donnees_test$semaine<-week(donnees_test$date_semaine)




