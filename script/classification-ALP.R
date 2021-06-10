library(tidyverse)
library(FactoMineR)
library(explor)
library(rgrs)
library(purrr)
library(ggplot2)
library(xts)
library(ggfortify)
library(ggthemes)
library(maps)
library(mapdata)
library(leaflet)
library(readxl)

donnees<-read_csv("data/magic_train.csv")
donnees<-donnees[,-1]


### classif des counties fonction de la météo + du nombre de feux + land cover
###faire un rapport ha brulés/superficie county

don<-donnees[,-(1:3)]
don <- na.omit(don)
don_centre <- scale(don)
dim(don_centre)

k <- 2:15
part <- k
for(i in k){
  kmk=kmeans(don,centers=i,nstart=20)
  part[i-1]=sum(kmk$withinss)/kmk$totss*100
  print(part[i-1])
}

plot(k,part,type="h", xlab="Nombre de classes",ylab="Inertie intra-classes")

###8 classes semblent le plus adapté

kmeans8 <- kmeans(don,centers=8,nstart=20)

table(kmeans8$cluster)

don_clust <- cbind.data.frame(don,kmeans8$cluster)
colnames(don_clust)[42] <- "cluster"


str(don_clust)

don_clust2 <- don_clust %>% 
  group_by(cluster) %>% 
  summarise (nb_mean=mean(nb),
             area_mean=mean(area_burned),
             temp_mean=mean(av_temp_mean),
             wind_mean=mean(av_wind_speed),
             cloud_mean=mean(av_cloud_cover),
             precip_mean=mean(av_precip),
             humidity_mean=mean(av_humidity))

don_clust2 <- don_clust %>% 
  group_by(cluster) %>% 
  summarise_all(list(mean))

don_clust <- as_tibble(don_clust)
x <- unique(don_clust[,c("county","cluster")]) %>% 
  mutate(county = ifelse(nchar(county)<5,paste0("0",county),county))


#affichage des classes sur une carte
liste_counties <- read_xlsx("data/liste_counties_selectionnes.xlsx")%>% 
  mutate(county = ifelse(nchar(fips)<5,paste0("0",fips),fips))
liste_counties$region<-tolower(liste_counties$NAME_1)
liste_counties$subregion<-tolower(liste_counties$NAME_2)

liste_counties_clust <- liste_counties %>% 
  right_join(x,by=c('fips'='county'))%>%
  mutate(cluster=factor(cluster))



counties_tot <- counties %>% 
  group_by(region,subregion) %>% 
  right_join(liste_counties_clust,by=c('region','subregion')) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = cluster)) + 
  geom_polygon() + 
  geom_path(color = 'white', size = 0.1) + 
  theme_map() + 
  coord_map('albers', lat0=30, lat1=40) + 
  ggtitle("US Counties selected") + 
  theme(plot.title = element_text(hjust = 0.5))

counties_tot

