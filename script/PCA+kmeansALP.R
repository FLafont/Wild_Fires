library(tidyverse)
library(FactoMineR)
library(explor)
library(purrr)
library(ggplot2)
library(xts)
library(ggfortify)
library(ggthemes)
library(maps)
library(mapdata)
library(leaflet)
library(readxl)

donnees_base<-read_csv("data/magic_train.csv")%>% 
  mutate(county = ifelse(nchar(county)<5,paste0("0",county),county))
donnees<-data.frame(donnees_base[,-1])

##création cause majoritaire :

donnees$cause <- colnames(donnees[,(7:18)])[apply(donnees[,(7:18)], 1, which.max)]
donnees[which(donnees$nb==0),45] <- "Aucune"
donnees$cause <- factor(donnees$cause)
summary(donnees$cause)

donnees$land_cover <- colnames(donnees[,(21:37)])[apply(donnees[,(21:37)], 1, which.max)]
donnees$land_cover <- factor(donnees$land_cover)
summary(donnees$land_cover)

str(donnees)

###ACP avec variable FIRE 
don <- donnees[,c(1,2,3,4,5,6,20,40,41,42,43,44,44,45,46)]
don <- na.omit(don)

don1 <- don[,-1]

res.pca <- PCA(don1,quali.sup=c(1,2,3,6,13,14))
explor(res.pca)

###ACP avec uniquement les semaines avec FIRE 
don2 <- don1 %>% 
  filter(FIRE==TRUE) %>% 
  select(-FIRE)

res.pca <- PCA(don2,quali.sup=c(1,2,3,12,13))
explor(res.pca)

###kmeans

don <- na.omit(don <- donnees[,-c(1,2,3,4,38,39,45,46)])
str(don)

k <- 2:15
part <- k
for(i in k){
  kmk=kmeans(don,centers=i,nstart=20)
  part[i-1]=sum(kmk$withinss)/kmk$totss*100
  print(part[i-1])
}

plot(k,part,type="h", xlab="Nombre de classes",ylab="Inertie intra-classes")

kmeans5 <- kmeans(don,centers=5,nstart=20)
table(kmeans5$cluster)

donnees <- na.omit(donnees)

don_clust <- cbind.data.frame(donnees,kmeans5$cluster)
colnames(don_clust)[47] <- "cluster"

don_clust2 <- don_clust %>% 
  group_by(cluster) %>% 
  summarise_all(list(mean))

x <- unique(don_clust[,c("county","cluster")]) %>% 
  mutate(county = ifelse(nchar(county)<5,paste0("0",county),county))
