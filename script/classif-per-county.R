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

donnees<-na.omit(donnees_base[,-c(1,2,4)])
#summary(donnees)

###moyennes par an par county
don <- donnees %>% 
  group_by(county,annee) %>% 
  summarise(across("nb":"Structure", ~ sum(.x, na.rm = TRUE)),
            across("11_Open_water":"av_humidity",~ mean(.x, na.rm = TRUE))) %>% 
  group_by(county)%>%
  select(-annee) %>% 
  summarise_all(list(mean))

summary(don)
don$county=factor(don$county)


res.pca <- PCA(don,quali.sup=1,quanti.sup=c(4:16))
explor(res.pca)

res.cah <- HCPC(res.pca)
x <- res.cah$data.clust

res.cah$desc.var

don_clust <- res.cah$data.clust

don_clust %>% 
  group_by(clust) %>% 
  select(-county) %>% 
  summarise_all(list(mean))

don1 <- don[,-(34:35)]
res.pca2 <- PCA(don1,quali.sup=1,quanti.sup=c(4:16))
explor(res.pca2)
