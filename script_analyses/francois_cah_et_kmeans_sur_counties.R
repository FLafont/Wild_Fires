library(tidyverse)
library(missMDA)
library(sf)
## CLUSTERING 
don <- read_csv("data/table_clustering_magic_train.csv") %>%
  select(-annee)

# duplicate county
dup <- don %>% filter(duplicated(county))
dup <- don %>% filter(county==dup$county)

# On garde debris car plus précis
don <- don %>% 
  filter(!(county==dup$county & cause_feu !="Miscellaneous")) %>%
  mutate(id = row_number())
  

don_quant <- don %>% select(-county,-cause_feu) %>%
  mutate_all(~ ifelse(is.infinite(.),NA,.)) 

# handle missing data
nb_dim <- estim_ncpPCA(don_quant)

res.comp <- MIPCA(don_quant, ncp = nb_dim$ncp, nboot = 1000)

don_clean <- inner_join(as.data.frame(res.comp$res.imputePCA),
                               select(don,county,id)) %>%
  select(-id) %>%
  select(county,everything())
# CAH 

# creation matrix distance
distances <- dist(don_clean)

cah.ward <- hclust(distances,method="ward.D2")
plot(cah.ward)

# Regardons l'écart entre les différents groupes
plot(rev(cah.ward$height)[1:30], type="h")


###### 4 ou 8 groupes a l'air ok 
kmeans_1 <- kmeans(don_clean, centers=4, nstart=50)

table(kmeans_1$cluster)

kmeans_1 <- kmeans(don_clean, centers=7, nstart=50)
table(kmeans_1$cluster)

# Not very concluent
don_clean$cluster <- kmeans_1$cluster

don_clean_6 <- don_clean %>% filter(cluster==6)



############ PLOT THEM  #####
counties_shp <- st_read("data/county_shp/tl_2011_us_county/tl_2011_us_county.shp")

counties_selec <- counties_shp %>%
  filter(GEOID %in% don_clean$county)

counties_selec <- inner_join(counties_selec,
                             select(don_clean,county,cluster),
                             by = c("GEOID"="county"))

counties_selec <- st_as_sf(counties_selec)

tigris::
counties_selec %>%
  mutate(cluster= as.character(cluster))%>%
  ggplot()+
  geom_sf(aes(fill=cluster))
