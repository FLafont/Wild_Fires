library(tidyverse)
library(tidymodels)
library(broom)
# clustering

data <- read_csv("data/table_classif_long.csv")

df <- data %>%
  pivot_wider(names_from = saison,
              values_from = c(A:total_nb_fire)) %>%
  mutate_if(is.numeric,~ifelse(is.na(.),0,.))


## TRY 1 
kclusts <- kmeans(df[,4:36], centers = 4)

data_clust <- inner_join(data,
                         augment(kclust, df) %>%select(1:3,37))

data_clust %>%
  count(.cluster)

## TRY 2 
df2 <- data %>%
  select(-A:-G)%>%
  pivot_wider(names_from = saison,
              values_from = c(mean_fire_size:total_nb_fire)) %>%
  mutate_if(is.numeric,~ifelse(is.na(.),0,.)) 

kclusts2 <- kmeans(df2[,4:15], centers = 4)

data_clust2 <- inner_join(data,augment(kclusts2, df2) %>%select(1:3,16))

data_clust2 %>%
  count(.cluster)


