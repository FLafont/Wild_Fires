library(tidyverse)
##
don <- read_csv("data/table_clustering_magic_train.csv")

don %>% mutate(state = str_sub(county,1,2))%>%
  group_by(state)%>%
  count()%>%
  arrange(desc(n)) %>%
  head(5)

