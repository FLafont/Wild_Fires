library(readxl)
library(tidyverse)
library(lubridate)

## Création de quelques variables supplémentaires
data_test <- read_xlsx("data/don_test.xlsx")


acres_to_hectare <- 0.404686

df <- data_test %>%
  mutate(FIRE=ifelse(nb==0,FALSE,TRUE),
         FIRE_prev_week = ifelse(lag(nb)==0,FALSE,TRUE),
         mois = str_sub(date_semaine,6,7),
         area_burned = area_burned*acres_to_hectare,
         percentage_area_burned = area_burned/county_size,
         percentage_area_burned_prev_week = lag(percentage_area_burned),
         # creation week day)
         jour_semaine = weekdays(date_semaine),
         week_end = ifelse(jour_semaine %in% c("samedi","dimanche"),TRUE,FALSE))

### J'aimerais bien créer aussi des variables SPRING BREAK et autre HOLIDAYS 

