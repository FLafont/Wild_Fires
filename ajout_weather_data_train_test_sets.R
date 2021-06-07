library(tidyverse)
### add weather data to test and train

train_weather <- read_csv("data/train_aggregated_weather_1_853.csv") %>% select(-1,-2) %>%
  mutate(annee=str_sub(date_target,1,4),
         geoid = ifelse(nchar(geoid)<5,paste0("0",geoid),geoid))

data_train <- read_csv("data/semaine_train.csv") 
  
magic <- left_join(data_train,train_weather %>% select(-week,-annee),
                   by = c("county"="geoid",
                          "date_semaine"="date_target"))

write_csv(magic,"magic_train.csv")

# TEST 
test_weather <- read_csv("data/test_aggregated_weather_1_853.csv") %>% select(-1,-2) %>%
  mutate(annee=str_sub(date_target,1,4),
         geoid = ifelse(nchar(geoid)<5,paste0("0",geoid),geoid))

data_test <- read_csv("data/semaine_test.csv") 

magic_test <- left_join(data_test,test_weather %>% select(-week,-annee),
                   by = c("county"="geoid",
                          "date_semaine"="date_target"))

write_csv(magic_test,"magic_test.csv")

