## Ajout population des counties 
library(tidyverse)
library(tidycensus)
library(censusapi)

library(readxl)
library(writexl)


key <- "982b4bbfc5dd20ae1373fabce29a43fa0d03b2fd"

data2000 <- censusapi::getCensus(
  name = "dec/sf1",
  vintage = 2000,
  vars = c("NAME", "P001001"),
  region="county:*",
  key=key)

write_csv(data2000,"data/county_census_pop_2000.csv")

data2010 <- censusapi::getCensus(
  name = "dec/sf1",
  vintage = 2010,
  vars = c("NAME", "P001001"),
  region="county:*",
  key=key)

write_csv(data2010,"data/county_census_pop_2010.csv")

data2015 <- tidycensus::get_estimates(geography = "county",
                          #product = "population",
                          variables = "POP",
                          key=key,
                          year = 2015) 

write_csv(data2015,"data/county_census_pop_2015.csv")

## une seule table 
data2000 <- data2000 %>%
  mutate(GEOID = paste0(state,county))%>%
  rename(pop_2000 = P001001)
data2010 <- data2010 %>%
  mutate(GEOID = paste0(state,county))%>%
  rename(pop_2010 = P001001)
data_pop <- inner_join(select(data2015,-NAME),data2010)%>%
  inner_join(data2000)%>%
  select(everything(),pop_2015=value)

# Calcul du taux de croissance moyen 
data_pop <- data_pop %>%
  mutate(gadm_10_00 = ((pop_2010/pop_2000)^(1/(2010-2000))-1),
         gadm_15_10 = ((pop_2015/pop_2010)^(1/(2015-2010))-1))



## Ajout de la population au jeu train
#train <- read_xlsx("data/don_train.xlsx")

train <- read_csv("data/semaine_train.csv") %>%
  mutate(county = ifelse(nchar(county)<5,paste0("0",county),county))



train_pop <- inner_join(data_pop,train,by=c("GEOID"="county")) %>%
  group_by(county)%>%
  mutate(pop_year = case_when(
    annee == "2000"~pop_2000,
    annee == "2001"~pop_2000*(1+gadm_10_00),
    annee == "2002"~pop_2000*(1+gadm_10_00)^(2002-2000),
    annee == "2003"~pop_2000*(1+gadm_10_00)^(2003-2000),
    annee == "2004"~pop_2000*(1+gadm_10_00)^(2004-2000),
    annee == "2005"~pop_2000*(1+gadm_10_00)^(2005-2000),
    annee == "2006"~pop_2000*(1+gadm_10_00)^(2006-2000),
    annee == "2007"~pop_2000*(1+gadm_10_00)^(2007-2000),
    annee == "2008"~pop_2000*(1+gadm_10_00)^(2008-2000),
    annee == "2009"~pop_2000*(1+gadm_10_00)^(2009-2000),
    annee == "2010"~pop_2010)
  ) %>% ungroup() %>%
  mutate(pop_year = round(pop_year)) %>%
  select(-c(pop_2015,pop_2000,pop_2010,gadm_10_00,gadm_15_10))

train_pop <- train_pop %>%
  select(-county,-variable,-NAME)%>%
  rename(county=GEOID)

write_csv(train_pop,"semaine_train.csv")
## Ajout de la population au jeu test
# test <- read_xlsx("data/don_test.xlsx") %>%
#   mutate(county=ifelse(nchar(county)<5,paste0("0",county),county))
test <- read_csv("data/semaine_test.csv")  %>% 
  mutate(county=ifelse(nchar(county)<5,paste0("0",county),county))



test_pop <- inner_join(data_pop,test,by=c("GEOID"="county")) %>%
  group_by(county)%>%
  mutate(pop_year = case_when(
    annee == "2011"~pop_2010*(1+gadm_15_10),
    annee == "2012"~pop_2010*(1+gadm_15_10)^(2012-2010),
    annee == "2013"~pop_2010*(1+gadm_15_10)^(2013-2010),
    annee == "2014"~pop_2010*(1+gadm_15_10)^(2014-2010),
    annee == "2015"~pop_2015)
  )%>% ungroup() %>%
  mutate(pop_year = round(pop_year)) %>%
  select(-c(pop_2015,pop_2000,pop_2010,gadm_10_00,gadm_15_10))

test_pop <-test_pop%>%
  select(-county,-variable,-NAME)%>%
  rename(county=GEOID)

write_csv(test_pop,"data/semaine_test.csv")
## SAVE 


## Test avec fichier brut 
# usa_pop_data <- read_csv("data/us.1969_2019.19ages.adjusted.txt")
# 
# colnames(usa_pop_data) <- "raw"
# 
# pop<- usa_pop_data %>%
#   mutate(raw=as.character(raw),
#          year=str_sub(raw,1,4),
#          state=str_sub(raw,5,6),
#          fips = str_sub(raw,7,11),
#          census_tract = str_sub(raw,12,17),
#          block=str_sub(raw,18,18))
# 
