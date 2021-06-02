## Ajout population des counties 
library(tidyverse)
library(tidycensus)
library(censusapi)



key <- "982b4bbfc5dd20ae1373fabce29a43fa0d03b2fd"

data2000 <- censusapi::getCensus(
  name = "dec/sf1",
  vintage = 2000,
  vars = c("NAME", "P001001"),
  region="county:*",
  key=key)

data2010 <- censusapi::getCensus(
  name = "dec/sf1",
  vintage = 2010,
  vars = c("NAME", "P001001"),
  region="county:*",
  key=key)


data2015 <- tidycensus::get_estimates(geography = "county",
                          #product = "population",
                          variables = "POP",
                          key=key,
                          year = 2015) 




## Test avec fichier brut 
usa_pop_data <- read_csv("data/us.1969_2019.19ages.adjusted.txt")

colnames(usa_pop_data) <- "raw"

pop<- usa_pop_data %>%
  mutate(raw=as.character(raw),
         year=str_sub(raw,1,4),
         state=str_sub(raw,5,6),
         fips = str_sub(raw,7,11),
         census_tract = str_sub(raw,12,17),
         block=str_sub(raw,18,18))

