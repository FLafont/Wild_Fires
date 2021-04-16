library(tidyverse)
library(rnoaa)


key <- "TkUdJixWgJQbNsNqZZTSnYTuUaFXdrac"
datasets <- ncdc_datasets(
  token = key
)

df_sets <- tibble(name=datasets$data$name ,
                  id=datasets$data$id,
                  uid=datasets$data$uid)

#class(datasets)


stations <- ncdc_stations(datasetid='GHCND', 
                          locationid='*',
                          token = key)
