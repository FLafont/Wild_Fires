library(tidyverse)
library(sf)
library(spdep)
## Find neighbouring counties

counties_selec <- read_csv("data/liste_counties_selectionnes.csv") %>%
  mutate(fips = ifelse(nchar(fips)<5,paste0("0",fips),fips))

counties <-  st_read("data/county_shp/tl_2015_us_county/tl_2015_us_county.shp") %>%
   filter(GEOID %in% counties_selec$fips) %>%
   mutate(row_id = row_number())

## find neighbourgs
# On utilise le package spdep 
# Qui utilise des objets de types "nb"
# queen = TRUE dit qu'une seule frontière en commun est acceptable
# (sinon "Rook", i.e. le fou est utilisé)


# A préciser: Certains counties n'auront pas l'air d'avoir de voisins
# car on n'a pas pris tous les counties 
counties_nb <- poly2nb(counties,queen = TRUE,snap = 5)

df_neighbours <- map2(counties_nb,counties$GEOID,
                         ~tibble(neighbours = .x,
                                 GEOID = .y))

# df_neighbours <- map2_df(counties_nb,counties$GEOID,
#                          ~tibble(neighbours = list(.x),
#                                  GEOID = .y)) 

## test de retrouver les voisons 
numbered_counties <- counties %>% tibble()%>% select(GEOID,row_id)

neighbours <- map(df_neighbours, 
                  ~numbered_counties %>% filter(row_id %in% .x$neighbours) %>%
  mutate(county = .x$GEOID)
)

### Calcul sur le jeu de trainb 
don <- read_xlsx("data/don_train.xlsx")


nb_feux_voisins <- map_df(neighbours,
                          ~  don %>%
                            filter(county %in% .x$GEOID) %>%
                            mutate(county_id = unique(.x$county))%>%
                            select(annee,semaine,nb,county,county_id)%>%
                            group_by(annee,semaine,county_id)%>%
                            summarise(nb_feux_voisins =sum(nb)))


don_avec_feux_voisins <- inner_join(don,nb_feux_voisins,by = c("county"="county_id",
                                                               "annee","semaine")) %>%
  mutate(nb_feux_voisins_lag = lag(nb_feux_voisins))

writexl::write_xlsx(don_avec_feux_voisins,"data/don_train_feux_voisins.xlsx")


### Calcul sur le jeu de test 
test <- read_xlsx("data/don_test.xlsx") %>%
  mutate(county=ifelse(nchar(county)<5,paste0("0",county),county))


nb_feux_voisins_test <- map_df(neighbours,
                          ~  test %>%
                            filter(county %in% .x$GEOID) %>%
                            mutate(county_id = unique(.x$county))%>%
                            select(annee,semaine,nb,county,county_id)%>%
                            group_by(annee,semaine,county_id)%>%
                            summarise(nb_feux_voisins =sum(nb)))


test_avec_feux_voisins <- inner_join(test,nb_feux_voisins_test,by = c("county"="county_id",
                                                               "annee","semaine")) %>%
  mutate(nb_feux_voisins_lag = lag(nb_feux_voisins))

writexl::write_xlsx(test_avec_feux_voisins,"data/don_test_feux_voisins.xlsx")
