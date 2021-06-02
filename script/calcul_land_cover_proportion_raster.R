library(rgdal)
library(dplyr)
library(raster)
library(sf)
library(purrr)
library(writexl)
library(readxl)
# RASTER NLCD USA 
Cont_USA <- raster("data/NLCD_2011_Land_Cover_L48_20190424/NLCD_2011_Land_Cover_L48_20190424.img")

# Shapefiles counties 2011
# counties<- st_read('data/county_shp/tl_2011_us_county/tl_2011_us_county.shp')
# counties <- st_transform(counties, proj4string(Cont_USA))


plot(Cont_USA)
plot(counties,add=TRUE)

# Selection des counties de notre liste 
liste_counties <- read_xlsx("data/liste_counties_selectionnes.xlsx") %>%
  mutate(fips=ifelse(nchar(fips)<5,
                     paste0("0",fips),
                     fips))

# counties<- readOGR('data/county_shp/tl_2011_us_county/tl_2011_us_county.shp')
# counties_selec <- subset(counties,counties$GEOID %in%liste_counties$fips )
# counties_selec <- spTransform(counties_selec, proj4string(Cont_USA))
# 
# #counties_selec <- counties %>%dplyr::filter(GEOID %in% liste_counties$fips)
#   
# plot(Cont_USA)
# plot_usa_counties_selec <- plot(counties_selec,add=TRUE)
# 
# ggsave(plot = plot_usa_counties_selec,filename="plots/raster_USA_avec_counties_selectionnes.png",dpi = 600)


counties<- st_read('data/county_shp/tl_2011_us_county/tl_2011_us_county.shp')
counties_selec <- subset(counties,counties$GEOID %in%liste_counties$fips )
counties_selec <- st_transform(counties_selec, proj4string(Cont_USA))

list_counties <-  counties_selec %>%
  group_by(GEOID)%>%
  group_split()

e <- purrr::map(list_counties, ~extent(.x))

### CROP AVEC UN COUNTY POUR TESTER 
# Image pour slides
# COUNTY 1: 
# A/ CROP
# crop1 <- crop(Cont_USA,e[[1]])
# plot(crop1)
# 
# # B/ MASK LA PARTIE INUTILE
# crop1_2 <- mask(crop1,list_counties[[1]])
# plot(crop1_2)


### Meme chose sur l'ensemble des counties 
# A/ CROP
# PREND ENVIRON 30 MIN SUR L'ORDI DU CEPE 
start <- Sys.time()
crops <- map(e,~crop(Cont_USA,.x))
end <- Sys.time()
end-start
start <- Sys.time()


# CALCUL LANDCOVER SUR TOUS LES COUNTIES 

calcul_lc <- function(crop){
  # prend un raster croppé comme argument
  # calcul la répartition des pixels par catégorie
  lc <- crop %>%
    values() %>%
    tibble::tibble(landcover = .) %>%
    na.omit() %>%
    dplyr::group_by(landcover) %>%
    count(name = "freq") %>%
    dplyr::mutate(area = (freq * 900) %>%
                    units::set_units("m^2") %>%
                    units::set_units("km^2"))
  return(lc)
  
}

list_lc <- list()

for (i in 1:length(crops)){
  print("---------------------")
  list_lc[[i]] <- calcul_lc(crops[[i]])
  print(paste("county",i,"terminé."))
}

df_lc <- map2(list_lc,list_counties,~.x %>%
               mutate(fips=.y$GEOID))
                
df_lc <- bind_rows(df_lc)

## Export 
write_xlsx(df_lc,"data/landcover_repartition_by_count_nlcd_2011.xlsx")
#write_xlsx(df_lc,"data/landcover_repartition_by_count_nlcd_2016.xlsx")

# calcul_lc <- function(crop){
#   # prend un raster croppé comme argument
#   # calcul la répartition des pixels par catégorie
#   temp <- data.frame(x2011 = values(.x))%>%
#     dplyr::filter(!is.na(x2011))
#   
#   temp_freq <- as.data.frame(table(temp))
#   
#   res <- temp_freq %>%
#     mutate(area_type_sqm = Freq*900,
#            area_type_km=area_type_sqm/1e6,
#            area_sqkm = sum(area_type_km))%>%
#     group_by(lc)%>%
#     mutate(pc_land =round(100*area_type_km/area_sqkm,1))
#   
#   return(res)
#   
# }
# 
