#devtools::install_github("ropensci/FedData")
library(FedData)
library(rgdal)
library(dplyr)
library(raster)
library(sf)
library(purrr)
#Get Apache polygone
counties<- readOGR('tl_2016_us_county/tl_2016_us_county.shp')
apache <- subset(counties,counties$GEOID=="04001")

# Get NCLD data 
nlcd_data <- get_nlcd(apache,
                      year = 2011,
                      label = "Apache",
                      force.redo = TRUE)

nlcd_data #inspect the object, can see that number of cells is around 57 million

## Calcul de la répartition du land cover 
landcover<-data.frame(x2011 = values(nlcd_data)) #Number of rows corresponds to number of cells 
landcover_freq<-table(landcover)

df_landcover <- as.data.frame(landcover_freq)

res <- df_landcover %>%
  mutate(area_type_sqm = Freq*900,
         area_type_km=area_type_sqm/1e6,
         area_sqkm = sum(area_type_km))%>%
  group_by(landcover)%>%
  mutate(pc_land =round(100*area_type_km/area_sqkm,1))

## Plot pour vérifier que le count est bien sur le raster 
apache <- spTransform(apache,proj4string(nlcd_data))
plot(nlcd_data)
plot(apache,add=TRUE)



# RASTER NLCD USA 
Cont_USA <- raster("NLCD_2016_Land_Cover_L48_20190424.img")

counties <- spTransform(counties, proj4string(Cont_USA))

plot(Cont_USA)
plot(counties,add=TRUE)

### DECOUPAGE DU RASTER USA CONTINENTAL AVEC LES POLYGONES 
library(sf)
counties<- st_read('tl_2016_us_county/tl_2016_us_county.shp')
counties <- st_transform(counties,crs=proj4string(Cont_USA)) 

counties_cont <- counties %>%
  filter(!(STATEFP %in% c("02","72","78","60","66","15","69")))

list_counties <-  counties_cont %>%
  group_by(GEOID)%>%
  group_split()

e <- purrr::map(list_counties, ~extent(.x))

### CROP AVEC UN COUNTY POUR TESTER 
# COUNTY 1: 
# A/ CROP
crop1 <- crop(Cont_USA,e[[1]])
plot(crop1)

# B/ MASK LA PARTIE INUTILE
crop1_2 <- mask(crop1,list_counties[[1]])
plot(crop1_2)

# CALCUL LANDCOVER 
lc<-data.frame(x2011 = values(crop1_2)) %>%
  dplyr::filter(!is.na(x2011))
lc_freq<-table(lc)

df_lc <- as.data.frame(lc_freq)

res <- df_lc %>%
  mutate(area_type_sqm = Freq*900,
         area_type_km=area_type_sqm/1e6,
         area_sqkm = sum(area_type_km))%>%
  group_by(lc)%>%
  mutate(pc_land =round(100*area_type_km/area_sqkm,1))



### Meme chose sur l'ensemble des counties 
# A/ CROP
# PREND ENVIRON 30 MIN SUR L'ORDI DU CEPE 
start <- Sys.time()
crops <- map(e,~crop(Cont_USA,.x))
end <- Sys.time()
end-start
start <- Sys.time()


# crops <- list()
# 
# for (i in 1:length(list_counties)){
#   print("-------------------")
#   crops[[i]] <- crop(Cont_USA,e[[i]])
#   print(paste0("County n°",i," terminé"))
# }

# B/ MASK 
crops_masked <- list()
for (i in 1:length(list_counties)){
  print("--------------------------------")
  crops_masked[[i]] <- mask(crops[[i]],
                            list_counties[[i]])  
  print(paste0("County n°",i," terminé."))
}

list_counties[[1725]]

crops_masked <- map2(crops,list_counties,
                     ~mask(.x,.y))
end <- Sys.time()
end-start

# CALCUL LANDCOVER SUR TOUS LES COUNTIES 

calcul_lc <- function(crop){
  # prend un raster croppé comme argument
  # calcul la répartition des pixels par catégorie
  temp <- data.frame(x2011 = values(.x))%>%
    dplyr::filter(!is.na(x2011))
  
  temp_freq <- as.data.frame(table(temp))
  
  res <- temp_freq %>%
    mutate(area_type_sqm = Freq*900,
           area_type_km=area_type_sqm/1e6,
           area_sqkm = sum(area_type_km))%>%
    group_by(lc)%>%
    mutate(pc_land =round(100*area_type_km/area_sqkm,1))
  
  return(res)
    
}
  
list_lc <- map(crops_masked,
               ~calcul_lc)




