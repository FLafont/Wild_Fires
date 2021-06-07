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
counties<- st_read('data/county_shp/tl_2011_us_county/tl_2011_us_county.shp')
counties <- st_transform(counties, proj4string(Cont_USA))

#### SAUVEGARDE PLOT : USA avec land cover
#plot_usa_land_cover<-plot(Cont_USA)
png(filename = "plots/USA_land_cover.png",
    width = 480, height = 480, units = "px")
plot(Cont_USA)
dev.off()

#ggsave(plot = plot_usa_land_cover,filename="plots/USA_land_cover.png",dpi = 600)

#### SAUVEGARDE PLOT : USA avec land cover et tous les counties
png(filename = "plots/USA_land_cover_counties.png",
    width = 480, height = 480, units = "px")
plot(Cont_USA)
plot(st_geometry(counties),add=TRUE)
dev.off()

#ggsave(plot = plot_usa_lc_counties,filename="plots/USA_land_cover_counties.png",dpi = 600)

#### SAUVEGARDE PLOT : USA avec tous les counties
png(filename = "plots/USA_counties_color.png",
    width = 480, height = 480, units = "px")
plot(Cont_USA)
plot(counties,add=TRUE)
dev.off()

# Selection des counties de notre liste 
liste_counties <- read_xlsx("data/liste_counties_selectionnes.xlsx")

liste_counties <- liste_counties %>%
  mutate(fips = ifelse(nchar(fips) < 5,
                       paste0("0", fips),
                       fips))


counties_selec <- subset(counties,counties$GEOID %in%liste_counties$fips )
counties_selec <- st_transform(counties_selec, proj4string(Cont_USA))
 

#### SAUVEGARDE PLOT : USA avec land cover et nos counties sélectionnés

png(filename="plots/USA_land_cover_counties_select.png",
    width = 480, height = 480, units = "px")
plot(Cont_USA)
plot_usa_counties_selec <- plot(st_geometry(counties_selec),add=TRUE)
dev.off()

