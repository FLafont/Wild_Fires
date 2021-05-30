library(FedData)
library(raster)
library(sf)

# Get the NLCD (USA ONLY)

Cont_USA <- raster("NLCD_2016_Land_Cover_L48_20190424.img")


attr <- Cont_USA@data@attributes[[1]]
Cont_USA@crs


shp <- read_sf("USA_Counties.shp") 

which_fips <- "04001" 
sub_shp <- subset(shp, FIPS == which_fips)

test <- raster::extract(x = Cont_USA, 
                               y = sub_shp, 
                               df = TRUE)

st_as_sf(Cont_USA, as_points = TRUE, merge = FALSE)
