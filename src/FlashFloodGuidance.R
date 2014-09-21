###
#  Flash flood guidance
###

library(ggmap)
library(maptools)
library(rgeos)
library(raster)

source("CommonFunctions.R")


# retieve storm path shapefile
flash.flood <- readShapeSpatial("./data/nws_ffg/nws_ffg.shp",
                               proj4string = CRS("+proj=longlat +datum=WGS84"))


# display high-level map
us <- get_map("United States",4)
region.to.display <- cropToMap(us,flash.flood)

flash.flood.map <- ggmap(us) +
    geom_polygon(aes(x=long, y=lat, group=id), 
                 data=region.to.display,
                 color="red",fill="yellow", alpha=0.2,size=1) +
    theme_nothing()

print(flash.flood.map)
