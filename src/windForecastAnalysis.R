###
#  Wind Forecast Data
###
library(ggmap)
library(maptools)
library(rgeos)
library(raster)

source("CommonFunctions.R")

# retieve wind forecast shapefile
wind.fcst <- readShapeSpatial("../nhcdata/al182012_fcst_025/al182012_2012102812_forecastradii.shp",
                               force_ring=TRUE,
                               proj4string = CRS("+proj=longlat +datum=WGS84"))
# select 72-hour forecast
wind.36 <- subset(wind.fcst,TAU==36)



ec <- get_map("arlington, va",7)
wind.36.to.display <- cropToMap(ec,wind.36)


ggmap(ec) +
    geom_polygon(aes(x=long, y=lat, group=id, fill=RADII),
                 data=wind.36.to.display,
                 color="red", alpha=0.2, size=0.3) +
    theme_nothing()