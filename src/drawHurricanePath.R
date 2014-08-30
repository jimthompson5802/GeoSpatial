###
# example code to draw Hurrican Storm Path
###

library(ggmap)
library(ggplot2)
library(maptools)
library(rgeos)
library(raster)

# function to draw forecasted hurrican path on Google Map
cropHurricanePath <- function(the.map,storm.path) {
    # the.map - Google map to crop the storm path to
    # storm.path - Spatial data from forcasted storm path downloaded form NHC
    
    
    # calculate bounding box for displaying storm path
    bb <- attr(the.map,"bb")
    
    # adjust bounding box to make it slightly smaller than map 
    epsilon <- 1e-6   
    bb <- bb + c(epsilon, epsilon, -epsilon, -epsilon)
    
    # create cropping bounding box for the requested map
    CP <- as(extent(bb$ll.lon, bb$ur.lon, bb$ll.lat, bb$ur.lat), "SpatialPolygons")
    proj4string(CP) <- CRS("+proj=longlat +datum=WGS84")  # project string for Google Maps
    
    # apply cropping to storm path data
    crop.storm.path <- gIntersection(storm.path, CP, byid=TRUE)
    crop.storm.path <- fortify(crop.storm.path)

    # return the cropped storm path
    invisible(crop.storm.path)

    
}


# retieve storm path shapefile
storm.cone <- readShapeSpatial("./nhcdata/al182012_5day_025/al182012.025_5day_pgn.shp",
                               proj4string = CRS("+proj=longlat +datum=WGS84"))

# generate mape at requested location and zoom level
storm.map <- get_map("arlington, va",6)

# get only the 72-hour forecast
storm.path <- subset(storm.cone,FCSTPRD==72)
storm.path.to.display <- cropHurricanePath(storm.map,storm.path)

# generate map with storm path
ggmap(storm.map) +
    geom_polygon(aes(x=long, y=lat, group=id), 
                 data=storm.path.to.display,
                 color="red",fill="yellow", alpha=0.2,size=0.3) +
    theme_nothing()
