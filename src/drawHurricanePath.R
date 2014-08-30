###
# example code to draw Hurrican Storm Path
###

library(ggmap)
library(ggplot2)
library(maptools)
library(rgeos)
library(raster)

# function to draw forecasted hurrican path on Google Map
drawHurricanePath <- function(location="Washington, DC",zoom=4,storm.path) {
    # storm.path - Spatial data from forcasted storm path downloaded form NHC
    
    # generate mape at requested location and zoom level
    storm.map <- get_map(location,zoom=zoom)
    
    # calculate bounding box for displaying storm path
    bb <- attr(storm.map,"bb")
    
    # adjust bounding box to make it slightly smaller than map 
    epsilon <- 1e-6   
    bb <- bb + c(epsilon, epsilon, -epsilon, -epsilon)
    
    # create cropping bounding box for the requested map
    CP <- as(extent(bb$ll.lon, bb$ur.lon, bb$ll.lat, bb$ur.lat), "SpatialPolygons")
    proj4string(CP) <- CRS("+proj=longlat +datum=WGS84")  # project string for Google Maps
    
    # apply cropping to storm path data
    crop.storm.path <- gIntersection(storm.path, CP, byid=TRUE)
    crop.storm.path <- fortify(crop.storm.path)

    # generate map with storm path
    ggmap(storm.map) +
        geom_polygon(aes(x=long, y=lat, group=id), 
                     data=crop.storm.path,
                     color="red",fill="yellow", alpha=0.2,size=0.3) +
        theme_nothing()

    
}


# retieve storm path shapefile
storm.cone <- readShapeSpatial("./nhcdata/al182012_5day_025/al182012.025_5day_pgn.shp",
                               proj4string = CRS("+proj=longlat +datum=WGS84"))
# get only the 72-hour forecast
storm.path <- subset(storm.cone,FCSTPRD==72)
drawHurricanePath("arlington, va", 5,storm.path)
