###
# example code to draw Hurrican Storm Path
###

library(ggmap)
library(ggplot2)
library(maptools)
library(rgeos)
library(raster)

# function to crop spatial data to bounding box of a Google Map
cropToMap <- function(the.map,spatial.data) {
    # the.map - Google map to crop the storm path to
    # spatial.data - Spatial data to display on map
    
    
    # calculate bounding box for displaying storm path
    bb <- attr(the.map,"bb")
    
    # adjust bounding box to make it slightly smaller than map 
    epsilon <- 1e-6   
    bb <- bb + c(epsilon, epsilon, -epsilon, -epsilon)
    
    # create cropping bounding box for the requested map
    CP <- as(extent(bb$ll.lon, bb$ur.lon, bb$ll.lat, bb$ur.lat), "SpatialPolygons")
    proj4string(CP) <- CRS("+proj=longlat +datum=WGS84")  # project string for Google Maps
    
    # apply cropping to spatial data
    crop.spatial.data <- gIntersection(spatial.data, CP, byid=TRUE)
    crop.spatial.data <- fortify(crop.spatial.data)

    # return the cropped spatial data
    invisible(crop.spatial.data)

    
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
    ggtitle("Hurricane Sandy Forecast Path as of 10/28/2012") +
    theme(legend.position="none")
