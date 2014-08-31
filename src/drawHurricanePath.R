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

# retrieve simulated property location data
load("./data/property_locations.RData")

# read census.gov county shapefile data
us.counties <- readShapeSpatial("./data/tl_2014_us_county/tl_2014_us_county.shp",
                                proj4string = CRS("+proj=longlat +datum=WGS84"))

# read census.gov state shapefile data
us.states <-readShapeSpatial("./data/tl_2014_us_state/tl_2014_us_state.shp",
                             proj4string = CRS("+proj=longlat +datum=WGS84"))

# select only states of interest
states.of.interest <- subset(us.states,STUSPS %in% c("VA"), select=GEOID)
counties.of.interest <- subset(us.counties,
                               NAME %in% c("Arlington", "Fairfax",
                                           "Alexandria",
                                           "Loudoun","Culpeper",
                                           "Rappahannock", "Fauquier",
                                           "Stafford","Prince William"))


# generate mape at requested location and zoom level
base.map <- get_map("prince william, va",9)
county.boundaries <- cropToMap(base.map,counties.of.interest)

# print map with property locations
storm.map <- ggmap(base.map) +
    geom_point(aes(x=lon, y=lat), 
               data=property.df, 
               shape=16, size=3) +
    geom_polygon(aes(x=long, y=lat, group=id), 
                 data=county.boundaries,
                 color="red",alpha=0) +
    geom_text(aes(x=as.numeric(as.character(INTPTLON)), 
                  y=as.numeric(as.character(INTPTLAT)), label=NAME),
              data=attr(counties.of.interest,"data"),
              fontface="bold", color="red", size=3) +
    theme_nothing()

print(storm.map)


# retieve storm path shapefile
storm.cone <- readShapeSpatial("./nhcdata/al182012_5day_025/al182012.025_5day_pgn.shp",
                               proj4string = CRS("+proj=longlat +datum=WGS84"))


# get only the 72-hour forecast
storm.path <- subset(storm.cone,FCSTPRD==72)

# display high-level map
ec <- get_map("arlington, va",6)
storm.path.to.display <- cropToMap(ec,storm.path)

ggmap(ec) +
    geom_polygon(aes(x=long, y=lat, group=id), 
                 data=storm.path.to.display,
                 color="red",fill="yellow", alpha=0.2,size=0.3) +
    ggtitle("Hurricane Sandy 3-Day Forecast Path as of 10/28/2012") +
    theme(legend.position="none")

ec <- get_map("arlington, va",7)
storm.path.to.display <- cropToMap(ec,storm.path)

ggmap(ec) +
    geom_polygon(aes(x=long, y=lat, group=id), 
                 data=storm.path.to.display,
                 color="red",fill="yellow", alpha=0.2,size=0.3) +
    ggtitle("Hurricane Sandy 3-Day Forecast Path as of 10/28/2012") +
    theme(legend.position="none")

# determine the properties in the storm path region
# convert property location to Spatial data for testing in or out of region
property.locations <- SpatialPoints(property.df[,1:2],
                                    proj4string=CRS("+proj=longlat +datum=WGS84"))

# extract out storm path polygon data for testing
sp.storm <- SpatialPolygons(Srl=attr(storm.path,"polygons"))
proj4string(sp.storm) <- CRS(proj4string(storm.path))

# determine the properties in the storm path region
flag <- over(property.locations,sp.storm)
property.df$col <- factor(ifelse(!is.na(flag),"in","out"),levels=c("in","out"))

# plot property locations
# print map with property locations
storm.map <- ggmap(base.map) +
    geom_point(aes(x=lon, y=lat, color=col), 
               data=property.df, 
               shape=16, size=3) +
    scale_color_manual(values=c("red","blue"))+
    geom_polygon(aes(x=long, y=lat, group=id), 
                 data=county.boundaries,
                 color="red",alpha=0) +
    geom_text(aes(x=as.numeric(as.character(INTPTLON)), 
                  y=as.numeric(as.character(INTPTLAT)), label=NAME),
              data=attr(counties.of.interest,"data"),
              fontface="bold", color="red", size=3) +
    theme_nothing()


storm.path.to.display <- cropToMap(base.map,storm.path)
# generate map with storm path
storm.map <- storm.map +
    geom_polygon(aes(x=long, y=lat, group=id), 
                 data=storm.path.to.display,
                 color="red",fill="yellow", alpha=0.2,size=0.3) +
#     ggtitle("Hurricane Sandy Affected Areas") +
    theme(legend.position="none")
print(storm.map)
