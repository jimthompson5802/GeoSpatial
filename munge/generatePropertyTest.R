###
# Generate simulated property data
###

library(ggmap)
library(ggplot2)
library(maptools)
library(sp)




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

this.map <- get_map("arlington, virginia",8)
county.boundaries <- cropToMap(this.map,counties.of.interest)


ggmap(this.map) +
    geom_polygon(aes(x=long, y=lat, group=id), 
                 data=county.boundaries,
                 color="red",alpha=0) +
    geom_text(aes(x=as.numeric(as.character(INTPTLON)), 
                  y=as.numeric(as.character(INTPTLAT)), label=NAME),
              data=attr(counties.of.interest,"data"),
              size=3)

###
# generate simulate property locations in the counties of interest
###

# generate long/lat coordinates and property value
generatePropertyData <- function(sp) {
    # get Polygon definition for a county
    polygon <- attr(sp,"Polygons")[[1]]
    
    # get coordinates for the polygon defintion
    coords <- attr(polygon,"coords")
    colnames(coords) <- c("long","lat")
    
    location.points <- SpatialPoints(coords,proj4string="+proj=longlat +datum=WGS84")
    
    invisible(location.points)
}

ll <- lapply(attr(counties.of.interest,"polygons"), generatePropertyData)

