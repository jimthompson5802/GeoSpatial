###
# Generate simulated property data
###

library(ggmap)
library(ggplot2)
library(maptools)
library(sp)
library(raster)
library(rgeos)

source("CommonFunctions.R")

# read census.gov county shapefile data
us.counties <- readShapeSpatial("../data/tl_2014_us_county/tl_2014_us_county.shp",
                                proj4string = CRS("+proj=longlat +datum=WGS84"))

# read census.gov state shapefile data
us.states <-readShapeSpatial("../data/tl_2014_us_state/tl_2014_us_state.shp",
                             proj4string = CRS("+proj=longlat +datum=WGS84"))

# select only states of interest
states.of.interest <- subset(us.states,STUSPS %in% c("VA"), select=GEOID)
counties.of.interest <- subset(us.counties,STATEFP == 51 &
                                   NAME %in% c("Arlington", "Fairfax",
                                               "Alexandria",
                                               "Loudoun","Culpeper",
                                               "Rappahannock", "Fauquier",
                                               "Stafford","Prince William"))


###
# generate simulate property locations in the counties of interest
###

# generate long/lat coordinates and property value
generatePropertyData <- function(sp, num.pts=5) {
    # get Polygon definition for a county
    polygon <- attr(sp,"Polygons")[[1]]
    
    # get coordinates for the polygon defintion
    coords <- attr(polygon,"coords")
    colnames(coords) <- c("long","lat")
    
    # compute bounding box for the region
    bb <- c(min(coords[,"long"]),min(coords[,"lat"]),
            max(coords[,"long"]),max(coords[,"lat"]))
    names(bb) <- c("ll.lon","ll.lat","ur.lon","ur.lat")
    
    # randomly "place" points in the region, 
    # this is not perfect some will be out of region
    set.seed(13)
    lon.pts <- runif(num.pts, bb["ll.lon"], bb["ur.lon"])
    lat.pts <- runif(num.pts, bb["ll.lat"], bb["ur.lat"])
    value <- runif(num.pts,50000,200000)
    
    invisible(cbind(lon=lon.pts,lat=lat.pts, value=value))
}

ll <- lapply(attr(counties.of.interest,"polygons"), generatePropertyData,10)

df <- data.frame(do.call(rbind,ll))

property.locations <- SpatialPointsDataFrame(df[,1:2],
                                data=data.frame(value=df[,3]),
                                proj4string=CRS("+proj=longlat +datum=WGS84"))
# makes points are in the counties of interest
sp.polygons <- SpatialPolygons(Srl=attr(counties.of.interest,"polygons"))
proj4string(sp.polygons) <- CRS(proj4string(counties.of.interest))
flag <- over(property.locations,sp.polygons)
df$col <- factor(ifelse(!is.na(flag),"in","out"),levels=c("in","out"))
df <- df[df$col=="in",]

# this.map <- get_map("arlington, virginia",9)
# county.boundaries <- cropToMap(this.map,counties.of.interest)
# 
# ggmap(this.map) +
#     geom_point(aes(x=lon, y=lat), 
#                data=df, 
#                color="black", shape=16, size=3) +
#     geom_polygon(aes(x=long, y=lat, group=id), 
#                  data=county.boundaries,
#                  color="red",alpha=0) +
#     geom_text(aes(x=as.numeric(as.character(INTPTLON)), 
#                   y=as.numeric(as.character(INTPTLAT)), label=NAME),
#               data=attr(counties.of.interest,"data"),
#               size=3) +
#     theme_nothing()

# Save property locations for analysis
property.df <- subset(df,select=-col)
property.count <- nrow(property.df)
property.value<- sum(floor(property.df$value))
save(property.df,file="../data/property_locations.RData")
