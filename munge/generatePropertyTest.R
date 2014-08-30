###
# Generate simulated property data
###

library(maptools)


# read census.gov county shapefile data
us.counties <- readShapeSpatial("./data/tl_2014_us_county/tl_2014_us_county.shp",
                               proj4string = CRS("+proj=longlat +datum=WGS84"))

# read census.gov state shapefile data
us.states <-readShapeSpatial("./data/tl_2014_us_state/tl_2014_us_state.shp",
                            proj4string = CRS("+proj=longlat +datum=WGS84"))

# select only states of interest
states.of.interest <- subset(us.states,STUSPS %in% c("VA"), select=GEOID)
counties.of.interest <- subset(us.counties,STATEFP %in% states.of.interest$GEOID)

