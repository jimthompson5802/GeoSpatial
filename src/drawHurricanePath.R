###
# example code to draw Hurricane Storm Path
###

library(ggmap)
library(maptools)
library(rgeos)
library(raster)


source("CommonFunctions.R")

# retrieve simulated property location data
load("../data/property_locations.RData")

# convert property location to Spatial data for testing in or out of region
property.locations <- SpatialPoints(property.df[,1:2],
                                    proj4string=CRS("+proj=longlat +datum=WGS84"))

# read census.gov county shapefile data
us.counties <- readShapeSpatial("../data/tl_2014_us_county/tl_2014_us_county.shp",
                                proj4string = CRS("+proj=longlat +datum=WGS84"))

# read census.gov state shapefile data
us.states <-readShapeSpatial("../data/tl_2014_us_state/tl_2014_us_state.shp",
                             proj4string = CRS("+proj=longlat +datum=WGS84"))

# select only counties of interest
counties.of.interest <- subset(us.counties, STATEFP == 51 &
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
                 color="black",alpha=0) +
    geom_text(aes(x=as.numeric(as.character(INTPTLON)), 
                  y=as.numeric(as.character(INTPTLAT)), label=NAME),
              data=attr(counties.of.interest,"data"),
              fontface="bold", color="red", size=3) +
    theme_nothing()

png("../figures/base_property_locations.png")
print(storm.map)
dev.off()


# retieve storm path shapefile
storm.cone <- readShapeSpatial("../nhcdata/al182012_5day_025/al182012.025_5day_pgn.shp",
                               proj4string = CRS("+proj=longlat +datum=WGS84"))


# get only the 72-hour forecast
storm.path <- subset(storm.cone,FCSTPRD==72)

# display high-level map
ec <- get_map("arlington, va",6)
storm.path.to.display <- cropToMap(ec,storm.path)

labpt <- attr(attr(storm.path,"polygons")[[1]],"labpt")

storm.map <- ggmap(ec) +
    geom_polygon(aes(x=long, y=lat, group=id), 
                 data=storm.path.to.display,
                 color="red",fill="yellow", alpha=0.2,size=1) +
    geom_text(aes(x=labpt[1], y=labpt[2]), 
              label="Hurricane Sandy\n72-hour Forecast Path", 
              size=10,
              color="red") +
    theme_nothing()
png("../figures/high-level_storm_path.png")
print(storm.map)
dev.off()

ec <- get_map("arlington, va",7)
storm.path.to.display <- cropToMap(ec,storm.path)
states.to.display <- cropToMap(ec,us.states)

png("../figures/mid-level_storm_path.png")
ggmap(ec) +
    geom_polygon(aes(x=long, y=lat, group=id),
                 data=states.to.display,
                 color="black",alpha=0,size=0.3) +
    geom_polygon(aes(x=long, y=lat, group=id), 
                 data=storm.path.to.display,
                 color="red",fill="yellow", alpha=0.2,size=0.3) +
    #     ggtitle("Hurricane Sandy 3-Day Forecast Path as of 10/28/2012") +
    theme_nothing()
dev.off()

# determine the properties in the storm path region


# extract out storm path polygon data for testing
sp.storm <- SpatialPolygons(Srl=attr(storm.path,"polygons"))
proj4string(sp.storm) <- CRS(proj4string(storm.path))

# determine the properties in the storm path region
flag <- over(property.locations,sp.storm)
property.df$col <- factor(ifelse(!is.na(flag),"In Storm Path","Not In Storm Path"),
                          levels=c("In Storm Path","Not In Storm Path"))
property.df$pch <- ifelse(!is.na(flag),"17","16")
property.count <- length(flag)
property.value<- sum(floor(property.df$value))
upb.value <- sum(floor(property.df$upb))
property.count.at.risk <- sum(!is.na(flag))
property.value.at.risk <- sum(floor(property.df$value[is.na(flag)]))
upb.value.at.risk <- sum(floor(property.df$upb[is.na(flag)]))


# plot property locations
# print map with property locations
storm.map <- ggmap(base.map) +
    geom_point(aes(x=lon, y=lat, color=col, shape=pch), 
               data=property.df, 
               size=3) +
    scale_color_manual(values=c("red","black"))+
    geom_polygon(aes(x=long, y=lat, group=id), 
                 data=county.boundaries,
                 color="black",alpha=0) +
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
    theme_nothing()

png("../figures/affected_properties.png")
print(storm.map)
dev.off()


# do analytics on propeties
library(plyr)

df2 <- ddply(property.df,.(NAMELSAD,col),summarize,value=sum(value),
             upb=sum(upb))


p1 <- ggplot(df2, aes(x = NAMELSAD, y = upb,fill=col)) +
    scale_fill_manual(name="Property Category",values=c("red","green")) +
    scale_y_continuous(breaks=seq(0,4000000,500000),
                       limits=c(0,4000000),
                       labels=paste0("$",seq(0,4,0.5),"M"))+ 
    geom_bar(stat='identity') +
    coord_flip()+
    xlab("Region") + ylab("UPB Value") +
    theme(axis.text.y=element_text(size=10),
          axis.text.x=element_text(size=10))

p2 <- ggplot(df2, aes(x = NAMELSAD, y = value,fill=col)) +
    scale_fill_manual(name="Property Category",values=c("red","green")) +
    scale_y_continuous(breaks=seq(0,4000000,500000),
                       limits=c(0,4000000),
                       labels=paste0("$",seq(0,4,0.5),"M"))+ 
    geom_bar(stat='identity') +
    coord_flip()+
    xlab("Region") + ylab("Property Value") +
    theme(axis.text.y=element_text(size=10),
          axis.text.x=element_text(size=10))

png("../figures/property_analytics1.png")
multiplot(p1, p2, cols=1)
dev.off()
