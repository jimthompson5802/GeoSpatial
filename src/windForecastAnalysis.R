###
#  Wind Forecast Data
###
library(ggmap)
library(maptools)
library(rgeos)
library(raster)
library(grid)

source("CommonFunctions.R")

# retieve wind forecast shapefile
wind.fcst <- readShapeSpatial(paste0("../nhcdata/al182012_fcst_025",
                                    "/al182012_2012102812_forecastradii.shp"),
                               force_ring=TRUE,
                               proj4string = CRS("+proj=longlat +datum=WGS84"))
# select 72-hour forecast
wind.36 <- subset(wind.fcst,TAU==36)
wind.36@data$poly_id <- rownames(wind.36@data)


# generate map and crop wind speed polygons to map area
ec <- get_map("arlington, va",6)
wind.36.to.display <- cropToMap(ec,wind.36)
poly.ids <- data.frame(do.call(rbind,strsplit(wind.36.to.display$id," ")),
                       stringsAsFactors=FALSE)
names(poly.ids) <- c("poly_id","segment_id")
wind.36.to.display <- cbind(wind.36.to.display,poly.ids)


# combine cropped map polygon data with forecast data 
wind.36.to.display <- merge(wind.36.to.display, wind.36@data)


# draw map and overlay with wind speed forecast
wind.map <- ggmap(ec) +
    geom_polygon(aes(x=long, y=lat, group=id, fill=as.character(RADII)),
                 data=subset(wind.36.to.display,RADII==34), # tropical storm winds
                 color="red", alpha=0.2, size=0.3) +
    geom_polygon(aes(x=long, y=lat, group=id, fill=as.character(RADII)),
                 data=subset(wind.36.to.display,RADII==50), # 50 knot winds
                 color="red", alpha=0.2, size=0.3) +
    geom_polygon(aes(x=long, y=lat, group=id, fill=as.character(RADII)),
                 data=subset(wind.36.to.display,RADII==64), # hurricane winds
                 color="red", alpha=0.2, size=0.3) +
    scale_fill_manual(values=c("yellow","blue","red"),
                      name="Wind Speed",
                      labels=c("Tropical Storm", "50 Knot","Hurricane")) +
    theme(legend.key.size=unit(2,"lines"),
          legend.title=element_text(size=15),
          legend.text=element_text(size=15))

png("../figures/wind_forecast.png",width=1024,height=1024)
print(wind.map)
dev.off()

