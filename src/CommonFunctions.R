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
