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
    CP <- as(extent(bb$ll.lon, bb$ur.lon, bb$ll.lat, bb$ur.lat), 
             "SpatialPolygons")
    
    # project string for Google Maps
    proj4string(CP) <- CRS("+proj=longlat +datum=WGS84")  
    
    # apply cropping to spatial data
    crop.spatial.data <- gIntersection(spatial.data, CP, byid=TRUE)
    crop.spatial.data <- fortify(crop.spatial.data)
    
    # return the cropped spatial data
    invisible(crop.spatial.data)
    
    
}

# Multiple plot function
# based on code found at
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    require(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}
