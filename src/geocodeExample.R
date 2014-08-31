###
# Geocode and map address
###

library(ggmap)


address.of.interest <- "8250 Jones Branch Dr., McLean, VA"

# call Google web service API to geocode the address
location <- geocode(address.of.interest,output="more")

# show resulting geocoded address
cat("property is located at lon=",location[1,1],", lat=",location[1,2],"\n")

# create map centered on address
this.map <- get_map(address.of.interest,16)

# draw map and annotate
ggmap(this.map) +
    # plot plot
    geom_point(aes(x=lon, y=lat), data=location, shape=4, size=5, color="red") +
    
    # label point
    geom_text(aes(x=lon, y=lat), data= location, label="PHO 2", hjust=0.5, vjust=1.5,
              color="red", fontface="bold")
