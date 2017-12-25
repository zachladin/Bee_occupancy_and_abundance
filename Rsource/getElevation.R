install.packages("raster")
library(raster)

##read in data in csv format
data <- read.csv("C:/Users/Grace/Dropbox/Grace_Bees/Data/site_locations.csv")
coords<-data[,-1]

##download altitude data for USA
x <- getData('alt', country = "USA")
##subset to the continental USA
sub <- x[[1]]
#get elevations for each lon,lat combination
elevations<-cbind(coords, alt = extract(sub, coords, method = "bilinear"))

##add site names back in
colnames(data)<-c("site","latitude","longitude")
elevation<-cbind(data,elevations)
##check that all lat lon's match
elevation$check<-(elevation$latitude-elevation$lat)+(elevation$longitude-elevation$lon)
max(elevation$check) #all good!

#remove duplicate columns for lat long
elevation<-elevation[,-4]
elevation<-elevation[,-4]
elevation<-elevation[,-5]

##save file
setwd("C:/Users/Grace/Dropbox/Grace_Bees/Data")
write.csv(elevation,file="elevation_all.csv")
