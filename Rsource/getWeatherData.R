##install these libraries
library(geonames)
library(XLConnect)
library("devtools")
#install_github("Ram-N/weatherData")
library(weatherData)

setwd("/Users/Zach/Dropbox (ZachTeam)/Projects/Grace_Bees")
new.data<-read.csv(paste(getwd(),"Data","Bee.formatted.data.csv",sep="/"),header=TRUE)

locationData<-new.data[,c("site","year","latitude","longitude","week_number","time1","time2")]
locationData$site.week.year<-paste(locationData$site, locationData$week_number, locationData$year, sep="_")

#read in all possible sampled locations
all.sampled<-read.csv(paste(getwd(),"Data","ALL_Possible_Sampled.csv",sep="/"),header=TRUE)
all.sampled$site.week.year<-paste(all.sampled$site, all.sampled$week_number, all.sampled$year, sep="_")
#all.sampled.sub<-data.frame(site.week.year=all.sampled[,c("site.week.year")])

merge.data<-merge(all.sampled,locationData, by=c("site.week.year"),all.x=TRUE)

#fix columns
merge.data$site.y<-NULL
merge.data$year.y<-NULL
merge.data$latitude.y<-NULL
merge.data$longitude.y<-NULL
merge.data$week_number.y<-NULL

names(merge.data)[names(merge.data)=="site.x"]<-"site"
names(merge.data)[names(merge.data)=="year.x"]<-"year"
names(merge.data)[names(merge.data)=="week_number.x"]<-"week_number"
names(merge.data)[names(merge.data)=="latitude.x"]<-"latitude"
names(merge.data)[names(merge.data)=="longitude.x"]<-"longitude"

#now fill in NAs with min/max dates for each week
#make year.week column
merge.data$year.week<-paste(merge.data$year, merge.data$week_number, sep="_")

YearWeekList<-as.character(unique(merge.data$year.week))

all.data.new<-list()
for(i in 1:length(YearWeekList)){
  
  sub.data<-subset(merge.data, year.week==YearWeekList[i])
  minTime1<-min(sub.data$time1,na.rm=TRUE)
  maxTime2<-max(sub.data$time2,na.rm=TRUE)
  
  sub.data$time1[is.na(sub.data$time1)] <- minTime1
  sub.data$time2[is.na(sub.data$time2)] <- maxTime2
  
  all.data.new<-rbind(all.data.new, sub.data)
}
  
  

dataIn=all.data.new
getWeatherData<-function(dataIn,dir){
  
##read in data and get in usable format
table <- dataIn

table2<-subset(table, select=c('site','latitude','longitude'))
table2<-unique(table2)

#create matrix of lat and lon
veclatlon<-subset(table2, select=c('latitude','longitude'))

#find nearest ICAO station code for each lat lon combination
diva<-NULL
options(geonamesUsername="jsmo")
for (i in 1:nrow(veclatlon)){
  d<-GNfindNearByWeather(lat=veclatlon[i,1],lng=veclatlon[i,2])
  d<-d[c('ICAO','lng','lat','stationName')]
  diva=rbind(diva, data.frame(d))
}

table3<-cbind(table2,diva)
table3$ICAO <- as.character(table3$ICAO)
table4<-merge(table,table3)

table4$time1<-paste(substr(table4$time1,1,4),substr(table4$time1,5,6),substr(table4$time1,7,8),sep="-")
table4$time2<-paste(substr(table4$time2,1,4),substr(table4$time2,5,6),substr(table4$time2,7,8),sep="-")

##find weather data for each ICAO station for the dates needed 
##custom columns = mean temp, mean humidity, wind speed, precip
weather<-list()
for (l in 13988:nrow(table4)){
  w<-try(getSummarizedWeather(station_id=table4[l,13], start_date=table4[l,10],
, end_date=table4[l,11],  opt_custom_columns = TRUE, custom_columns = c(3,9,18,20)))
  
  w$site<-as.character(table4[l,1])
  w$ICAO<-table4[l,13]
  w$year<-table4[l,5]
  w$week_number=table4[l,9]
  
  w.out<-as.data.frame(w)
  
  weather=rbind(weather, w.out)
}

weather.save<-unique(weather)

#write out data to spreadsheet with site and date information
write.csv(weather.save, paste(getwd(),"Data","New_allweather_unique_3.csv",sep="/"),row.names=F)
}


#had to do this cause of internet connection breaking up

weather1<-read.csv(paste(getwd(), "Data", "New_allweather_unique_2.csv",sep="/"),heade=TRUE)
weather2<-read.csv(paste(getwd(), "Data", "New_allweather_unique_3.csv",sep="/"),heade=TRUE)

weather.out<-rbind(weather1, weather2)
weather.out.save<-unique(weather.out)
write.csv(weather.out.save, paste(getwd(),"Data","New_allweather_unique.csv",sep="/"),row.names=F)
