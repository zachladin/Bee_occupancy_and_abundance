#Preliminary data analyses Grace Bees

#clear out Environment
rm(list=ls())

#set working directory
setwd("/Users/Zach/Dropbox (ZachTeam)/Projects/Grace_Bees")

#you'll have to set this to the filepath on your machine
#setwd("your file path here")

##################################################################
#make a results folder to save figs in
dir.create(paste(getwd(),"Results",sep="/"))

##################################################################
#if missing packages use
#install.packages("ggplot2","reshape","unmarked")

#load packages
library(ggplot2)
library(reshape)
library(unmarked)
library(maptools)
library(raster)
library(ggmap)
library(rgdal)
library(plyr)
library(doParallel)
library(dclone)
library(foreach)
library(data.table)
library(raster)
library(ggalt)
library(ggsn)
library(rasterVis)
library(sp)
library(rgeos)
library(fields)
library(ggfortify)

showConnections(all = TRUE)

#load source functions (saved in Rsource folder)
source(paste(getwd(),"Rsource","formatBeeData.R",sep="/"))
source(paste(getwd(),"Rsource","summaryFunction.R",sep="/"))
source(paste(getwd(),"Rsource","makeUmfPcount.R",sep="/"))
source(paste(getwd(),"Rsource","makeUmfOccu.R",sep="/"))
source(paste(getwd(),"Rsource","runUnmarked.R",sep="/"))
source(paste(getwd(),"Rsource","getModelOutput.R",sep="/"))
source(paste(getwd(),"Rsource","capwords.R",sep="/"))


##################################################################
#read in data
data2014<-read.csv(paste(getwd(),"Data","2014_beedata_final.csv",sep="/"), header=TRUE,strip.white=TRUE)

#read in 2015
data2015<-read.csv(paste(getwd(),"Data","2015_beedata_final.csv",sep="/"), header=TRUE,strip.white=TRUE)

#combine using rbind
data<-rbind(data2014, data2015)



# data$site_id<-NULL
# data$site.week<-NULL
# data$site_week_species<-NULL
# data<-unique(data)
##################################################################
#format data and save as new file
formatBeeData(dataIn=data)

##################################################################
#read in formatted data
data<-read.csv(paste(getwd(),"Data","Bee.formatted.count.data.csv",sep="/"),header=TRUE)
head(data)

##################################################################
#get speciesList
speciesList<-unique(as.character(data$full_species_name))
length(speciesList)#158

species=speciesList[1]
##################################################################
#make simple map of site locations

#create bounding box bbox
bbox.delmar<-c(left=-80, bottom=37,right=-74,top=40.5)

#define map area and type
map.delmar <- get_map(location = bbox.delmar, maptype="terrain")
ggmap(map.delmar)

#get county boundaries for study area
counties <- map_data("county")
delmar_county <- subset(counties, c(region == 'virginia' | region=='maryland' | region=='delaware' | region=='district of columbia'))

delmar_county$region<-capwords(delmar_county$region)
delmar_county$subregion<-capwords(delmar_county$subregion)

#covert column names in delmar_county
colnames(delmar_county)[5]<-"State"
colnames(delmar_county)[6]<-"county"

delmar_county$State<-as.factor(as.character(delmar_county$State))

levels(delmar_county$State)[levels(delmar_county$State)=="District Of Columbia"] <- "DC"

delmar_county[delmar_county=="Prince Georges"]<- "Prince George's"
delmar_county[delmar_county=="Queen Annes"]<- "Queen Anne's"
delmar_county[delmar_county=="St Marys"]<- "Saint Mary's"
delmar_county[delmar_county=="Baltimore City"]<- "Baltimore"

delmar_county$county[delmar_county$State %in% c("DC")] <- "District of Columbia"

#sampled county df
sampled.county.df<-data.frame(State=c("Maryland", "Delaware", "Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Virginia", "Maryland","Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Delaware", "Maryland", "Maryland", "DC","Maryland","Maryland","Maryland", "Virginia"), county=c("Kent", "Kent","Howard","Worcester",            "Harford","Montgomery", "Caroline" ,"Garrett","Saint Mary's" ,"Wicomico","Fairfax", "Baltimore","Cecil","Anne Arundel","Carroll","Frederick","Washington","Queen Anne's","Sussex","Dorchester","Calvert","District of Columbia", "Charles", "Prince George's",      "Talbot","Virginia Beach"),Sampled="Sampled")


#merge sampled with all counties in delmar_county
delmar.merge<-merge(delmar_county, sampled.county.df, by=c("State","county"),all.x=TRUE)
delmar.merge.sort<-delmar.merge[order(delmar.merge$order,decreasing=FALSE),]

#make group a factor
delmar.merge.sort$group<-as.factor(as.character(delmar.merge.sort$group))
delmar.merge.sort$Sampled<-ifelse(is.na(delmar.merge.sort$Sampled),"Not sampled","Sampled")


#delmar.test<-cbind(delmar_county.sort, delmar.merge.sort$Predicted)
delmar.plot<-fortify(delmar.merge.sort)
#colnames(delmar.plot)[7]<-"Predicted"





#test filling in counties (chloropleth map)
length(unique(data$site_id))
# delmar_county$new.values<-rnorm(n=length(delmar_county$subregion),mean=100,sd=25)
# range(delmar_county$new.values)

#make color list for states
myStateColors<-c("seagreen3","goldenrod1","darkorchid1","royalblue2")

#make color list for borders
borderColors<-c("black","darkgray")

delmar.plot$Sampled<-as.factor(as.character(delmar.plot$Sampled))
delmar.plot$Sampled<-factor(delmar.plot$Sampled, levels=rev(levels(delmar.plot$Sampled)))
#levels(delmar.plot$Sampled)

delmar.sampled<-subset(delmar.plot, Sampled=="Sampled")


#scalebar placement 

map.minLon=as.numeric(attr(map.delmar, "bb")[2])
map.maxLon=as.numeric(attr(map.delmar, "bb")[4])
map.minLat=as.numeric(attr(map.delmar, "bb")[1])
map.maxLat=as.numeric(attr(map.delmar, "bb")[3])

scalebar.x<--74
scalebar.y<- 36.3

scalebar.dist<-as.numeric(ifelse(abs(map.maxLon - map.minLon) < 0.03, 0.5,1))
#add figure heading



#make plot
mapSitesBees<-ggmap(map.delmar,darken=c(0.4, "white")) +
  geom_polygon(data = delmar.plot, aes(x=long, y=lat,group=group, color=Sampled, fill=State, alpha=Sampled),lwd=0.25)+
  geom_polygon(data = delmar.sampled, aes(x=long, y=lat,group=group, color=Sampled),fill=NA,lwd=0.25)+
  scale_fill_manual(values=myStateColors,  guide = guide_legend(title="State"))+
  scale_color_manual(values=borderColors, guide=guide_legend(title="Sampled\ncounties"))+
  scale_alpha_discrete(range=c(0.6,0.2))+
  #geom_line(data=new.data,aes(x = Lon, y = Lat, group=BandNum),color="orange",lwd=0.2)+
  geom_point(data=data, aes(x =longitude, y =latitude ), color="firebrick",alpha = 0.2,size=0.5)+
  theme(panel.border=element_rect(color="black",fill=NA))+
  theme(legend.position = c(0.75, 1.01), 
        legend.justification = c(0, 1.2),
        legend.background = element_rect(fill = alpha("white",0.9), color="black"))+
  guides(alpha=FALSE, color=FALSE)+
  #scale_fill_gradientn(colors = c("red","yellow","green","lightblue","darkblue"),values=c(1.0,0.8,0.6,0.4,0.2,0))+
  ggtitle("Hymenoptera sampling locations (n = 119) from 2014-2015")+
  labs(x="Longitude", y="Latitude")+  scalebar(anchor=c(x=scalebar.x, y=scalebar.y), x.min=map.minLon, x.max=map.maxLon, y.min=map.minLat, y.max=map.maxLat, dist=100, dd2km= TRUE, model='WGS84',st.dist=0.02, st.size=3,height=0.015,st.bottom = TRUE)
mapSitesBees

north2(mapSitesBees, x=.79, y=.32, symbol=11)

png(filename = paste(getwd(), "Results","Maps","County Abundance","BeeSamplingLocations.map.png",sep="/"),width = 7, height = 7, units = "in", bg = "white",  res = 150)

#add Species Richness
plot.new()
#text(0.5,0.5,paste("Trends of changes in Salt marsh Elevation (mm)"),family="Times")

try(print(mapSitesBees))
north2(mapSitesBees, x=.79, y=.32, symbol=11)

dev.off()


#create folder within results for saving maps
dir.create(paste(getwd(), "Results","Maps",sep="/"))

#create filepath to save figures
mapFilepath<-paste(getwd(), "Results","Maps",sep="/")

#save figure to folder
ggsave(mapSitesBees, filename="bee.site.locations.2014-15.png", path=mapFilepath,width=7, height=5,limitsize = FALSE)

##################################################################
#make simple table (Table 1)

data$site_id<-NULL
data$site.week<-NULL
data<-unique(data)

#table of locations with lat long and state, county, years sampled
location.data<-unique(data[,c("site","latitude","longitude", "year","state","county")])
colnames(location.data)[1]<-"Site"

table1<-as.data.frame.matrix(table(location.data$Site, location.data$year))
table1$Site<-row.names(table1)
row.names(table1)<-NULL

loc.data.2<-location.data[,c("Site","latitude","longitude","county","state")]
colnames(loc.data.2)<-c("Site","Latitude","Longitude","County","State")
merge1<-merge(table1, loc.data.2, by=c("Site"))
table2<-merge1[order(merge1$State, merge1$County, decreasing=FALSE),]


table.show<-table2

##################################################################
#Fig 3. clipped landcover data
#get study area state  and county boundariers
us <- raster::getData("GADM", country="USA", level=2)
unique(us@data$NAME_1)
states<-subset(us, NAME_1=="Delaware" | NAME_1=="Virginia" | NAME_1=="Maryland" | NAME_1=="District of Columbia")

plot(states)

#countyList<-unique(delmar.sampled$county)

#county.names<-states@data$NAME_2
states.sub<-subset(states, c(states@data$NAME_1=="Delaware" & states@data$NAME_2=="Kent"  | 
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Kent"  | 
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Cecil" | 
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Chester" |
                               states@data$NAME_1=="Delaware" & states@data$NAME_2=="Sussex"  |
                               states@data$NAME_2=="District of Columbia" |
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Anne Arundel" |
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Calvert" |
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Baltimore" |
                               states@data$NAME_1=="Maryland" & states@data$NAME_2== "Caroline" |
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Carroll" |
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Charles" |
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Dorchester" |
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Frederick" |
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Garrett" |
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Harford" |
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Howard" |
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Montgomery" |
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Prince George's" |
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Queen Anne's" |
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Saint Mary's" |
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Talbot" |
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Washington" |
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Wicomico" |
                               states@data$NAME_1=="Maryland" & states@data$NAME_2=="Worcester" |
                               states@data$NAME_1=="Virginia" & states@data$NAME_2=="Fairfax" |
                               states@data$NAME_1=="Virginia" & states@data$NAME_2=="Virginia Beach"))


plot(states.sub)

#get projection
proj4string(states.sub)
bee.projection<-proj4string(states.sub)




#now load NLCD 2011 

# #read in file and make raster
raster1<-raster(paste("/Users/zach/Dropbox (ZachTeam)/Projects/WOTH GPS Tags/DE_land_cover","nlcd_2011_landcover_2011_edition_2014_10_10","nlcd_2011_landcover_2011_edition_2014_10_10.img",sep="/"))
plot(raster1)
# 
projection.use<-raster::projection(raster1)
#
bee.proj<-spTransform(states.sub, projection.use)
bee.projection<-proj4string(bee.proj)
#plot(bee.proj)

bee.extent<-extent(bee.proj)

#crop to extent
lulc.bee.crop<-raster::crop(x=raster1, y=bee.extent, snap="out")
plot(lulc.bee.crop)
states.proj<-spTransform(states, projection.use)
plot(states.proj, border="lightgray",col="transparent", add=TRUE)
plot(bee.proj, col=alpha("blue",0.2),add=TRUE)

unique(raster2.df$value)

#now mask
bee.lulc.mask <- raster::mask(lulc.bee.crop, bee.proj)
plot(bee.lulc.mask)

#save raster as GTiff
writeRaster(lulc.bee.crop, filename=paste(getwd(),"Data","bee.crop.tif",sep="/"), format="GTiff")
writeRaster(bee.lulc.mask, filename=paste(getwd(),"Data","bee.mask.tif",sep="/"), format="GTiff")

########################################################################################################################################################
#read in raster2 (de.mask)
raster2<-raster(paste(getwd(),"Data","bee.mask.tif",sep="/"))
plot(raster2)

#get list of unique land cover classes

#reproject
old.projection<-raster::projection(raster2)
new.projection<-(proj4string(states.sub))

raster2.proj<-projectRaster(from=raster2, crs=new.projection,method="ngb")
plot(raster2.proj)
plot(states.sub,col=alpha("blue",0.3), add=TRUE)

#save raster as GTiff
writeRaster(raster2.proj, filename=paste(getwd(),"Data","bee.mask.proj.tif",sep="/"), format="GTiff")

#####
#reproject full extent of area (for clipping buffers later)
raster3.proj<-projectRaster(from=lulc.bee.crop, crs=new.projection,method="ngb")
projection(raster3.proj)
plot(raster3.proj)

#save raster as GTiff
writeRaster(raster3.proj, filename=paste(getwd(),"Data","all.proj.tif",sep="/"), format="GTiff")




#########################################################################################################
#read in raster2 (start here for DE raster layer)
raster2.proj<-raster(paste("/Users/zach/Dropbox (ZachTeam)/Projects/Grace_Bees","Data","bee.mask.proj.tif",sep="/"))
################################################

#look at unique ID values
vals<-unique(getValues(raster2.proj))

#convert raster to a data.bee
raster2.pts <- rasterToPoints(raster2.proj)
raster2.df <- data.frame(raster2.pts)

#reset colnames
colnames(raster2.df)[3]<-"value"

#get nlcd class types
uniqueID<-sort(unique(raster2.df$value))
#length(uniqueID)
#read in NCLD_2011_legend
NLCD.legend<-read.csv(paste(getwd(),"Data", "NLCD_2011_Legend.csv",sep="/"))

#merge with unique ID


#############
#This works!!

colTab <- attr(raster2, "legend")@colortable
#set NA values (0) to same color as background (i.e., white)
colTab<-gsub("#000000","#FFFFFF",x=colTab)
names(colTab) <- 0:(length(colTab) - 1)
valTab <- sort(unique(raster2.df[[3]]))

mycolors<-colTab[as.character(valTab)]
#############


landcoverList<-c("Open Water","Developed Open Space","Developed Low Intensity","Developed Medium Intensity","Developed High Intensity","Barren Land","Deciduous Forest","Evergreen Forest","Mixed Forest", "Scrub/Shrub","Grassland/Herbaceous","Pasture/Hay","Cultivated Crops","Woody Wetlands","Emergent Herbaceous Wetlands")

#ratify raster
rat.raster2.proj<-ratify(raster2.proj)
rat <- levels(rat.raster2.proj)[[1]]
rat$code <- c(valTab)
rat<-rat[-1,]
rat$landcover <- landcoverList

levels(rat.raster2.proj) <- rat

myColors <- mycolors
myKey <- list(rectangles=list(col = myColors),
              text=list(lab=rat$landcover),
              space='right',
              columns=1,
              title="2011 NLCD\nLand Cover Class",
              cex.title=1)

print(levelplot(rat.raster2.proj, col.regions = myColors, colorkey = FALSE, key = myKey) +
        layer(sp.polygons(states.sub, col="black")))


#################################################################################################
#get landcover class proportions at each site for different buffer sizes.

data<-read.csv(paste(getwd(),"Data","Bee.formatted.count.data.csv",sep="/"),header=TRUE)

data.keep<-unique(data[,c("site","longitude","latitude")])

data.keep.1<-unique(data[,c("site","state","county")])

data.keep.merge<-join(data.keep.1, data.keep, by="site", type="left",match="first")
data.keep.merge.sub<-data.keep.merge[,c("site","latitude","longitude")]

# #add NA sites
# NAsites<-read.csv(paste(getwd(), "Data","NAs_bees.csv",sep="/"),header=TRUE)
# NAsites.sub<-NAsites[,c("site","latitude","longitude","state","county")]
# #replace _ with " " in site names
# NAsites.sub$site<-gsub("_"," ",NAsites.sub$site)
# 
# #add to data.keep.merge
# data.keep.merge.all<-rbind(data.keep.merge.sub, NAsites.sub)
data.keep.merge.all<-data.keep.merge

coordinates(data.keep.merge.all) = ~longitude+latitude # formula

data(data.keep.merge.all)
data.points<-data.keep.merge.all

class(data.keep.merge.all)




raster3.proj<-raster(paste("/Users/zach/Dropbox (ZachTeam)/Projects/Grace_Bees","Data","all.proj.tif",sep="/"))

str(raster3.proj)
plot(raster3.proj)
raster::projection(raster3.proj)
raster::projection(data.keep.merge.all)
raster::projection(data.keep.merge.all)<- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"


proj4string(data.keep.merge.all)<-raster::projection(raster3.proj)
data.proj<-spTransform(data.keep.merge.all, CRS(raster::projection(raster3.proj)))

# extract_buffered200 <- raster::extract(raster2.proj, data.proj, buffer=200)
# extract_buffered500 <- raster::extract(raster2.proj, data.proj, buffer=500)
# extract_buffered1000 <- raster::extract(raster2.proj, data.proj, buffer=1000)

bufferList<-c(200, 500, 1000)

#read in NCLD_2011_legend
NLCD.legend<-read.csv(paste(getwd(),"Data", "NLCD_2011_Legend.csv",sep="/"), header=TRUE)
NLCD.df<-data.frame(ID=NLCD.legend[2], Class=NLCD.legend[3], Broad.Class=NLCD.legend[1])

#loop to gather all land cover proporition data
all.out<-list()
for(j in 1:length(bufferList)){
  
  buffer.num<-as.integer(bufferList[j])
  temp.buffered<-raster::extract(raster3.proj, data.proj, buffer=buffer.num)
  
  prop.out<-list()
  
  for(i in 1:length(temp.buffered)){
    print(i)
  site.buffer<-temp.buffered[i]
  table1<-prop.table(table(site.buffer))
  
  new.df<-data.frame(ID=table1)
  colnames(new.df)<-c("ID","Proportion")
  
  merge1<-merge(NLCD.df,new.df,all=TRUE)
  merge1$Proportion[is.na(merge1$Proportion)]<-0
  
  i.num<-as.integer(i)
  merge1$site<-as.character(data.proj@data$site[i.num])
  merge1$county<-as.character(data.proj@data$county[i.num])
  merge1$state<-as.character(data.proj@data$state[i.num])
  merge1$buffer<-buffer.num
  
  prop.out<-rbind(prop.out, merge1)
}

all.out<-rbind(all.out, prop.out)
}


#save as .csv
write.csv(all.out, file=paste(getwd(), "Data","R_extracted_NLCD_data.csv",sep="/"),row.names=FALSE)



###################################################################################################
#Figure 4. Summary of means by site
#now explore the data
#get site-level covariates
site.covs<-unique(read.csv(paste(getwd(),"Data","Bee.data.site.covs.csv",sep="/"),header=TRUE))



#first test on 1 species

#subset out most commomn species Andrena erigeniae
sp1<-subset(data, full_species_name=="Andrena erigeniae")
head(sp1)

##################################################################
#use summary function to look at overall means per site
summary.sp1<-summaryFunction(dataIn=sp1, factor="site", response="count")

#merge site.covs on summary.sp1
sp1.summary.merge<-merge(summary.sp1, site.covs, by=c("site"),all.y=TRUE)

#replace all NAs in means with 0s
sp1.summary.merge$mean[is.na(sp1.summary.merge$mean)] <- 0

##################################################################
#sort highest to lowest by site

sp1.summary.sort<-sp1.summary.merge[order(sp1.summary.merge$mean,decreasing=TRUE),]

site.levels<-sp1.summary.sort$site_id
sp1.summary.sort$order <- factor(sp1.summary.sort$site_id, levels = site.levels)

#make site_id a factor
sp1.summary.sort$site_id<-as.factor(as.character(sp1.summary.sort$site_id))
sp1.summary.sort$site<-as.character(sp1.summary.sort$site)

#create folder to save tables
dir.create(paste(getwd(), "Results","Tables",sep="/"))

#create folder to save tables of Overall mean counts
dir.create(paste(getwd(), "Results","Tables","Overall_Mean_Counts",sep="/"))

#make filepath to save tables
tableFilepath<-paste(getwd(), "Results", "Tables", "Overall_Mean_Counts",sep="/")


#get speciesName
speciesName<-unique(sp1$full_species_name)

#save table to .csv file
write.csv(sp1.summary.sort, file=paste(tableFilepath, paste(speciesName,"overall.mean.counts.csv",sep="" ),sep="/"),row.names=FALSE)

#get siteList
siteList<-list(as.character(sp1.summary.sort$site))

##################################################################

#make latin name italics
#plot mean counts per site
mean.count<-ggplot(data=sp1.summary.sort, aes(x=site_id, y=mean, ymin=mean-SE, ymax=mean+SE,group=state))+
  coord_flip()+
  #stat_smooth(alpha=0.7,method="lm",color="white")+
  geom_errorbar(width=0, size=0.75,aes(color=state))+
  #geom_line(color=I("grey20"))+
  geom_bar(stat="identity",aes(fill=state),alpha=0.75)+
  #geom_point(stat="identity",size=3, aes(color=state))+
  ggtitle(paste(speciesName, " mean counts in 2014-15",sep=""))+
#theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), panel.border=element_blank())+
  #theme(panel.background=element_rect(fill='white'))+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5, size=12, color="black"),
        axis.text.y = element_text(size=10, color="black"),
        axis.title.x = element_text(size=14, hjust=0.5, vjust=0.2),
        axis.title.y = element_text(angle = 90, vjust=1.2, size=14))+
  theme(panel.border=element_rect(color="black",fill=NA))+
  #theme(legend.position="none")+
  scale_x_discrete(limits=rev(levels(sp1.summary.sort$order)),labels=rev(siteList))+
  scale_colour_hue(guide = "none")+
  labs(x="Site", y="Overall mean count",fill="State")
mean.count

#create filepath and new folder
meanCountFilepath<-paste(getwd(),"Results","Mean_count_by_site",sep="/")
dir.create(meanCountFilepath)

#save plot of mean counts
ggsave(mean.count, filename=paste(speciesName,"mean.count.2014.pdf",sep="."),path=meanCountFilepath, width=7,height=11, limitsize=FALSE)
ggsave(mean.count, filename=paste(speciesName,"mean.count.2014.png",sep="."),path=meanCountFilepath, width=7,height=11, limitsize=FALSE)

#################################################################
##################################################################
#Figure 4.
#now create for loop to iterate through all species for Overall mean counts

#make speciesList
#speciesList<-unique(as.character(data$full_species_name))
speciesList<-c("Andrena perplexa", "Andrena barbara", "Andrena erigeniae","Andrena violae", "Andrena cornelli","Bombus impatiens","Lasioglossum coeruleum","Halictus confusus")

#for loop to get mean counts of all pooled data for each species at each site.
for(i in 1:length(speciesList)){
  print(i)
  #step 1 subset data
  new.data<-subset(data, full_species_name==speciesList[i])

  #get speciesName
  speciesName<-unique(new.data$full_species_name)

  #get summary
  summary.new<-summaryFunction(dataIn=new.data, factor="site", response="count")

  #merge site.covs on summary.new
  new.summary.merge<-merge(summary.new, site.covs, by=c("site"),all.y=TRUE)

  #replace all NAs in means with 0s
  new.summary.merge$mean[is.na(new.summary.merge$mean)] <- 0
  ##################################################################
  #sort highest to lowest by site
  new.summary.sort<-new.summary.merge[order(new.summary.merge$mean,decreasing=TRUE),]
  site.levels<-new.summary.sort$site
  new.summary.sort$order <- factor(new.summary.sort$site, levels = site.levels)

  #make site_id a factor
  new.summary.sort$site_id<-as.factor(as.character(new.summary.sort$site))
  #make site a character string
  new.summary.sort$site<-as.character(new.summary.sort$site)

  #create folder to save tables
  dir.create(paste(getwd(), "Results","Tables",sep="/"))

  #create folder to save tables of Overall mean counts
  dir.create(paste(getwd(), "Results","Tables","Overall_Mean_Counts",sep="/"))

  #make filepath to save tables
  tableFilepath<-paste(getwd(), "Results", "Tables", "Overall_Mean_Counts",sep="/")

  #save table to .csv file
  write.csv(new.summary.sort, file=paste(tableFilepath, paste(speciesName,"overall.mean.counts.csv",sep="." ),sep="/"),row.names=FALSE)

  #get siteList
  siteList<-as.character(new.summary.sort$site)

  ##################################################################
  #plot mean counts per site
  mean.count<-ggplot(data=new.summary.sort, aes(x=site_id, y=mean, ymin=mean-SE, ymax=mean+SE,group=state))+
    coord_flip()+
    #stat_smooth(alpha=0.7,method="lm",color="white")+
    geom_errorbar(width=0, size=0.75,aes(color=state))+
    #geom_line(color=I("grey20"))+
    geom_bar(stat="identity",aes(fill=state),alpha=0.75)+
    #geom_point(stat="identity",size=3, aes(color=state))+
    ggtitle(paste(speciesName," mean counts in 2014-2015",sep=""))+
    #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), panel.border=element_blank())+
    #theme(panel.background=element_rect(fill='white'))+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5, size=12, color="black"),
          axis.text.y = element_text(size=7, color="black"),
          axis.title.x = element_text(size=14, hjust=0.5, vjust=0.2),
          axis.title.y = element_text(angle = 90, vjust=1.2, size=14))+
    theme(panel.border=element_rect(color="black",fill=NA))+
    #theme(legend.position="none")+
    scale_x_discrete(limits=rev(levels(new.summary.sort$order)),labels=rev(siteList))+
    scale_colour_hue(guide = "none")+
    labs(x="Site", y="Overall mean count",fill="State")
  print(mean.count)

  #create filepath and new folder
  meanCountFilepath<-paste(getwd(),"Results","Mean_count_by_site",sep="/")
  dir.create(meanCountFilepath)

  #save plot of mean counts
  #ggsave(mean.count, filename=paste(speciesName,"mean.count.pdf",sep="."),path=meanCountFilepath, width=8.5,height=11, limitsize=FALSE)
  # ggsave(mean.count, filename=paste(speciesName,"mean.count.png",sep="."),path=meanCountFilepath, width=8.5,height=11, limitsize=FALSE)

}

##################################################################
#figure out what weeks were sampled per site
sample.table<-as.data.frame.matrix(table(data$site, data$week_number))
sample.table$site<-row.names(sample.table)
row.names(sample.table)<-NULL



##################################################################

#get mean per week per county

#for loop to iterate through species

##################################################################
#Figure 5.
#get mean per week per county and fit both linear and non-linear modes to data

speciesList<-c("Andrena perplexa", "Andrena barbara", "Andrena erigeniae","Andrena violae", "Andrena cornelli","Bombus impatiens","Lasioglossum coeruleum","Halictus confusus")


#create week data.frame to merge with sites where not all weeks detected individuals
weekDF<-data.frame(week_number=c(seq(1,12),seq(1,12)))

yearDF<-data.frame(year=c(rep(2014,12),rep(2015,12)))
week.yearDF<-cbind(weekDF, yearDF)
week.yearDF$week.year<-paste(week.yearDF$week_number, week.yearDF$year, sep="_")

for(i in 1:length(speciesList)){

  #step 1 subset data
    #subset out a species
    new.data<-subset(data, full_species_name==speciesList[i])

    #make week.year column 
    new.data$week.year<-paste(new.data$week_number,new.data$year, sep="_")
    #get summary
    new.week.summary<-summaryFunction(dataIn=new.data, factor="week.year",response="count")

    #separate week and year
    separate<-read.table(text=as.character(new.week.summary$week.year),colClasses = "character",sep="_")
    colnames(separate)<-c("week_number","year")
    
    #now add back to dataframe
    new.week.summary.2<-cbind(separate, new.week.summary)
    
    #now merge with weekDF
    new.week.summary.merge<-merge(new.week.summary.2, week.yearDF,by=c("week.year"), all.y=TRUE)

    #replace all NAs in means with 0s
    new.week.summary.merge$mean[is.na(new.week.summary.merge$mean)] <- 0

    #fix columns
    new.week.summary.merge$week_number.x<-NULL
    new.week.summary.merge$year.x<-NULL
    colnames(new.week.summary.merge)[10]<-"week_number"
    colnames(new.week.summary.merge)[11]<-"year"
    
    #reorder by week
    new.week.summary.merge$week_number<-as.integer(as.character(new.week.summary.merge$week_number))

    #sort highest to lowest by site
    new.week.summary.sort<-new.week.summary.merge[order(new.week.summary.merge$week_number,decreasing=FALSE),]
    week.levels<-new.week.summary.sort$week_number
    new.week.summary.sort$order <- factor(new.week.summary.sort$week_number, levels = week.levels)

    #make week_number a factor
    new.week.summary.sort$week_number<-as.factor(as.character(new.week.summary.sort$week_number))

    #make year a factor
    new.week.summary.sort$year<-as.factor(as.character(new.week.summary.sort$year))
   #put weeks in correct order
   #new.week.summary.table<-new.week.summary.merge[order(new.week.summary.merge$Week,decreasing=FALSE),]
mycolors<-c("royalblue3","goldenrod2")
    
  #  #create plot (linear model)
  #  week.mean<-ggplot(data=new.week.summary.sort, aes(x=week_number, y=mean, ymin=mean-SE, ymax=mean+SE, group=year))+
  #    #coord_flip()+
  #    stat_smooth(alpha=0.3,method="lm",aes(color=year))+
  #    geom_errorbar(width=0, size=0.75, aes(color=year))+
  #    geom_line(aes(color=year))+
  #    #geom_bar(stat="identity",aes(fill=state))
  #    geom_point(stat="identity",size=3, aes(color=year))+
  #    scale_color_manual(values=mycolors,guide = guide_legend(title = "Year"))+
  #    ggtitle(paste(speciesList[i]," mean count per week ",sep=""))+
  #    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), panel.border=element_blank())+
  #    theme(panel.background=element_rect(fill='white'))+
  #    theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5, size=10, color="black"),
  #          axis.text.y = element_text(size=10, color="black"),
  #          axis.title.x = element_text(size=15, hjust=0.5, vjust=0.2),
  #          axis.title.y = element_text(angle = 90, vjust=1.2, size=15))+
  #    theme(panel.border=element_rect(color="black",fill=NA))+
  #    theme(legend.position = c(0.75, 0.95), 
  #          legend.justification = c(0, 1.2),
  #          legend.background = element_rect(fill = NA, color="black")
  #          )+
  #    #theme(axis.ticks.x=element_blank(), axis.text.x = element_blank())+
  #    #     scale_fill_brewer(palette="Dark2")+
  #    #     scale_color_brewer(palette="Dark2")+
  #    scale_x_discrete(limits=levels(new.week.summary.sort$order))+
  #    labs(x="Week", y="Mean count")
  #  #scale_x_discrete()
  #  print(week.mean)
  # 
  #  #print(week.mean)
  # 
  # #now save plot
  #  #create filepath and new folder
  #  meanCountWeekFilepath<-paste(getwd(),"Results","Mean_count_by_week",sep="/")
  #  dir.create(meanCountWeekFilepath)
  # 

   # try(ggsave(week.mean, filename=paste(speciesList[i],"mean.count.per.week.png",sep="."),path=meanCountWeekFilepath, width=7.5,height=5, limitsize=FALSE))

   
   
     week.mean<-ggplot(data=new.week.summary.sort, aes(x=week_number, y=mean, ymin=mean-SE, ymax=mean+SE, group=year))+
     #coord_flip()+
       stat_smooth(alpha=0.3,method="lm",formula = y ~ x + I(x^2),aes(color=year))+
       geom_errorbar(width=0, size=0.75, aes(color=year))+
     geom_line(aes(color=year))+
     #geom_bar(stat="identity",aes(fill=state))
     geom_point(stat="identity",size=3, aes(color=year))+
     scale_color_manual(values=mycolors,guide = guide_legend(title = "Year"))+
     ggtitle(paste(speciesList[i]," mean count per week ",sep=""))+
     theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), panel.border=element_blank())+
     theme(panel.background=element_rect(fill='white'))+
     theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5, size=10, color="black"),
           axis.text.y = element_text(size=10, color="black"),
           axis.title.x = element_text(size=15, hjust=0.5, vjust=0.2),
           axis.title.y = element_text(angle = 90, vjust=1.2, size=15))+
     theme(panel.border=element_rect(color="black",fill=NA))+
     theme(legend.position = c(0.75, 0.95), 
           legend.justification = c(0, 1.2),
           legend.background = element_rect(fill = NA, color="black")
     )+
     #theme(axis.ticks.x=element_blank(), axis.text.x = element_blank())+
     #     scale_fill_brewer(palette="Dark2")+
     #     scale_color_brewer(palette="Dark2")+
     scale_x_discrete(limits=levels(new.week.summary.sort$order))+
     labs(x="Week", y="Mean count")
   #scale_x_discrete()
   print(week.mean)
   
  
   #create plot (non-linear model)

   #print(week.mean)

   #now save plot
   #create filepath and new folder
   # meanCountWeekFilepath_nonLinear<-paste(getwd(),"Results","Mean_count_by_week_nonLinear",sep="/")
   # dir.create(meanCountWeekFilepath_nonLinear)
   # 
   # 
   # try(ggsave(week.mean, filename=paste(speciesList[i],"mean.count.per.week.nonLinear.png",sep="."),path=meanCountWeekFilepath_nonLinear, width=7.5,height=5, limitsize=FALSE))

}

##############################################################################################
##############################################################################################
##############################################################################################
#now make umf to run abundance model

#get list of states
stateList<-unique(as.character(data$state))

#subset out data by state
md.data<-subset(data, state==stateList[1])

#get speciesList
speciesList<-unique(as.character(md.data$full_species_name))

###############################################################################################
#for loop to get abundance estiamtes by county, and make a chloropleth map
all.results<-list()
for(i in 1:length(speciesList)){
  
speciesName<-speciesList[i]
print(speciesName)
#make unmarkedFrame for Pcount
umf<-makeUmfPcount(dataIn=data, species=speciesName)

#run model and get county-level estimates
mod.1<-NULL
mod.1<-tryCatch({pcount(~week ~ county, data=umf, mixture="P")
  },error=function(cond2){
    cond2=NULL
    cond2
  })

#sampled county df
sampled.county.df<-data.frame(state=c("Maryland", "Delaware", "Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Virginia", "Maryland","Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Delaware", "Maryland", "Maryland", "DC","Maryland","Maryland","Maryland", "Virginia"), county=c("Kent", "Kent","Howard","Worcester","Harford","Montgomery", "Caroline" ,"Garrett","Saint Mary's" ,"Wicomico","Fairfax", "Baltimore","Cecil","Anne Arundel","Carroll","Frederick","Washington","Queen Anne's","Sussex","Dorchester","Calvert","District of Columbia", "Charles", "Prince George's","Talbot","Virginia Beach"))

#get preidcted estimates
abun.out<-NULL
abun.out<-tryCatch({predict(mod.1, type="state", appendData=TRUE)
},error=function(cond2){
  cond2=data.frame(sampled.county.df,Predicted=0,SE=NA,lower=NA, upper=NA)
  cond2
})

abun.out.1<-abun.out[,c("state","county","Predicted","SE","lower","upper")]
abun.out.2<-unique(abun.out.1)

abun.out.2$county<-as.character(abun.out.2$county)

#rename Kent county
abun.out.2[abun.out.2=="Kent.DE"]<-"Kent"
abun.out.2[abun.out.2=="Kent.MD"]<-"Kent"
abun.out.2[abun.out.2=="Prince Georges"]<- "Prince George's"
abun.out.2[abun.out.2=="Queen Annes"]<- "Queen Anne's"
abun.out.2[abun.out.2=="Saint Marys"]<- "Saint Mary's"
abun.out.2[abun.out.2=="Baltimore City"]<- "Baltimore"

#rename columns
colnames(abun.out.2)<-c("State","county","Predicted","SE","lower","upper")
#################################################################################
#make a quick chloropleth map of abundance estimates by county

#create bounding box bbox
bbox.delmar<-c(left=-79.7, bottom=36.8,right=-73.8,top=40.2)

#define map area and type
map.delmar <- get_map(location = bbox.delmar, maptype="terrain")
ggmap(map.delmar)

#get county boundaries for study area
counties <- map_data("county")
delmar_county <- subset(counties, c(region == 'virginia' | region=='maryland' | region=='delaware' | region=='district of columbia'))

delmar_county$region<-capwords(delmar_county$region)
delmar_county$subregion<-capwords(delmar_county$subregion)

#covert column names in delmar_county
colnames(delmar_county)[5]<-"State"
colnames(delmar_county)[6]<-"county"

delmar_county$State<-as.factor(as.character(delmar_county$State))

levels(delmar_county$State)[levels(delmar_county$State)=="District Of Columbia"] <- "DC"

delmar_county[delmar_county=="Prince Georges"]<- "Prince George's"
delmar_county[delmar_county=="Queen Annes"]<- "Queen Anne's"
delmar_county[delmar_county=="St Marys"]<- "Saint Mary's"
delmar_county[delmar_county=="Baltimore City"]<- "Baltimore"

delmar_county$county[delmar_county$State %in% c("DC")] <- "District of Columbia"



#All state and county data.frame
all.county.df<-unique(delmar_county[,c("State","county")])

#merge with abun.output
abun.merge<-merge(all.county.df, abun.out.2, by=c("State","county"),all.x=TRUE)


#delmar_county.sub<-delmar_county[delmar_county$county %in% abun.out.2$county,]
delmar.merge<-merge(delmar_county, abun.merge, by=c("State","county"),all.x=TRUE)
delmar.merge.sort<-delmar.merge[order(delmar.merge$order,decreasing=FALSE),]

delmar_county.sort<-delmar_county[order(delmar_county$order,decreasing=FALSE),]



delmar.test<-cbind(delmar_county.sort, delmar.merge.sort$Predicted)
delmar.plot<-fortify(delmar.test)
colnames(delmar.plot)[7]<-"Predicted"

#make group a factor
delmar.plot$group<-as.factor(as.character(delmar.plot$group))
delmar.plot$Sampled<-ifelse(is.na(delmar.plot$Predicted),"Not sampled","Sampled")

delmar.sampled<-delmar.plot[!is.na(delmar.plot$Predicted),]



#sampled county df
sampled.county.df<-data.frame(state=c("Maryland", "Delaware", "Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Virginia", "Maryland","Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Delaware", "Maryland", "Maryland", "DC","Maryland","Maryland","Maryland", "Virginia"), county=c("Kent", "Kent","Howard","Worcester",            "Harford","Montgomery", "Caroline" ,"Garrett","Saint Mary's" ,"Wicomico","Fairfax", "Baltimore","Cecil","Anne Arundel","Carroll","Frederick","Washington","Queen Anne's","Sussex","Dorchester","Calvert","District of Columbia", "Charles", "Prince George's",      "Talbot","Virginia Beach"))

#test filling in counties (chloropleth map)
length(unique(data$site_id))
# delmar_county$new.values<-rnorm(n=length(delmar_county$subregion),mean=100,sd=25)
# range(delmar_county$new.values)

#make color list for states
myStateColors<-c("seagreen3","goldenrod1","darkorchid1","royalblue2")

#make color list for borders
borderColors<-c("black","darkgray")

delmar.plot$Sampled<-as.factor(as.character(delmar.plot$Sampled))
delmar.plot$Sampled<-factor(delmar.plot$Sampled, levels=rev(levels(delmar.plot$Sampled)))
levels(delmar.plot$Sampled)

#scalebar placement 

map.minLon=as.numeric(attr(map.delmar, "bb")[2])
map.maxLon=as.numeric(attr(map.delmar, "bb")[4])
map.minLat=as.numeric(attr(map.delmar, "bb")[1])
map.maxLat=as.numeric(attr(map.delmar, "bb")[3])

scalebar.x<--74
scalebar.y<- 36.3

scalebar.dist<-as.numeric(ifelse(abs(map.maxLon - map.minLon) < 0.03, 0.5,1))
#add figure heading



#start as NULL each iteration through loop
mapChloro<-NULL
#make plot
mapChloro<-ggmap(map.delmar,darken=c(0.4, "white")) +
  geom_polygon(data = delmar.plot, aes(x=long, y=lat,group=group, color=Sampled, fill=Predicted, alpha=Sampled),lwd=0.25)+
  geom_polygon(data = delmar.sampled, aes(x=long, y=lat,group=group, color=Sampled),fill=NA,lwd=0.25)+
  scale_fill_gradientn(colors = c("red","yellow","seagreen","royalblue","violet","gray"),values=c(1.0,0.8,0.6,0.4,0.2,0), guide = guide_colorbar(title="Predicted\nabundance"),limits=c(0,80), na.value="white")+
  scale_color_manual(values=borderColors, guide=guide_legend(title="Sampled\ncounties"))+
  scale_alpha_discrete(range=c(0.6,0.3))+
  #geom_line(data=new.data,aes(x = Lon, y = Lat, group=BandNum),color="orange",lwd=0.2)+
  geom_point(data=data, aes(x =longitude, y =latitude ), color="firebrick",alpha = 0.2,size=0.5)+
  theme(panel.border=element_rect(color="black",fill=NA))+
  theme(legend.position = c(0.75, 1.01), 
        legend.justification = c(0, 1.2),
        legend.background = element_rect(fill = alpha("white",0.9), color="black"))+
  guides(alpha=FALSE, color=FALSE)+
  #scale_fill_gradientn(colors = c("red","yellow","green","lightblue","darkblue"),values=c(1.0,0.8,0.6,0.4,0.2,0))+
  ggtitle(paste(speciesName, " mean predicted abundance by county",sep=""))+
  labs(x="Longitude", y="Latitude")+
  scalebar(anchor=c(x=scalebar.x, y=scalebar.y), x.min=map.minLon, x.max=map.maxLon, y.min=map.minLat, y.max=map.maxLat, dist=100, dd2km= TRUE, model='WGS84',st.dist=0.02, st.size=3,height=0.015,st.bottom = TRUE)
mapChloro

north2(mapChloro, x=.79, y=.32, symbol=11)

png(filename = paste(getwd(), "Results","Maps","County Abundance",paste(speciesName,"_county.abundance.map.png",sep=""),sep="/"),width = 7, height = 7, units = "in", bg = "white",  res = 150)

#add Species Richness
plot.new()
#text(0.5,0.5,paste("Trends of changes in Salt marsh Elevation (mm)"),family="Times")

try(print(mapChloro))
north2(mapChloro, x=.79, y=.32, symbol=11)

dev.off()



# #create folder within results for saving maps
# dir.create(paste(getwd(), "Results","Maps","County Abundance",sep="/"))
# 
# #create filepath to save figures
# mapFilepath<-paste(getwd(), "Results","Maps","County Abundance",sep="/")
# 
# #save figure to folder
# try(ggsave(mapChloro, filename=paste(speciesName,"_county.abundance.map.png",sep=""), path=mapFilepath,width=7, height=8,limitsize = FALSE))


abun.out.2$Species<-speciesName

all.results<-rbind(all.results, abun.out.2)

write.csv(all.results, file=paste(getwd(), "Results","County_abundance_results.csv",sep="/"),row.names=FALSE)

}

#################################################################################
#now set up model selection for each species - occupancy

#get speciesList
speciesList<-unique(as.character(data$full_species_name))

speciesName<-speciesList[]

AIC.out.occu<-NULL
for(i in 1:length(speciesList)){
  
  speciesName=speciesList[i]
  #speciesName="Bombus impatiens"
  print(speciesName)
  
  #make unmarkedFrame for Pcount
  umf<-makeUmfOccu(dataIn=data, species=speciesName)
  
  #convert numbers to zeros and anything greater than 1 to 1



#first model selection for detection prob

# #build models
# (null <- occu(~1 ~1, data=umf))
# (global <- occu(~week+temp+precip+wind+humidity ~1, data=umf))
# (week <- occu(~week ~1, data=umf))
# (precip <- occu(~precip ~1, data=umf))
# (temp <- occu(~temp ~1, data=umf))
# (wind<- occu(~wind ~1, data=umf))
# (humidity <- occu(~humidity ~1, data=umf))
# (week.precip<-occu(~week+precip ~1, data=umf))
# (week.temp<-occu(~week+temp ~1, data=umf))
# (week.wind<-occu(~week+wind ~1, data=umf))
# (week.humidity<-occu(~week+humidity ~1, data=umf))
  

#fit models for detection 
# fit_det <- fitList(
#   "null" = null,
#   "global" = global,
#   "week" = week,
#   "precip " = precip,
#   "temp" = temp,
#   "wind" = wind,
#   "humidity" = humidity,
#   "week.precip"=week.precip,
#   "week.temp"=week.temp,
#   "week.wind"=week.wind,
#   "week.humidity"=week.humidity)
# 
# # Rank them by AIC
# (fit_ms_det <- modSel(fit_det))
  


#build models
  null<-NULL
  try(null <- occu(~week+temp ~1, data=umf))
  global<-NULL
  try(global <- occu(~week+temp ~  year + sex + elevation_m + forest_200 + forest_500 + forest_1000 + grassland_200 + grassland_500 + grassland_1000 + developed_200 + developed_500+developed_1000 +wetland_200+wetland_500+wetland_1000, data=umf))
  year<-NULL
  try(year<-occu(~week+temp ~year, data=umf))
  sex<-NULL
  try(sex<-occu(~week+temp ~sex, data=umf))
  elevation<-NULL
  try(elevation<-occu(~week+temp ~ elevation_m, data=umf))
  # county<-NULL
  # try(county<-occu(~week ~county, data=umf))
  #(year.county<-occu(~week ~ year + county, data=umf))
  # forest<-NULL
  # try(forest<-occu(~week+temp ~forest_200 + forest_500 + forest_1000,data=umf))
  # 
  # forest.year<-NULL
  # try(forest.year<-occu(~week+temp ~forest_200 + forest_500 + forest_1000 + year, data=umf))
  #(forest.county<-occu(~week ~forest + county, data=umf))
  #(forest.year.county<-occu(~week ~forest + year + county, data=umf))
  
  forest200<-NULL
  try(forest200<-occu(~week+temp ~forest_200,data=umf))
  # forest200.year<-NULL
  # try(forest200.year<-occu(~week+temp ~forest_200 + year,data=umf))
  #(forest200.county<-occu(~week ~forest200 + county, data=umf))
  #(forest200.year.county<-occu(~week ~forest_200 + year + county, data=umf))
  forest500<-NULL
  try(forest500<-occu(~week+temp ~forest_500,data=umf))
  # forest500.year<-NULL
  # try(forest500.year<-occu(~week+temp ~forest_500 + year,data=umf))
  #(forest500.county<-occu(~week ~forest500 + county, data=umf))
  #(forest500.year.county<-occu(~week ~forest_500 + year + county, data=umf))
  forest1000<-NULL
  try(forest1000<-occu(~week+temp ~forest_1000,data=umf))
  # forest1000.year<-NULL
  # try(forest1000.year<-occu(~week+temp ~forest_1000 + year,data=umf))
  #(forest1000.county<-occu(~week ~forest1000 + county, data=umf))
  #(forest1000.year.county<-occu(~week ~forest_1000 + year + county, data=umf))
  # grassland<-NULL
  # try(grassland<-occu(~week+temp ~grassland_200 + grassland_500 + grassland_1000,data=umf))
  # grassland.year<-NULL
  # try(grassland.year<-occu(~week+temp ~grassland_200 + grassland_500 + grassland_1000 + year, data=umf))
  #(grassland.county<-occu(~week ~grassland + county, data=umf))
  #(grassland.year.county<-occu(~week ~grassland + year + county, data=umf))
  grassland200<-NULL
  try(grassland200<-occu(~week+temp ~grassland_200,data=umf))
  # grassland200.year<-NULL
  # try(grassland200.year<-occu(~week+temp ~grassland_200 + year,data=umf))
  #(grassland200.county<-occu(~week ~grassland200 + county, data=umf))
  #(grassland200.year.county<-occu(~week ~grassland_200 + year + county, data=umf))
  grassland500<-NULL
  try(grassland500<-occu(~week+temp ~grassland_500,data=umf))
  # grassland500.year<-NULL
  # try(grassland500.year<-occu(~week+temp ~grassland_500 + year,data=umf))
  #(grassland500.county<-occu(~week ~grassland500 + county, data=umf))
  #(grassland500.year.county<-occu(~week ~grassland_500 + year + county, data=umf))
  grassland1000<-NULL
  try(grassland1000<-occu(~week+temp ~grassland_1000,data=umf))
  # grassland1000.year<-NULL
  # try(grassland1000.year<-occu(~week+temp ~grassland_1000 + year,data=umf))
  #(grassland1000.county<-occu(~week ~grassland1000 + county, data=umf))
  #(grassland1000.year.county<-occu(~week ~grassland_1000 + year + county, data=umf))
  # developed<-NULL
  # try(developed<-occu(~week+temp ~developed_200 + developed_500 + developed_1000,data=umf))
  # developed.year<-NULL
  # try(developed.year<-occu(~week+temp~developed_200 + developed_500 + developed_1000 + year, data=umf))
  #(developed.county<-occu(~week ~developed + county, data=umf))
  #(developed.year.county<-occu(~week ~developed + year + county, data=umf))
  developed200<-NULL
  try(developed200<-occu(~week+temp ~developed_200,data=umf))
  # devleoped200.year<-NULL
  # try(developed200.year<-occu(~week+temp ~developed_200 + year,data=umf))
  #(developed200.county<-occu(~week ~developed200 + county, data=umf))
  #(developed200.year.county<-occu(~week ~developed_200 + year + county, data=umf))
  developed500<-NULL
  try(developed500<-occu(~week+temp ~developed_500,data=umf))
  # developed500.year<-NULL
  # try(developed500.year<-occu(~week+temp ~developed_500 + year,data=umf))
  #(developed500.county<-occu(~week ~developed500 + county, data=umf))
  #(developed500.year.county<-occu(~week ~developed_500 + year + county, data=umf))
  developed1000<-NULL
  try(developed1000<-occu(~week+temp ~developed_1000,data=umf))
  # developed1000.year<-NULL 
  # try(developed1000.year<-occu(~week+temp ~developed_1000 + year,data=umf))
  #(developed1000.county<-occu(~week ~developed1000 + county, data=umf))
  #(developed1000.year.county<-occu(~week ~developed_1000 + year + county, data=umf))
  wetland200<-NULL
  try(wetland200<-occu(~week+temp ~wetland_200,data=umf))
  # wetland200.year<-NULL
  # try(wetland200.year<-occu(~week+temp ~wetland_200 + year,data=umf))
  #(wetland200.county<-occu(~week ~wetland200 + county, data=umf))
  #(wetland200.year.county<-occu(~week ~wetland_200 + year + county, data=umf))
  wetland500<-NULL
  try(wetland500<-occu(~week+temp ~wetland_500,data=umf))
  # wetland500.year<-NULL
  # try(wetland500.year<-occu(~week+temp ~wetland_500 + year,data=umf))
  #(wetland500.county<-occu(~week ~wetland500 + county, data=umf))
  #(wetland500.year.county<-occu(~week ~wetland_500 + year + county, data=umf))
  wetland1000<-NULL
  try(wetland1000<-occu(~week+temp ~wetland_1000,data=umf))
  # wetland1000.year<-NULL 
  # try(wetland1000.year<-occu(~week+temp ~wetland_1000 + year,data=umf))
  #(wetland1000.county<-occu(~week ~wetland1000 + county, data=umf))
  #(wetland1000.year.county<-occu(~week ~wetland_1000 + year + county, data=umf))
  

fit_occu<-NULL
#fit models for abundance
fit_occu <- tryCatch({fitList(
  "null" = null,
  "global" = global,
  "year" = year,
  "sex" = sex,
  "elevation" = elevation,
  #"county" = county,
  #"year.county" = year.county,
  # "forest" = forest,
  # "forest.year" = forest.year,
  # "forest.county" = forest.county,
  # "forest.year.county" = forest.year.county,
  "forest200" = forest200,
  #"forest200.year" = forest200.year,
  # "forest200.county" = forest200.county,
  # "forest200.year.county" = forest200.year.county,
  "forest500" = forest500,
  #"forest500.year" = forest500.year,
  # "forest500.county" = forest500.county,
  # "forest500.year.county" = forest500.year.county,
  "forest1000" = forest1000,
  #"forest1000.year" = forest1000.year,
  # "forest1000.county" = forest1000.county,
  # "forest1000.year.county" = forest1000.year.county,
  # "grassland" = grassland,
  # "grassland.year" = grassland.year,
  # "grassland.county" = grassland.county,
  # "grassland.year.county" = grassland.year.county,
  "grassland200" = grassland200,
  #"grassland200.year" = grassland200.year,
  # "grassland200.county" = grassland200.county,
  # "grassland200.year.county" = grassland200.year.county,
  "grassland500" = grassland500,
  #"grassland500.year" = grassland500.year,
  # "grassland500.county" = grassland500.county,
  # "grassland500.year.county" = grassland500.year.county,
  "grassland1000" = grassland1000,
  #"grassland1000.year" = grassland1000.year,
  # "grassland1000.county" = grassland1000.county,
  # "grassland1000.year.county" = grassland1000.year.county,
  # "developed" = developed,
  # "developed.year" = developed.year,
  # "developed.county" = developed.county,
  # "developed.year.county" = developed.year.county,
  "developed200" = developed200,
  #"developed200.year" = developed200.year,
  # "developed200.county" = developed200.county,
  # "developed200.year.county" = developed200.year.county,
  "developed500" = developed500,
  #"developed500.year" = developed500.year,
  # "developed500.county" = developed500.county,
  # "developed500.year.county" = developed500.year.county,
  "developed1000" = developed1000,
  #"developed1000.year" = developed1000.year,
  # "developed1000.county" = developed1000.county,
  # "developed1000.year.county" = developed1000.year.county,
  # "wetland" = wetland,
  # "wetland.year" = wetland.year,
  # "wetland.county" = wetland.county,
  # "wetland.year.county" = wetland.year.county,
  "wetland200" = wetland200,
  #"wetland200.year" = wetland200.year,
  # "wetland200.county" = wetland200.county,
  # "wetland200.year.county" = wetland200.year.county,
  "wetland500" = wetland500,
  #"wetland500.year" = wetland500.year,
  # "wetland500.county" = wetland500.county,
  # "wetland500.year.county" = wetland500.year.county,
  "wetland1000" = wetland1000)
  #"wetland1000.year" = wetland1000.year
# "wetland1000.county" = wetland1000.county,
# "wetland1000.year.county" = wetland1000.year.county)

},error=function(cond2){
  cond2=NULL
  cond2
})


  # Rank them by AIC
fit_ms_occu<-NULL
try(fit_ms_occu <- modSel(fit_occu))

#harvest AIC output
AIC.output.occu<-tryCatch({as.data.frame(fit_ms_occu@Full)
},error=function(cond2){
  cond2=data.frame(model=NA, formula=NA,nPars=NA, AIC=NA,delta=NA,AICwt=NA, cumltvWt=NA)
  cond2
})

AIC.output.occu.2<-AIC.output.occu[,c("model","formula","nPars","AIC","delta","AICwt","cumltvWt")]
AIC.output.occu.2$Species<-speciesName


AIC.out.occu<-rbind(AIC.out.occu, AIC.output.occu.2)

}

#dir.create(paste(getwd(),"Results","Occupancy",sep="/"))

write.csv(AIC.out.occu, file=paste(getwd(),"Results","Occupancy","AIC_occu_results.csv",sep="/"),row.names=FALSE)


#################################################################################
#now set up model selection for each species - abundance

speciesName<-speciesList[1]

AIC.out.abun<-NULL
for(i in 1:length(speciesList)){
  
  speciesName=speciesList[i]
  #speciesName="Bombus impatiens"
  print(speciesName)
  
  #make unmarkedFrame for Pcount
  umf<-makeUmfPcount(dataIn=data, species=speciesName)
  
  
  #first model selection for detection prob
  
  # #build models
  # (null <- pcount(~1 ~1, data=umf))
  # (global <- pcount(~week+temp.scale+precip.scale+wind.scale+humidity.scale ~1, data=umf))
  # (week <- pcount(~week ~1, data=umf))
  # (precip <- pcount(~precip.scale ~1, data=umf))
  # (temp <- pcount(~temp.scale ~1, data=umf))
  # (wind<- pcount(~wind.scale ~1, data=umf))
  # (humidity <- pcount(~humidity.scale ~1, data=umf))
  # (week.precip<-pcount(~week+precip.scale ~1, data=umf))
  # (week.temp<-pcount(~week+temp.scale ~1, data=umf))
  # (week.wind<-pcount(~week+wind.scale ~1, data=umf))
  # (week.humidity<-pcount(~week+humidity.scale ~1, data=umf))
  # 
  # 
  # #fit models for detection 
  # fit_det_abun <- fitList(
  #   "null" = null,
  #   "global" = global,
  #   "week" = week,
  #   "precip " = precip,
  #   "temp" = temp,
  #   "wind" = wind,
  #   "humidity" = humidity,
  #   "week.precip"=week.precip,
  #   "week.temp"=week.temp,
  #   "week.wind"=week.wind,
  #   "week.humidity"=week.humidity)
  # 
  # # Rank them by AIC
  # (fit_ms_det_abun <- modSel(fit_det_abun))
  

  
  #build models
  null<-NULL
  try(null <- pcount(~week+temp ~1, data=umf))
  global<-NULL
  try(global <- pcount(~week+temp.scale ~  year + sex + elevation_m + forest_200 + forest_500 + forest_1000 + grassland_200 + grassland_500 + grassland_1000 + developed_200 + developed_500+developed_1000+wetland_200+wetland_500+wetland_1000, data=umf))
  year<-NULL
  try(year<-pcount(~week+temp ~year, data=umf))
  sex<-NULL
  try(sex<-pcount(~week+temp ~sex, data=umf))
  elevation<-NULL
  try(elevation<-pcount(~week+temp ~elevation_m, data=umf))
  # county<-NULL
  # try(county<-pcount(~week ~county, data=umf))
  #(year.county<-pcount(~week ~ year + county, data=umf))
  # forest<-NULL
  # try(forest<-pcount(~week+temp ~forest_200 + forest_500 + forest_1000,data=umf))
  # 
  # forest.year<-NULL
  # try(forest.year<-pcount(~week+temp ~forest_200 + forest_500 + forest_1000 + year, data=umf))
  #(forest.county<-pcount(~week ~forest + county, data=umf))
  #(forest.year.county<-pcount(~week ~forest + year + county, data=umf))
  
  forest200<-NULL
  try(forest200<-pcount(~week+temp ~forest_200,data=umf))
  # forest200.year<-NULL
  # try(forest200.year<-pcount(~week+temp ~forest_200 + year,data=umf))
  #(forest200.county<-pcount(~week ~forest200 + county, data=umf))
  #(forest200.year.county<-pcount(~week ~forest_200 + year + county, data=umf))
  forest500<-NULL
  try(forest500<-pcount(~week+temp ~forest_500,data=umf))
  # forest500.year<-NULL
  # try(forest500.year<-pcount(~week+temp ~forest_500 + year,data=umf))
  #(forest500.county<-pcount(~week ~forest500 + county, data=umf))
  #(forest500.year.county<-pcount(~week ~forest_500 + year + county, data=umf))
  forest1000<-NULL
  try(forest1000<-pcount(~week+temp ~forest_1000,data=umf))
  # forest1000.year<-NULL
  # try(forest1000.year<-pcount(~week+temp ~forest_1000 + year,data=umf))
  #(forest1000.county<-pcount(~week ~forest1000 + county, data=umf))
  #(forest1000.year.county<-pcount(~week ~forest_1000 + year + county, data=umf))
  # grassland<-NULL
  # try(grassland<-pcount(~week+temp ~grassland_200 + grassland_500 + grassland_1000,data=umf))
  # grassland.year<-NULL
  # try(grassland.year<-pcount(~week+temp ~grassland_200 + grassland_500 + grassland_1000 + year, data=umf))
  #(grassland.county<-pcount(~week ~grassland + county, data=umf))
  #(grassland.year.county<-pcount(~week ~grassland + year + county, data=umf))
  grassland200<-NULL
  try(grassland200<-pcount(~week+temp ~grassland_200,data=umf))
  # grassland200.year<-NULL
  # try(grassland200.year<-pcount(~week+temp ~grassland_200 + year,data=umf))
  #(grassland200.county<-pcount(~week ~grassland200 + county, data=umf))
  #(grassland200.year.county<-pcount(~week ~grassland_200 + year + county, data=umf))
  grassland500<-NULL
  try(grassland500<-pcount(~week+temp ~grassland_500,data=umf))
  # grassland500.year<-NULL
  # try(grassland500.year<-pcount(~week+temp ~grassland_500 + year,data=umf))
  #(grassland500.county<-pcount(~week ~grassland500 + county, data=umf))
  #(grassland500.year.county<-pcount(~week ~grassland_500 + year + county, data=umf))
  grassland1000<-NULL
  try(grassland1000<-pcount(~week+temp ~grassland_1000,data=umf))
  # grassland1000.year<-NULL
  # try(grassland1000.year<-pcount(~week+temp ~grassland_1000 + year,data=umf))
  #(grassland1000.county<-pcount(~week ~grassland1000 + county, data=umf))
  #(grassland1000.year.county<-pcount(~week ~grassland_1000 + year + county, data=umf))
  # developed<-NULL
  # try(developed<-pcount(~week+temp ~developed_200 + developed_500 + developed_1000,data=umf))
  # developed.year<-NULL
  # try(developed.year<-pcount(~week+temp~developed_200 + developed_500 + developed_1000 + year, data=umf))
  #(developed.county<-pcount(~week ~developed + county, data=umf))
  #(developed.year.county<-pcount(~week ~developed + year + county, data=umf))
  developed200<-NULL
  try(developed200<-pcount(~week+temp ~developed_200,data=umf))
  # devleoped200.year<-NULL
  # try(developed200.year<-pcount(~week+temp ~developed_200 + year,data=umf))
  #(developed200.county<-pcount(~week ~developed200 + county, data=umf))
  #(developed200.year.county<-pcount(~week ~developed_200 + year + county, data=umf))
  developed500<-NULL
  try(developed500<-pcount(~week+temp ~developed_500,data=umf))
  # developed500.year<-NULL
  # try(developed500.year<-pcount(~week+temp ~developed_500 + year,data=umf))
  #(developed500.county<-pcount(~week ~developed500 + county, data=umf))
  #(developed500.year.county<-pcount(~week ~developed_500 + year + county, data=umf))
  developed1000<-NULL
  try(developed1000<-pcount(~week+temp ~developed_1000,data=umf))
  # developed1000.year<-NULL 
  # try(developed1000.year<-pcount(~week+temp ~developed_1000 + year,data=umf))
  #(developed1000.county<-pcount(~week ~developed1000 + county, data=umf))
  #(developed1000.year.county<-pcount(~week ~developed_1000 + year + county, data=umf))
  wetland200<-NULL
  try(wetland200<-pcount(~week+temp ~wetland_200,data=umf))
  # wetland200.year<-NULL
  # try(wetland200.year<-pcount(~week+temp ~wetland_200 + year,data=umf))
  #(wetland200.county<-pcount(~week ~wetland200 + county, data=umf))
  #(wetland200.year.county<-pcount(~week ~wetland_200 + year + county, data=umf))
  wetland500<-NULL
  try(wetland500<-pcount(~week+temp ~wetland_500,data=umf))
  # wetland500.year<-NULL
  # try(wetland500.year<-pcount(~week+temp ~wetland_500 + year,data=umf))
  #(wetland500.county<-pcount(~week ~wetland500 + county, data=umf))
  #(wetland500.year.county<-pcount(~week ~wetland_500 + year + county, data=umf))
  wetland1000<-NULL
  try(wetland1000<-pcount(~week+temp ~wetland_1000,data=umf))
  # wetland1000.year<-NULL 
  # try(wetland1000.year<-pcount(~week+temp ~wetland_1000 + year,data=umf))
  #(wetland1000.county<-pcount(~week ~wetland1000 + county, data=umf))
  #(wetland1000.year.county<-pcount(~week ~wetland_1000 + year + county, data=umf))
  
  
  fit_abun<-NULL
  #fit models for abundance
  fit_abun <- tryCatch({fitList(
    "null" = null,
    "global" = global,
    "year" = year,
    "sex" = sex,
    "elevation" = elevation,
    #"county" = county,
    #"year.county" = year.county,
    # "forest" = forest,
    # "forest.year" = forest.year,
    # "forest.county" = forest.county,
    # "forest.year.county" = forest.year.county,
    "forest200" = forest200,
    # "forest200.year" = forest200.year,
    # "forest200.county" = forest200.county,
    # "forest200.year.county" = forest200.year.county,
    "forest500" = forest500,
    # "forest500.year" = forest500.year,
    # "forest500.county" = forest500.county,
    # "forest500.year.county" = forest500.year.county,
    "forest1000" = forest1000,
    # "forest1000.year" = forest1000.year,
    # "forest1000.county" = forest1000.county,
    # "forest1000.year.county" = forest1000.year.county,
    # "grassland" = grassland,
    # "grassland.year" = grassland.year,
    # "grassland.county" = grassland.county,
    # "grassland.year.county" = grassland.year.county,
    "grassland200" = grassland200,
    # "grassland200.year" = grassland200.year,
    # "grassland200.county" = grassland200.county,
    # "grassland200.year.county" = grassland200.year.county,
    "grassland500" = grassland500,
    # "grassland500.year" = grassland500.year,
    # "grassland500.county" = grassland500.county,
    # "grassland500.year.county" = grassland500.year.county,
    "grassland1000" = grassland1000,
    # "grassland1000.year" = grassland1000.year,
    # "grassland1000.county" = grassland1000.county,
    # "grassland1000.year.county" = grassland1000.year.county,
    # "developed" = developed,
    # "developed.year" = developed.year,
    # "developed.county" = developed.county,
    # "developed.year.county" = developed.year.county,
    "developed200" = developed200,
    # "developed200.year" = developed200.year,
    # "developed200.county" = developed200.county,
    # "developed200.year.county" = developed200.year.county,
    "developed500" = developed500,
    # "developed500.year" = developed500.year,
    # "developed500.county" = developed500.county,
    # "developed500.year.county" = developed500.year.county,
    "developed1000" = developed1000,
    # "developed1000.year" = developed1000.year,
    # "developed1000.county" = developed1000.county,
    # "developed1000.year.county" = developed1000.year.county,
    # "wetland" = wetland,
    # "wetland.year" = wetland.year,
    # "wetland.county" = wetland.county,
    # "wetland.year.county" = wetland.year.county,
    "wetland200" = wetland200,
    # "wetland200.year" = wetland200.year,
    # "wetland200.county" = wetland200.county,
    # "wetland200.year.county" = wetland200.year.county,
    "wetland500" = wetland500,
    # "wetland500.year" = wetland500.year,
    # "wetland500.county" = wetland500.county,
    # "wetland500.year.county" = wetland500.year.county,
    "wetland1000" = wetland1000)
    # "wetland1000.year" = wetland1000.year)
  # "wetland1000.county" = wetland1000.county,
  # "wetland1000.year.county" = wetland1000.year.county)
  
  },error=function(cond2){
    cond2=NULL
    cond2
  })
  
  
  # Rank them by AIC
  fit_ms_abun<-NULL
  try(fit_ms_abun <- modSel(fit_abun))
  
  #harvest AIC output
  AIC.output.abun<-tryCatch({as.data.frame(fit_ms_abun@Full)
  },error=function(cond2){
    cond2=data.frame(model=NA, formula=NA,nPars=NA, AIC=NA,delta=NA,AICwt=NA, cumltvWt=NA)
    cond2
  })
  AIC.output.abun.2<-AIC.output.abun[,c("model","formula","nPars","AIC","delta","AICwt","cumltvWt")]
  AIC.output.abun.2$Species<-speciesName
  
  
  AIC.out.abun<-rbind(AIC.out.abun, AIC.output.abun.2)
  
  
}

dir.create(paste(getwd(),"Results","Abundance",sep="/"))

write.csv(AIC.out.abun, file=paste(getwd(),"Results","Abundance","AIC_abun_results.csv",sep="/"),row.names=FALSE)

##################################################################################################################fi
#summarize AIC results

#AIC occupancy summary

AIC_occu<-read.csv(paste(getwd(),"Results","Occupancy","AIC_occu_results.csv",sep="/"),header=TRUE)
#remove all models with delta AIC <= 2
AIC_occu.sub<-AIC_occu[AIC_occu$delta<=2,]

#sort by model
AIC_occu.sub.sort<-AIC_occu.sub[order(AIC_occu.sub$model),]
levels(AIC_occu.sub$model)
AIC_occu.sub$model<-factor(AIC_occu.sub$model,levels=rev(c("null","global","year","sex","elevation","forest200","forest500", "forest1000","grassland200", "grassland500", "grassland1000", "wetland200", "wetland500","wetland1000", "developed200", "developed500",  "developed1000")))             
                

#get frequency of species per model
model.spp.occu<-table(AIC_occu.sub$model)
spp.count.occu<-data.frame(model.spp.occu)
colnames(spp.count.occu)<-c("Model","Species_count")


#plot results
plotAICoccu<-ggplot(data=spp.count.occu, aes(x=Model, y=Species_count))+
  coord_flip()+
  geom_bar(stat="identity", fill="royalblue2",alpha=0.8)
plotAICoccu

################################################################
#AIC abundance summary

AIC_abun<-read.csv(paste(getwd(),"Results","Abundance","AIC_abun_results.csv",sep="/"),header=TRUE)
#remove all models with delta AIC <= 2
AIC_abun.sub<-AIC_abun[AIC_abun$delta<=2,]

#sort by model
AIC_abun.sub.sort<-AIC_abun.sub[order(AIC_abun.sub$model),]
levels(AIC_abun.sub$model)
AIC_abun.sub$model<-factor(AIC_abun.sub$model,levels=rev(c("null","global","year","sex","elevation","forest200","forest500", "forest1000","grassland200", "grassland500", "grassland1000", "wetland200", "wetland500","wetland1000", "developed200", "developed500",  "developed1000")))             


#get frequency of species per model
model.spp.abun<-table(AIC_abun.sub$model)
spp.count.abun<-data.frame(model.spp.abun)
colnames(spp.count.abun)<-c("Model","Species_count")


#plot results
plotAICabun<-ggplot(data=spp.count.abun, aes(x=Model, y=Species_count))+
  coord_flip()+
  geom_bar(stat="identity", fill="seagreen4",alpha=0.8)
plotAICabun

##################################################################################################################fi
#example single species anlaysis (Occupancy)

#read in AIC results
AIC_occu<-read.csv(paste(getwd(),"Results","Occupancy","AIC_occu_results.csv",sep="/"),header=TRUE)

speciesName<-speciesList[1]
#first look at AIC table

speciesAIC<-subset(AIC_occu, Species==speciesName)

#print AIC table
print(speciesAIC)

#fit top model(s)

#make umf
umf<-makeUmfOccu(dataIn=data, species=speciesName)

#fit model
mod.1<-occu(~week + temp ~ elevation_m, data=umf)
#look at model results
mod.1

#get predicted values
predict.occu.elev<-predict(mod.1, type="state",appendData=TRUE)

#now get predicted values for newdata

#plot with ggplot
plot1<-ggplot(data=predict.occu.elev, aes(x=elevation_m, y=Predicted))+
  #geom_point(color="royalblue",size=2)+
  geom_line(data=predict.occu.elev, aes(x=elevation_m, y=Predicted),color="royalblue",size=1.2)+
  geom_line(data=predict.occu.elev, aes(x=elevation_m, y=upper),color="gray",size=1.2)+
  geom_line(data=predict.occu.elev, aes(x=elevation_m, y=lower),color="gray",size=1.2)+
  labs(x="Elevation (m)", y="Probability of occurrence")+
  theme(panel.border=element_rect(color="black",fill=NA),
        panel.background = element_rect(fill="white"))+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5, size=10, color="black"),
        axis.text.y = element_text(size=10, color="black"),
        axis.title.x = element_text(size=15, hjust=0.5, vjust=0.2),
        axis.title.y = element_text(angle = 90, vjust=1.2, size=15))

plot1

#plot map of probabilities from elevation data

#download altitude data for USA
raster::getData('alt', country = "USA",level=1)

#read in elevation data
elevation.all<-raster(paste(getwd(),"Data","Elevation_data","USA1_msk_alt.grd",sep="/"))

#get county boundaries for study area
#get study area state  and county boundariers
us <- raster::getData("GADM", country="USA", level=2)
unique(us@data$NAME_1)
states<-subset(us, NAME_1=="Delaware" | NAME_1=="Virginia" | NAME_1=="Maryland" | NAME_1=="District of Columbia")
plot(states)


# Crop elevation data by extent of state subset
elevation.sub <- crop(elevation.all, extent(states))

#maks elevation all by states
elevation.sub.mask<-mask(elevation.sub, states)

plot(elevation.sub.mask)
plot(states, add=TRUE)

#make elevation raster into raster stack
elev.stack<-stack(elevation.sub.mask)
names(elev.stack) <- c("elevation_m")
#now get predicted probabilities of occupancy and map

# Get predictions of detection for each 1km2 quadrat of Switzerland
coef(mod.1)

#get model coefficients
beta <- coef(mod.1, type="state")

logit.psi <- beta[1] + beta[2]*elevation.sub.mask

psi <- exp(logit.psi) / (1 + exp(logit.psi))

#make color ramp
colfunc<-colorRampPalette(rev(c("red","yellow","springgreen","royalblue")))
plot(psi, col=colfunc(100), breaks=seq(0,1, length.out=10))
plot(psi, col=rainbow(start=0, end=1, n=100), breaks=seq(0,1,length.out=100) )

plot(psi, col=rev( rainbow( 99, start=0,end=1 ) ), 
      breaks=seq(min(minValue( psi )),max(maxValue(psi)),length.out=100) ) 


plot(psi, col=rev(( rainbow( 99, start=0,end=0.8 ))) ,legend=FALSE )
r.range <- c(0,max(maxValue(psi)))
plot(psi, legend.only=TRUE, col=rev( rainbow( 99, start=0,end=0.8 ) ),
     legend.width=1, legend.shrink=0.75,
     axis.args=list(at=seq(r.range[1], r.range[2], 0.1),
                    labels=seq(r.range[1], r.range[2], 0.1), 
                    cex.axis=0.6),
     legend.args=list(text='Probability of occurrence', side=4, font=2, line=2.5, cex=0.8))
plot(states, border="black",lwd=0.5,add=TRUE)

#fit model
mod.2<-occu(~week + temp ~ grassland_1000, data=umf)
#look at model results
mod.2

#get predicted values for grassland_1000
predict.occu.grassland<-predict(mod.2, type="state",appendData=TRUE)

#now get predicted values for newdata

#plot with ggplot
plot2<-ggplot(data=predict.occu.grassland, aes(x=grassland_1000, y=Predicted))+
  #geom_point(color="royalblue",size=2)+
  geom_line(data=predict.occu.grassland, aes(x=grassland_1000, y=Predicted),color="royalblue",size=1.2)+
  geom_line(data=predict.occu.grassland, aes(x=grassland_1000, y=upper),color="gray",size=1.2)+
  geom_line(data=predict.occu.grassland, aes(x=grassland_1000, y=lower),color="gray",size=1.2)+
  labs(x="Proportion of grassland land cover within 1000 m buffer", y="Probability of occurrence")+
  theme(panel.border=element_rect(color="black",fill=NA),
        panel.background = element_rect(fill="white"))+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5, size=10, color="black"),
        axis.text.y = element_text(size=10, color="black"),
        axis.title.x = element_text(size=15, hjust=0.5, vjust=0.2),
        axis.title.y = element_text(angle = 90, vjust=1.2, size=15))
  
plot2



########################################################################################################
#single species abundance models

#read in AIC results
AICresults<-read.csv(paste(getwd(),"Results","Abundance","AIC_abun_results.csv",sep="/"),header=TRUE)

speciesName<-speciesList[3]

sp.AIC<-subset(AICresults, Species==speciesName)

#get top model


#make unmarkedFrame for Pcount
umf<-makeUmfPcount(dataIn=data, species=speciesName)

#fit top model
modelName<-as.character(sp.AIC$model[1])
modelName

mod.1<-pcount(~Week ~ grassland.merge.500, data=umf, mixture="P")

predict.abun<-predict(mod.1, type="state",appendData=TRUE)

#now get predicted values for newdata
library(fields)
library(ggfortify)

#plot with ggplot
plot1<-ggplot(data=predict.abun, aes(x=grassland.merge.500, y=Predicted))+
  #geom_point(color="royalblue",size=2)+
  geom_line(data=predict.abun,aes(x=grassland.merge.500, y=Predicted),color="royalblue",size=1.2)+
  geom_line(data=predict.abun,aes(x=grassland.merge.500, y=upper),color="gray",size=1.2)+
  geom_line(data=predict.abun,aes(x=grassland.merge.500, y=lower),color="gray",size=1.2)
  
plot1


#now plot with x-axis on origial scale

#get original developed.merge.200
grassland.merge.500.orig<-umf@siteCovs$Pasture_Hay_500 + umf@siteCovs$Grassland_Herbaceous_500 + umf@siteCovs$Shrub_Scrub_500
range(grassland.merge.500.orig)

developed.merge.200.scaled<-scale(developed.merge.200.orig)

developed.merge.200.unscaled<-ggfortify::unscale(developed.merge.200.scaled)

predict.abun$grassland.merge.500.orig<-grassland.merge.500.orig

plot2<-ggplot(data=predict.abun, aes(x=grassland.merge.500.orig, y=Predicted))+
  #geom_point(color="royalblue",size=2)+
  geom_line(aes(x=grassland.merge.500.orig, y=Predicted),color="royalblue",size=1.2)+
  geom_line(aes(x=grassland.merge.500.orig, y=upper),color="gray",size=1.2)+
  geom_line(aes(x=grassland.merge.500.orig, y=lower),color="gray",size=1.2)
plot2

xticks <- -1:2
xlabs <- xticks*woody.sd + woody.mean
axis(1, at=xticks, labels=round(xlabs, 1))




# Expected abundance over range of proprotion of developed land cover
newData2 <- data.frame(tree_core_mean=seq(-1.5,1.8, by=.1))
E.psi <- predict(tree_core, type="state", newdata=newData2, appendData=TRUE)
head(E.psi)

# Plot predictions with 95% CI
par(mar = rep(2, 4))
plot(Predicted ~ tree_core_mean, E.psi, type="l", ylim=c(0,10),
     xlab="Tree Core Mean (standardized)",
     ylab="Expected occupancy probability")
lines(lower ~ tree_core_mean, E.psi, type="l", col=gray(0.5))
lines(upper ~ tree_core_mean, E.psi, type="l", col=gray(0.5))


# Plot it again, but this time convert the x-axis back to original scale
plot(Predicted ~ tree_core_mean, E.psi, type="l", ylim=c(0,10),
     xlab="Mean Tree Core Age",
     ylab="Expected Abundance",
     xaxt="n")
xticks <- -1.5:1.5
xlabs <- xticks*mean_tree_core_age.sd + mean_tree_core_age.mean
axis(1, at=xticks, labels=round(xlabs, 1))
lines(lower ~ tree_core_mean, E.psi, type="l", col=gray(0.5))
lines(upper ~ tree_core_mean, E.psi, type="l", col=gray(0.5))

####################################################################################################
#Goodness of fit tests

# Function returning three fit-statistics.
fitstats <- function(fm) {
  observed <- getY(fm@data)
  expected <- fitted(fm)
  resids <- residuals(fm)
  sse <- sum(resids^2)
  chisq <- sum((observed - expected)^2 / expected)
  freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
  out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
  return(out)
}

(pb <- parboot(fm4, fitstats, nsim=100, report=1))
### To look at bootstrap distributions do this
###plot(pb, main="")
print(pb)

