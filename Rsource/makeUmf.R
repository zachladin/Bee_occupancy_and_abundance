#make N-mixture Model unmarkedFrame (Pcount)
makeUmf<-function(dataIn,species){
  require(unmarked)

  data.new<-dataIn

  stateName<-unique(as.character(data.new$state))
  countyList<-sort(unique(as.character(data.new$county)))

  data.new$site_id<-as.factor(as.character(data.new$site_id))

  #data.new$VisitNum<-as.character(data.new$VisitNum)

  #create Year.Site.Week
  data.new$Year.Site.Week<-paste(data.new$year, data.new$site_id, data.new$week_number, sep="_")
  data.new$Year.Site.Week<-as.factor(as.character(data.new$Year.Site.Week))

  #create Year.Site
  data.new$Year.Site<-paste(data.new$year, data.new$site_id, sep="_")
  data.new$Year.Site<-as.factor(as.character(data.new$Year.Site))

  #get detection data and covs with melt and cast
  data.count.melt <- melt(data.new, id=c("site_id","week_number","full_species_name","latitude","longitude",
                                         "state","county","site","Year.Site.Week","Year.Site"),
                                        measure=c("count"), na.rm=FALSE)

  #make week_number a factor
  data.count.melt$week_number<-as.factor(as.character(data.count.melt$week_number))

  #cast count data (one visit at first) add try()
  count.data <- cast(data.count.melt, Year.Site ~ week_number, add.missing=TRUE,fun.aggregate=sum, subset=c(full_species_name==species))
  head(count.data)

  #get nYears, nSites
  # site.covs<-read.table(text=as.character(count.data$Year.Point),colClasses="character",sep=".")
  # count.data.out.1<-cbind(site.covs,count.data)
  # colnames(count.data.out.1)[1:3]<-c("Year","PointID","VisitNum")
  # count.data.out.1$PointID<-as.factor(as.character(count.data.out.1$PointID))
  # count.data.out.1$Year<-as.factor(as.character(count.data.out.1$Year))

  #get some site covariates from new.data
  #head(data.new)
  data.new.covs<-data.new[,c("Year.Site","year","state","county","site_id","site","latitude",
                             "longitude")]

  data.new.covs.unique<-unique(data.new.covs)

  data.out.1<-merge(count.data, data.new.covs.unique, by="Year.Site",all=FALSE)
  count.data.out<-unique(data.out.1)
names(count.data.out)
  #reorder week columns
  count.data.out.2<-count.data.out[,c("Year.Site", "1","2","3","4","5", "6","7","8","9","10","11","12",                
                                              "year","state","county", "site_id","site","latitude","longitude")]
  #number of survey locations
  #nSites<-length(unique(count.data.out$site_id))

  #number of visits
  nWeeks<-length(unique(data.new$week_number))

  #create a visitMat of nSites x nWeeks in dimensions
  visitMat<- matrix(rep(1:nWeeks,each=length(row.names(count.data.out.2))),ncol=nWeeks)
  colnames(visitMat)<-c("wk.1","wk.2","wk.3", "wk.4", "wk.5","wk.6","wk.7","wk.8", "wk.9", "wk.10","wk.11","wk.12")

  count.data.out.3<-cbind(count.data.out.2,visitMat)

  #rename y column headers
  colnames(count.data.out.3)[2:13]<-c("y.1","y.2","y.3", "y.4", "y.5","y.6","y.7","y.8", "y.9", "y.10","y.11","y.12")

  #save data in long format to working directory
  #write.csv(count.data.out,paste(getwd(), "Data",paste(species,stateName,"temp.umf.csv",sep="."),sep="/"),row.names=FALSE)

  #########################################################################################
  # Import data and check structure
  #species.data <- read.csv(paste(getwd(), "Data",paste(species,stateName,"temp.umf.csv",sep="."),sep="/"), na.strings = c("-Inf", "NA","<NA>"),check.names=FALSE)
  species.data<-count.data.out.3
  # species.data$VisitNum<-as.factor(as.character(species.data$VisitNum))
  # species.data$Year<-as.factor(as.character(species.data$Year))
  # head(species.data)

  #########################################################################################

  y<-species.data[,c("y.1","y.2","y.3", "y.4", "y.5","y.6","y.7","y.8", "y.9", "y.10","y.11","y.12")]
  covs<-species.data[,c("year","state","county","site_id","site")]
  covs$county<-as.factor(as.character(covs$county))
  covs$site_id<-as.factor(as.character(covs$site_id))
  
  #add landcover data
  forest.200<-unique(dataIn[,c("site_id","Deciduous_Forest_200","Evergreen_Forest_200", "Mixed_Forest_200")])
  forest.200$forest.merge.200<-scale(forest.200$Deciduous_Forest_200 + forest.200$Evergreen_Forest_200 + forest.200$Mixed_Forest_200)
  
  forest.500<-unique(dataIn[,c("site_id","Deciduous_Forest_500","Evergreen_Forest_500", "Mixed_Forest_500")])
  forest.500$forest.merge.500<-scale(forest.500$Deciduous_Forest_500 + forest.500$Evergreen_Forest_500 + forest.500$Mixed_Forest_500)
  
  forest.1000<-unique(dataIn[,c("site_id","Deciduous_Forest_1000","Evergreen_Forest_1000", "Mixed_Forest_1000")])
  forest.1000$forest.merge.1000<-scale(forest.1000$Deciduous_Forest_1000 + forest.1000$Evergreen_Forest_1000 + forest.1000$Mixed_Forest_1000)
  
  grassland.200<-unique(dataIn[,c("site_id","Pasture_Hay_200","Grassland_Herbaceous_200", "Shrub_Scrub_200")])
  grassland.200$grassland.merge.200<- scale(grassland.200$Pasture_Hay_200 + grassland.200$Grassland_Herbaceous_200 + grassland.200$Shrub_Scrub_200)
  
  
  grassland.500<-unique(dataIn[,c("site_id","Pasture_Hay_500","Grassland_Herbaceous_500", "Shrub_Scrub_500")])
  grassland.500$grassland.merge.500<- scale(grassland.500$Pasture_Hay_500 + grassland.500$Grassland_Herbaceous_500 + grassland.500$Shrub_Scrub_500)
  
  grassland.1000<-unique(dataIn[,c("site_id","Pasture_Hay_1000","Grassland_Herbaceous_1000", "Shrub_Scrub_1000")])
  grassland.1000$grassland.merge.1000<- scale(grassland.1000$Pasture_Hay_1000 + grassland.1000$Grassland_Herbaceous_1000 + grassland.1000$Shrub_Scrub_1000)
  
  developed.200<-unique(dataIn[,c("site_id","Developed_Open_Space_200","Developed_Low_Intensity_200", "Developed_Medium_Intensity_200", "Developed_High_Intensity_200")])
  developed.200$developed.merge.200<-scale(developed.200$Developed_Open_Space_200+ developed.200$Developed_Low_Intensity_200+developed.200$Developed_Medium_Intensity_200+developed.200$Developed_High_Intensity_200)
  
  developed.500<-unique(dataIn[,c("site_id","Developed_Open_Space_500","Developed_Low_Intensity_500", "Developed_Medium_Intensity_500", "Developed_High_Intensity_500")])
  developed.500$developed.merge.500<-scale(developed.500$Developed_Open_Space_500+ developed.500$Developed_Low_Intensity_500+developed.500$Developed_Medium_Intensity_500+developed.500$Developed_High_Intensity_500)
  
  developed.1000<-unique(dataIn[,c("site_id","Developed_Open_Space_1000","Developed_Low_Intensity_1000", "Developed_Medium_Intensity_1000", "Developed_High_Intensity_1000")])
  developed.1000$developed.merge.1000<-scale(developed.1000$Developed_Open_Space_1000+ developed.1000$Developed_Low_Intensity_1000+developed.1000$Developed_Medium_Intensity_1000+developed.1000$Developed_High_Intensity_1000)
  
  land.cover.site.covs<-cbind(forest.200, forest.500[,-1], forest.1000[,-1], grassland.200[,-1], grassland.500[,-1], grassland.1000[,-1], developed.200[,-1], developed.500[,-1], developed.1000[,-1])
  
  #merge land.cover.site.covs with unmarked frame
  covs.merge<-merge(covs, land.cover.site.covs, by="site_id",all.x=TRUE)
  
  obs.covs<-list(Week=species.data[,c("wk.1","wk.2","wk.3", "wk.4", "wk.5","wk.6","wk.7","wk.8", "wk.9", "wk.10","wk.11","wk.12")])
  #head(covs)


  #now bundle everything into unmarkedFrame
  umf <- unmarkedFramePCount(y=y,
                          siteCovs=covs.merge,
                          obsCovs=obs.covs)

  return(umf)
}

#End