#make N-mixture Model unmarkedFrame (Pcount)
makeUmfPcount<-function(dataIn,species){
  require(unmarked)
  
  data.new<-dataIn
  
  stateName<-unique(as.character(data.new$state))
  countyList<-sort(unique(as.character(data.new$county)))
  
  data.new$site<-as.factor(as.character(data.new$site))
  
  #data.new$VisitNum<-as.character(data.new$VisitNum)
  
  #create Year.Site.Week
  data.new$Year.Site.Week<-paste(data.new$year, data.new$site, data.new$week_number, sep="_")
  data.new$Year.Site.Week<-as.factor(as.character(data.new$Year.Site.Week))
  
  #create Year.Site
  data.new$Year.Site<-paste(data.new$year, data.new$site, sep="_")
  data.new$Year.Site<-as.factor(as.character(data.new$Year.Site))
  
  #get detection data and covs with melt and cast
  data.count.melt <- melt(data.new, id=c("site","week_number","full_species_name","latitude","longitude",
                                         "state","county","Year.Site.Week","Year.Site"),
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
  data.new.covs<-data.new[,c("Year.Site","year","state","county","site","latitude",
                             "longitude","forest_200","forest_500","forest_1000","grassland_200","grassland_500","grassland_1000","wetland_200","wetland_500","wetland_1000","developed_200","developed_500","developed_1000","forest_200_scale","forest_500_scale","forest_1000_scale","grassland_200_scale","grassland_500_scale","grassland_1000_scale","wetland_200_scale","wetland_500_scale","wetland_1000_scale","developed_200_scale","developed_500_scale","developed_1000_scale")]
  
  data.new.covs.unique<-unique(data.new.covs)
  
  data.out.1<-merge(count.data, data.new.covs.unique, by="Year.Site",all=FALSE)
  count.data.out<-unique(data.out.1)
  names(count.data.out)
  #reorder week columns
  count.data.out.2<-count.data.out[,c("Year.Site", "1","2","3","4","5", "6","7","8","9","10","11","12",                
                                      "year","state","county","site","latitude","longitude","forest_200","forest_500","forest_1000","grassland_200","grassland_500","grassland_1000","wetland_200","wetland_500","wetland_1000","developed_200","developed_500","developed_1000","forest_200_scale","forest_500_scale","forest_1000_scale","grassland_200_scale","grassland_500_scale","grassland_1000_scale","wetland_200_scale","wetland_500_scale","wetland_1000_scale","developed_200_scale","developed_500_scale","developed_1000_scale")]
  
  #number of survey locations
  #nSites<-length(unique(count.data.out$site))
  
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
  
  covs<-species.data[,c("year","state","county","site","year","state","county","site","latitude","longitude","forest_200","forest_500","forest_1000","grassland_200","grassland_500","grassland_1000","wetland_200","wetland_500","wetland_1000","developed_200","developed_500","developed_1000","forest_200_scale","forest_500_scale","forest_1000_scale","grassland_200_scale","grassland_500_scale","grassland_1000_scale","wetland_200_scale","wetland_500_scale","wetland_1000_scale","developed_200_scale","developed_500_scale","developed_1000_scale")]
  covs$year<-as.factor(as.character(covs$year))
  covs$county<-as.factor(as.character(covs$county))
  covs$site<-as.factor(as.character(covs$site))
  
  covs$site.1<-NULL
  covs$year.1<-NULL
  covs$county.1<-NULL
  covs$state.1<-NULL
  
  #use cast and melt to get obs covs in right format
  #cast count data (one visit at first) add try()
  #get detection data and covs with melt and cast
  
  #get complete list of site.week.years
  
  #all sites
  siteList<-data.frame(site=unique(dataIn$site))
  #all weeks
  weekList<-seq(1,12)
  #all years
  yearList<-c(2014,2015)
  
  all.out<-list()
  for(j in 1: length(yearList)){
    
    year<-yearList[j]
    
    siteRep<-list()
    for(i in 1:length(weekList)){
      new.sites<-data.frame(site=siteList)
      new.sites$week_number<-i
      siteRep=rbind(siteRep,new.sites)
    }
    
    siteRep$year<-year
    
    all.out<-rbind(all.out, siteRep)
  }
  
  obs.sub<-dataIn[,c("site","week_number","year","temp","temp_scale","wind","wind_scale","precipitation","precipitation_scale","humidity","humidity_scale")]
  
  obs.sub$site.week.year<-paste(obs.sub$site, obs.sub$week_number, obs.sub$year,sep="_")
  
  #read in weather data
  # weather.data<-read.csv(paste(getwd(),"Data","New_allweather_unique.csv",sep="/"))
  # weather.data$site<-as.character(weather.data$site)
  
  all.out$site.week.year<-paste(all.out$site, all.out$week_number, all.out$year, sep="_")
  
  #merge obs covs with all.out
  all.obs.covs<-merge(all.out, obs.sub, by="site.week.year",all.x=TRUE)
  all.obs.covs$site.y<-NULL
  all.obs.covs$year.y<-NULL
  all.obs.covs$week_number.y<-NULL
  names(all.obs.covs)[names(all.obs.covs) == 'site.x'] <- 'site'
  names(all.obs.covs)[names(all.obs.covs) == 'year.x'] <- "year"
  names(all.obs.covs)[names(all.obs.covs) == 'week_number.x'] <- "week_number"
  
  all.obs.covs$site.year<-paste(all.obs.covs$site, all.obs.covs$year,sep="_")
  
  
  data.obs.melt <- melt(all.obs.covs, id=c("site","week_number","site.year"),
                        measure=c("temp","temp_scale","wind","wind_scale","precipitation","precipitation_scale","humidity","humidity_scale"), na.rm=FALSE)
  
  #remove site.year that were not sampled
  dataIn$site.year<-paste(dataIn$site, dataIn$year, sep="_")
  siteYearList<-unique(as.character(dataIn$site.year))
  
  
  #convert data.obs.melt -Inf to NAs
  data.obs.melt[data.obs.melt=="-Inf"]<-NA
  data.obs.melt$site.year<-as.character(data.obs.melt$site.year)
  data.obs.melt<-data.obs.melt[data.obs.melt$site.year %in% siteYearList,]
  
  #gather weather covariates
  temp.data <- cast(data.obs.melt, site.year ~ week_number, add.missing=TRUE, fill=NA,fun.aggregate="max", subset=c(variable=="temp"))
  # colnames(temp.data)<-c("site.year","temp1","temp2","temp3","temp4","temp5","temp6","temp7","temp8","temp9","temp10","temp11","temp12")
  temp.scale.data <- cast(data.obs.melt, site.year ~ week_number, add.missing=TRUE, fill=NA,fun.aggregate="max", subset=c(variable=="temp_scale"))
  # colnames(temp.scale.data)<-c("site.year","temp.scale1","temp.scale2","temp.scale3","temp.scale4","temp.scale5","temp.scale6","temp.scale7","temp.scale8","temp.scale9","temp.scale10","temp.scale11","temp.scale12")
  
  wind.data <- cast(data.obs.melt, site.year ~ week_number, add.missing=TRUE, fill=NA,fun.aggregate="max", subset=c(variable=="wind"))
  # colnames(wind.data)<-c("site.year","wind1","wind2","wind3","wind4","wind5","wind6","wind7","wind8","wind9","wind10","wind11","wind12")
  
  wind.scale.data <- cast(data.obs.melt, site.year ~ week_number, add.missing=TRUE, fill=NA,fun.aggregate="max", subset=c(variable=="wind_scale"))
  # colnames(wind.scale.data)<-c("site.year","wind.scale1","wind.scale2","wind.scale3","wind.scale4","wind.scale5","wind.scale6","wind.scale7","wind.scale8","wind.scale9","wind.scale10","wind.scale11","wind.scale12")
  
  precipitation.data <- cast(data.obs.melt, site.year ~ week_number, add.missing=TRUE, fill=NA,fun.aggregate="max", subset=c(variable=="precipitation"))
  # colnames(precipitation.data)<-c("site.year","precip1","precip2","precip3","precip4","precip5","precip6","precip7","precip8","precip9","precip10","precip11","precip12")
  
  precipitation.scale.data <- cast(data.obs.melt, site.year ~ week_number, add.missing=TRUE, fill=NA, fun.aggregate="max", subset=c(variable=="precipitation_scale"))
  # colnames(precipitation.scale.data)<-c("site.year","precip.scale1","precip.scale2","precip.scale3","precip.scale4","precip.scale5","precip.scale6","precip.scale7","precip.scale8","precip.scale9","precip.scale10","precip.scale11","precip.scale12")
  
  humidity.data <- cast(data.obs.melt, site.year ~ week_number, add.missing=TRUE, fill=NA,fun.aggregate="max", subset=c(variable=="humidity"))
  # colnames(humidity.data)<-c("site.year","humidity1","humidity2","humidity3","humidity4","humidity5","humidity6","humidity7","humidity8","humidity9","humidity10","humidity11","humidity12")
  
  
  humidity.scale.data <- cast(data.obs.melt, site.year ~ week_number, add.missing=TRUE, fill=NA,fun.aggregate="max", subset=c(variable=="humidity_scale"))
  # colnames(humidity.scale.data)<-c("site.year","humidity.scale1","humidity.scale2","humidity.scale3","humidity.scale4","humidity.scale5","humidity.scale6","humidity.scale7","humidity.scale8","humidity.scale9","humidity.scale10","humidity.scale11","humidity.scale12")
  
  
  #add NAs into week matrix
  temp.use<-temp.data[,-1]
  week=species.data[,c("wk.1","wk.2","wk.3","wk.4","wk.5","wk.6","wk.7","wk.8","wk.9","wk.10","wk.11","wk.12")]
  out.NA<-list()
  for(i in 1:length(names(week))){
    out<-ifelse(is.na(temp.use[,i]),NA,week[,i])
    out.NA<-cbind(out.NA, out)
  }
  week.NA<-as.data.frame(out.NA, row.names=NULL)
  colnames(week.NA)<-c("wk.1","wk.2","wk.3","wk.4","wk.5","wk.6","wk.7","wk.8","wk.9","wk.10","wk.11","wk.12")
  
  #add NAs into count data
  temp.use<-temp.data[,-1]
  y<-species.data[,c("y.1","y.2","y.3", "y.4", "y.5","y.6","y.7","y.8", "y.9", "y.10","y.11","y.12")] 
  out.NA<-list()
  for(i in 1:length(names(week))){
    out<-ifelse(is.na(temp.use[,i]),NA,y[,i])
    out.NA<-cbind(out.NA, out)
  }
  
  y.NA<-as.data.frame(out.NA, row.names=NULL)

  colnames(y.NA)<-c("y.1","y.2","y.3", "y.4", "y.5","y.6","y.7","y.8", "y.9", "y.10","y.11","y.12")
  

  obs.covs<-list(week=data.frame(week.NA),
                 temp=data.frame(temp.data[,-1]),
                 temp.scale=data.frame(temp.scale.data[,-1]),
                 wind=data.frame(wind.data[,-1]),
                 wind.scale=data.frame(wind.data[,-1]),
                 humidity=data.frame(humidity.data[,-1]),
                 humidity.scale=data.frame(humidity.scale.data[,-1]),
                 precipitation=data.frame(precipitation.data[,-1]),
                 precipitation.scale=data.frame(precipitation.scale.data[,-1])
  )

  #now bundle everything into unmarkedFrame
  umf <- unmarkedFramePCount(y=y.NA,
                           siteCovs=covs,
                           obsCovs=obs.covs,
                           obsToY=TRUE)
  
  return(umf)
}

#End