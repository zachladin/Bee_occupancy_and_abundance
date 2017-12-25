#make Occupancy Model unmarkedFrame (Occu)
makeUmfOccu<-function(dataIn,species){
  require(unmarked)

  #species=speciesList[1]
  #redefine dataIn
  data.new<-dataIn
  
  #list of sites
  siteList<-unique(as.character(data.new$site))
  
  #get list of states
  stateName<-unique(as.character(data.new$state))
  
  #get list of counties
  countyList<-sort(unique(as.character(data.new$county)))

  #set site as factor
  #data.new$site<-as.factor(as.character(data.new$site))

  #create year.site
  data.new$year.site<-paste(data.new$year, data.new$site, sep="_")
  
  #create year.site.week
  data.new$year.site.week<-paste(data.new$year, data.new$site, data.new$week_number, sep="_")
  data.new$year.site.week<-as.factor(as.character(data.new$year.site.week))

  #create year.site.sex
  data.new$year.site.sex<-paste(data.new$year, data.new$site,data.new$sex, sep="_")
  data.new$year.site.sex<-as.factor(as.character(data.new$year.site.sex))

  #get count data and covs with melt and cast
  data.count.melt <- melt(data.new, id=c("year","site","week_number","genus","full_species_name","sex","latitude","longitude","state","county","year.site.week","year.site","year.site.sex"),measure=c("count"), na.rm=FALSE)
  
  data.count.melt$full_species_name<-as.character(data.count.melt$full_species_name)
  
  #make week_number a factor
  data.count.melt$week_number<-as.factor(as.character(data.count.melt$week_number))

  #cast count data (one visit at first) add try()
  count.data <- cast(data.count.melt, year.site.sex ~ week_number, add.missing=TRUE, fill=0,fun.aggregate=max, subset=c(full_species_name==species))
  
  #create year.site.sex column in count.data
  count.data$year.site.sex<-as.character(count.data$year.site.sex)

  #separate year, site, and sex as columns
  count.data.sep<-read.table(text=as.character(count.data$year.site.sex),colClasses = "character",sep="_")
  colnames(count.data.sep)<-c("year","site","sex")
  count.data$year.site<-paste(count.data.sep$year, count.data.sep$site,sep="_")
  
  count.data<-unique(count.data)
  #########################################################################
  # get list of all unique sites sampled in each year
  data.2014<-subset(data.new, year==2014)
  siteList2014<-as.character(unique(data.2014$site))
  data.2015<-subset(data.new, year==2015)
  siteList2015<-as.character(unique(data.2015$site))

  #combine to get siteList
  siteList<-unique(unlist(c(siteList2014, siteList2015)))
  
  #create sexList
  sexList<-c("Male","Female")
  
  #get all potentially sampled sites in each year
  #2014
      all.out.2014<-list()
    for(i in 1:length(sexList)){
      new.sites<-data.frame(site=siteList2014)
      new.sites$sex<-sexList[i]
      all.out.2014=rbind(all.out.2014,new.sites)
    }

    all.out.2014$year<-2014
    
 #2015  
    all.out.2015<-list()
    for(i in 1:length(sexList)){
      new.sites<-data.frame(site=siteList2015)
      new.sites$sex<-sexList[i]
      all.out.2015=rbind(all.out.2015,new.sites)
    }
    all.out.2015$year<-2015

    #combine 2014 and 2015
    all.out<-unique(rbind(all.out.2014, all.out.2015))
    #make year.site.sex column
    all.out$year.site.sex<-paste(all.out$year, all.out$site, all.out$sex,sep="_")
    
    #merge with count.data
    count.data.merge<-merge(count.data, all.out, by="year.site.sex",all=TRUE)
  
    #fill in count.data.merge NAs with 0s
    count.data.merge[is.na(count.data.merge)]<-0
    
    #delete year.site column
    count.data.merge$year.site<-NULL
    #make new year.site column
    count.data.merge$year.site<-paste(count.data.merge$year, count.data.merge$site, sep="_")
    
    #update all.out
    all.out<-count.data.merge[,c("site","year","sex","year.site","year.site.sex")]
    ########################################################################
    #COUNT DATA
    
    #read in NAs_bees list
    NAs_bees<-read.csv(paste(getwd(), "Data","NAs_bees.csv",sep="/"),header=TRUE)
    colnames(NAs_bees)[1]<-c("collection_event_id")
    
    #add year (all from 2014)
    NAs_bees$year<-2014
    
    #removed underscores from site
    NAs_bees$site<-gsub("_"," ", NAs_bees$site)
    #make site.week.year column
    NAs_bees$year.site<-paste(NAs_bees$year,NAs_bees$site,sep="_")
    
    #add sex
    NAs_bees.male<-NAs_bees
    NAs_bees.male$sex<-"Male"
    
    NAs_bees.female<-NAs_bees
    NAs_bees.female$sex<-"Female"
    
    NAs_bees.out<-rbind(NAs_bees.male, NAs_bees.female)
    NAs_bees.out$sex<-as.character(NAs_bees.out$sex)
    NAs_bees.out$year.site.sex<-paste(NAs_bees.out$year, NAs_bees.out$site, NAs_bees.out$sex, sep="_")
    NAs_bees.out$count<-NA
    
    naList<-unique(as.character(NAs_bees.out$year.site.sex))
    
    na.df<-unique(NAs_bees.out[,c("year.site.sex","week_number")])
    
    count.data.test<-count.data.merge
    count.data.test.sep<-read.table(text=as.character(count.data.test$year.site.sex),colClasses = "character",sep="_")
    colnames(count.data.test.sep)<-c("year","site","sex")
    count.data.test$year.site<-paste(count.data.test.sep$year, count.data.test.sep$site,sep="_")
    count.data.merge.1<-count.data.test
    
    for(i in 1:length(naList)){
      new.row<-subset(count.data.merge.1, year.site.sex==naList[i])
      
      new.na.df<-subset(na.df, year.site.sex==naList[i])
      new.weekNum<-as.character(new.na.df$week_number)
      
      new.row[,c(as.character(new.weekNum))]<- NA
      
      count.data.merge.1[row.names(new.row),]<- new.row[row.names(new.row),]
    }
    
    #reorder week columns
    count.data.out<-count.data.merge.1[,c("1","2","3","4","5", "6","7","8","9","10","11","12")]            
    
    #rename y column headers
    colnames(count.data.out)<-c("y.1","y.2","y.3", "y.4", "y.5","y.6","y.7","y.8", "y.9", "y.10","y.11","y.12")
    #save as y
    y<-count.data.out[,c("y.1","y.2","y.3", "y.4", "y.5","y.6","y.7","y.8", "y.9", "y.10","y.11","y.12")]
    
    #############################################################################################
    #SITE COVARIATES
    
    #get elevation covariates
    elev<-read.csv(paste(getwd(), "Data","elevation_all.csv",sep="/"),header=TRUE)
    elev$site<-as.character(elev$site)
    colnames(elev)<-c("site","latitude","longitude","elevation_m")
    #removed underscores from site
    elev$site<-gsub("_"," ", elev$site)
    
    elev.df<-elev[,c("site","elevation_m")]
    
    
  #get some site covariates from data.new
  data.new.covs<-unique(data.new[,c("site","county","state","forest_200","forest_500","forest_1000","grassland_200","grassland_500","grassland_1000","wetland_200","wetland_500","wetland_1000","developed_200","developed_500","developed_1000","forest_200_scale","forest_500_scale","forest_1000_scale","grassland_200_scale","grassland_500_scale","grassland_1000_scale","wetland_200_scale","wetland_500_scale","wetland_1000_scale","developed_200_scale","developed_500_scale","developed_1000_scale")])
  
    #make site a character
    data.new.covs$site<-as.character(data.new.covs$site)
    
    #merge elev.df and data.new.covs
    data.new.covs.2<-merge(data.new.covs, elev.df, by=c("site"),all.x=TRUE)
    
  
    #merge with all.out
    covs.merge<-merge(all.out, data.new.covs.2, by="site",all.x=TRUE)
    
    covs.merge$year<-as.factor(as.character(covs.merge$year))
    covs.merge$site<-as.factor(as.character(covs.merge$site))
    covs.merge$sex<-as.factor(as.character(covs.merge$sex))
    
  #number of visits
  nWeeks<-length(unique(data.new$week_number))

  #create a visitMat of nSites x nWeeks in dimensions
  visitMat<- matrix(rep(1:nWeeks,each=length(row.names(covs.merge))),ncol=nWeeks)
  colnames(visitMat)<-c("wk.1","wk.2","wk.3", "wk.4", "wk.5","wk.6","wk.7","wk.8", "wk.9", "wk.10","wk.11","wk.12")
  

  #add weather data
  weather.data<-read.csv(paste(getwd(), "Data","New_allweather_unique.csv",sep="/"))
  #weather.data$year<-substr(as.character(weather.data$Date),1,4)
  weather.data$site.week.year<-paste(weather.data$site, weather.data$week_number, weather.data$year, sep="_")
  #convert F to C temp
  weather.data$Mean_TemperatureC<-((weather.data$Mean_TemperatureF-32)*5)/9
  #convert wind MPH to m/s
  weather.data$Mean_Wind_Speed_m.per.s<-weather.data$Mean_Wind_SpeedMPH*0.44704
  #convert Precip (in) to (cm)
  weather.data$Precipitation_cm<-as.numeric(as.character(weather.data$PrecipitationIn))*2.54
  
  
  length(unique(weather.data$week_number))
  weather.data$site.week.year<-as.factor(as.character(weather.data$site.week.year))
  #get mean weather metrics per week.site.year
  mean.temp<-summaryFunction(dataIn=weather.data, factor="site.week.year",response="Mean_TemperatureC")
  mean.temp.out<-data.frame(site.week.year=mean.temp$site.week.year, temp=mean.temp$mean)
  
  mean.precip<-summaryFunction(dataIn=weather.data, factor="site.week.year",response="Precipitation_cm")
  mean.precip.out<-data.frame(site.week.year=mean.precip$site.week.year, temp=mean.precip$mean)
  
  mean.wind<-summaryFunction(dataIn=weather.data, factor="site.week.year",response="Mean_Wind_Speed_m.per.s")
  mean.wind.out<-data.frame(site.week.year=mean.precip$site.week.year, wind=mean.precip$mean)
  
  mean.humidity<-summaryFunction(dataIn=weather.data, factor="site.week.year",response="Mean_Humidity")
  mean.humidity.out<-data.frame(site.week.year=mean.humidity$site.week.year, wind=mean.humidity$mean)
  
  #compile weather covs
  weather.all.covs<-cbind(mean.temp.out, mean.precip.out[,-1], mean.wind.out[,-1], mean.humidity.out[,-1])
  colnames(weather.all.covs)<-c("site.week.year","temp","precipitation","wind","humidity")
  
  separate<-read.table(text=as.character(weather.all.covs$site.week.year), colClasses = "character",sep="_")
  colnames(separate)<-c("site","week","year")
  weather.all.covs.out<-cbind(separate,weather.all.covs)
  
  weather.all.covs.out$site<-as.character(weather.all.covs.out$site)
  weather.all.covs.out$site<-gsub(" ","_",weather.all.covs.out$site)
  weather.all.covs.out$site.week.year<-gsub(" ","_",weather.all.covs.out$site.week.year)
  
  #now get scaled weather covariates
  weather.all.covs.out$temp_scale<-scale(weather.all.covs$temp)
  weather.all.covs.out$precipitation_scale<-scale(weather.all.covs$precipitation)
  weather.all.covs.out$wind_scale<-scale(weather.all.covs$wind)
  weather.all.covs.out$humidity_scale<-scale(weather.all.covs$humidity)
  
  #add year.site colummn
  weather.all.covs.out$site.year<-paste(weather.all.covs.out$site, weather.all.covs.out$year,  sep="_")
  
  #add sex columns
  weather.male<-data.frame(weather.all.covs.out, sex="Male")
  weather.female<-data.frame(weather.all.covs.out, sex="Female")
  #combine
  weather.all.out<-rbind(weather.male, weather.female)
  
  weather.all.out$site.year.sex<-paste(weather.all.out$site, weather.all.out$year,  weather.all.out$sex, sep="_")
  #####################################################################################################


  data.obs.melt <- melt(weather.all.out, id=c("site","week","year","site.year","site.year.sex"),
                          measure=c("temp","temp_scale","wind","wind_scale","precipitation","precipitation_scale","humidity","humidity_scale"), na.rm=FALSE)
  
  #function to replace NAs in right slots
  addNAs<-function(x){
    df<-x[,c("1","2","3","4","5","6","7","8","9","10","11","12")]
    
    out.NA<-NULL
    for(i in 1:length(names(df))){
      out<-ifelse(is.na(y[,i]),NA, df[,i])
      out.NA<-cbind(out.NA, out)
    }
    out<-data.frame(df[,1],out.NA)
    colnames(out)<-colnames(x)
    return(out)
  }

  #gather weather covariates
 temp.data <- cast(data.obs.melt, site.year.sex ~ week, add.missing=TRUE, fill=0,fun.aggregate="max", subset=c(variable=="temp"))
 temp.data.out<-data.frame(addNAs(temp.data))
 colnames(temp.data.out)<-c("site.year.sex","temp1","temp2","temp3","temp4","temp5","temp6","temp7","temp8","temp9","temp10","temp11","temp12")
 
 
 temp.scale.data <- cast(data.obs.melt, site.year.sex ~ week, add.missing=TRUE, fill=0,fun.aggregate="max", subset=c(variable=="temp_scale"))
 temp.scale.data.out<-addNAs(temp.scale.data)
 colnames(temp.scale.data.out)<-c("site.year.sex","temp.scale1","temp.scale2","temp.scale3","temp.scale4","temp.scale5","temp.scale6","temp.scale7","temp.scale8","temp.scale9","temp.scale10","temp.scale11","temp.scale12")
 
 wind.data <- cast(data.obs.melt, site.year.sex ~ week, add.missing=TRUE, fill=NA,fun.aggregate="max", subset=c(variable=="wind"))
 wind.data.out<-addNAs(wind.data)
 colnames(wind.data.out)<-c("site.year.sex","wind1","wind2","wind3","wind4","wind5","wind6","wind7","wind8","wind9","wind10","wind11","wind12")
 
  wind.scale.data <- cast(data.obs.melt, site.year.sex ~ week, add.missing=TRUE, fill=NA,fun.aggregate="max", subset=c(variable=="wind_scale"))
  wind.scale.data.out<-addNAs(wind.scale.data)
  colnames(wind.scale.data.out)<-c("site.year.sex","wind.scale1","wind.scale2","wind.scale3","wind.scale4","wind.scale5","wind.scale6","wind.scale7","wind.scale8","wind.scale9","wind.scale10","wind.scale11","wind.scale12")
  
 precipitation.data <- cast(data.obs.melt, site.year.sex ~ week, add.missing=TRUE, fill=NA,fun.aggregate="max", subset=c(variable=="precipitation"))
 precipitation.data.out<-addNAs(precipitation.data)
 colnames(precipitation.data.out)<-c("site.year.sex","precip1","precip2","precip3","precip4","precip5","precip6","precip7","precip8","precip9","precip10","precip11","precip12")
 
 precipitation.scale.data <- cast(data.obs.melt, site.year.sex ~ week, add.missing=TRUE, fill=NA, fun.aggregate="max", subset=c(variable=="precipitation_scale"))
 precipitation.scale.data.out<-addNAs(precipitation.scale.data)
 colnames(precipitation.scale.data.out)<-c("site.year.sex","precip.scale1","precip.scale2","precip.scale3","precip.scale4","precip.scale5","precip.scale6","precip.scale7","precip.scale8","precip.scale9","precip.scale10","precip.scale11","precip.scale12")
 
 humidity.data <- cast(data.obs.melt, site.year.sex ~ week, add.missing=TRUE, fill=NA,fun.aggregate="max", subset=c(variable=="humidity"))
 humidity.data.out<-addNAs(humidity.data)
 colnames(humidity.data.out)<-c("site.year.sex","humidity1","humidity2","humidity3","humidity4","humidity5","humidity6","humidity7","humidity8","humidity9","humidity10","humidity11","humidity12")
 
 
 humidity.scale.data <- cast(data.obs.melt, site.year.sex ~ week, add.missing=TRUE, fill=NA,fun.aggregate="max", subset=c(variable=="humidity_scale"))
 humidity.scale.data.out<-addNAs(humidity.scale.data)
 colnames(humidity.scale.data.out)<-c("site.year.sex","humidity.scale1","humidity.scale2","humidity.scale3","humidity.scale4","humidity.scale5","humidity.scale6","humidity.scale7","humidity.scale8","humidity.scale9","humidity.scale10","humidity.scale11","humidity.scale12")
 
 
 #add NAs into week matrix
 week=as.data.frame(visitMat)

 out.NA<-NULL
 for(i in 1:length(names(week))){
   out<-ifelse(is.na(y[,i]),NA, week[,i])
   out.NA<-cbind(out.NA, out)
 }
 
 week.NA<-out.NA
 colnames(week.NA)<-c("wk.1","wk.2","wk.3","wk.4","wk.5","wk.6","wk.7","wk.8","wk.9","wk.10","wk.11","wk.12")
 
 week=week.NA
 temp=temp.data.out[,-1]
 temp.scale= data.frame(temp.scale.data.out[,-1])
 wind= data.frame(wind.data.out[,-1])
 wind.scale=data.frame(wind.data.out[,-1])
 humidity=data.frame(humidity.data.out[,-1])
 humidity.scale=data.frame(humidity.scale.data.out[,-1])
 precip=data.frame(precipitation.data.out[,-1])
 precip.scale=data.frame(precipitation.scale.data.out[,-1])

 obs.covs<-list(week=week, temp=temp, temp.scale=temp.scale, wind=wind,
                wind.scale=wind.scale, humidity=humidity, humidity.scale=humidity.scale,
                precip=precip, precip.scale=precip.scale)
                
                
  #head(covs)

 #now site.covariates

  #convert y to 0/1
  y1<-y
  y1[y1>1] <- 1
  

  #now bundle everything into unmarkedFrame
  umf <- unmarkedFrameOccu(y=y1,
                          obsCovs = obs.covs,
                          siteCovs=covs.merge)

  return(umf)
}

#End