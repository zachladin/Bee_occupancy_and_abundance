#formatting data
formatBeeData<-function(dataIn){

  dataIn2<-dataIn
  
  #remove any rows where sex is blank
  dataIn2$sex<-as.character(dataIn2$sex)
  dataIn2<-dataIn2[! dataIn2$sex=="",]
  dataIn2$sex<-as.factor(dataIn2$sex)
  levels(dataIn2$sex)<-c("Female","Male")
  
  
  siteFixList<-c("Audrey_Carroll_Audubon_Sanctuary", "Blackiston","Catoctin_Creek_NC", "Cecil_Jones", "Cecil_Wilen",
                 "Collins_Plot","Corcoran_Woods_Old_Wire_Section","Dorchester_Ponds","ERR&RR","Little_Creek","NPS_DC1",
                 "Pickering_Creek","Plum_Creek")

  #loop to fix coords in data
  for(i in 1:length(siteFixList)){
    
    temp.site<-as.character(siteFixList[i])
    sub.data<-subset(dataIn2, site==temp.site)
    temp.coords<-unique(sub.data[,c("longitude","latitude")])
    use.coords<-temp.coords[1,]
    
    dataIn2$longitude[dataIn2$site %in% siteFixList[i]] <- use.coords$longitude
    dataIn2$latitude[dataIn2$site %in% siteFixList[i]] <- use.coords$latitude
    
  }
  

  message("Formatting and saving Bee data as .csv")

  #grab desired columns
  new.data<-dataIn2[,c("collection_event_id","specimen_id","family",
                    "full_species_name","genus","species","sex","latitude","longitude",
                    "state","county","site","number_days","time1","time2","day1","month1","day2","month2","year","week_number","volunteer_name")]
  
  new.data$site<-as.character(new.data$site)
  new.data$site<-gsub("_"," ",new.data$site)
  

  #convert District of Columbia to DC
  levels(new.data$state)[levels(new.data$state)=="District of Columbia"] <- "DC"
  
  #create a date (from day of deployment)
  date1<-paste(new.data$month1, new.data$day1, new.data$year, sep="-")
  
  #add Month, Year, and Day columns to dat (for day of deployment of bowls)
  date.data<-date1
  date.data<-as.character(date.data)
  date.string<-read.table(text=as.character(date.data), sep="-",colClasses = "character")
  colnames(date.string)<-c("Month","Day","Year")
  date.string$new.date<-as.character(paste(date.string$Month,date.string$Day,date.string$Year,sep="-"))
  date.string$day_ord1 <- as.integer(format(as.Date(date.string$new.date, format = "%m-%d-%Y"), "%j"))
  
  #add day_ord back to data.frame
  new.data$day_ord1<-date.string$day_ord1
  
  #create a date (from day of collection of bowls)
  date2<-paste(new.data$month2, new.data$day2, new.data$year, sep="-")
  
  #add Month, Year, and Day columns to dat (for day of deployment of bowls)
  date.data<-date2
  date.data<-as.character(date.data)
  date.string<-read.table(text=as.character(date.data), sep="-",colClasses = "character")
  colnames(date.string)<-c("Month","Day","Year")
  date.string$new.date<-as.character(paste(date.string$Month,date.string$Day,date.string$Year,sep="-"))
  date.string$day_ord2 <- as.integer(format(as.Date(date.string$new.date, format = "%m-%d-%Y"), "%j"))
  
  #now add day_ord
  new.data$day_ord2<-date.string$day_ord2
  
  #add collection period length column
  #new.data$collection_length_days<-new.data$day_ord2-new.data$day_ord1
  #add useful columns for later

  #create a unique site ID number
  new.data$UniqueSiteString<-paste(new.data$site, new.data$latitude, new.data$longitude,sep="_")
  
  new.data$site_id<-paste("Site",as.integer(as.factor(new.data$UniqueSiteString)),sep="_")

  #fix species names with "/" in names that could monkey with filepaths when saving
  #new.data$Full.Species.Name<-gsub("/","_",new.data$Full.Species.Name)

  #make Week.species column
  new.data$week_species<-paste(new.data$week_number, new.data$full_species_name, sep=".")

  #make Site.Week.Species column
  new.data$site_week_species<-paste(new.data$site_id,new.data$week_number, new.data$full_species_name, sep=".")


  #remove "NA" weeks from data
  new.data.2<-subset(new.data, week_number != "NA")
  head(new.data.2)

  #remove any lat lon coords with NA (Wolf Trap Ephemeral has a some missing longitudes)
  new.data.3<-new.data.2[!is.na(new.data.2$longitude),]

  length(unique(new.data.3$site_id))
  
  #add NLCD data
  lc.data<-read.csv(paste(getwd(), "Data","R_extracted_NLCD_data.csv",sep="/"), header=TRUE)
  lc.data$Class<-trimws(lc.data$Class)
  lc.data.sub<-lc.data[,c("site","Class","buffer","Proportion")]
  lc.data.sub$class.buffer<-paste(lc.data.sub$Class, lc.data.sub$buffer,sep="_")
  lc.data.melt<-melt(lc.data.sub, id.vars=c("site", "Class","buffer","class.buffer"), measure.vars=c("Proportion"))
  lc.data.melt$site.buffer<-paste(lc.data.melt$site, lc.data.melt$class.buffer,sep="_")
  
  lc.data.cast<-cast(lc.data.melt, site ~ class.buffer, fun.aggregate="max")
  
  row.names(lc.data.cast)<-NULL

  lc.data.cast$site<-as.character(lc.data.cast$site)
  lc.data.cast$site<-gsub("_"," ",lc.data.cast$site)
  
colnames(lc.data.cast)<-c("site","Barren_Land_1000","Barren_Land_200","Barren_Land_500","Cultivated_Crops_1000","Cultivated_Crops_200","Cultivated_Crops_500","Deciduous_Forest_1000","Deciduous_Forest_200","Deciduous_Forest_500","Developed_High_Intensity_1000","Developed_High_Intensity_200","Developed_High_Intensity_500","Developed_Low_Intensity_1000","Developed_Low_Intensity_200","Developed_Low_Intensity_500", "Developed_Medium_Intensity_1000","Developed_Medium_Intensity_200","Developed_Medium_Intensity_500","Developed_Open_Space_1000","Developed_Open_Space_200","Developed_Open_Space_500","Emergent_Herbaceous_Wetlands_1000","Emergent_Herbaceous_Wetlands_200","Emergent_Herbaceous_Wetlands_500","Evergreen_Forest_1000","Evergreen_Forest_200","Evergreen_Forest_500","Grassland_Herbaceous_1000","Grassland_Herbaceous_200","Grassland_Herbaceous_500","Mixed_Forest_1000","Mixed_Forest_200","Mixed_Forest_500","Open_Water_1000","Open_Water_200","Open_Water_500","Pasture_Hay_1000","Pasture_Hay_200","Pasture_Hay_500","Shrub_Scrub_1000","Shrub_Scrub_200","Shrub_Scrub_500","Woody_Wetlands_1000","Woody_Wetlands_200","Woody_Wetlands_500")                             


#now create merged landcover columns at different scales
#forest
lc.data.cast$forest_200<-lc.data.cast$Deciduous_Forest_200 + lc.data.cast$Evergreen_Forest_200 + lc.data.cast$Mixed_Forest_200
lc.data.cast$forest_500<-lc.data.cast$Deciduous_Forest_500 + lc.data.cast$Evergreen_Forest_500 + lc.data.cast$Mixed_Forest_500
lc.data.cast$forest_1000<-lc.data.cast$Deciduous_Forest_1000 + lc.data.cast$Evergreen_Forest_1000 + lc.data.cast$Mixed_Forest_1000

#grassland
lc.data.cast$grassland_200<- lc.data.cast$Cultivated_Crops_200 + lc.data.cast$Grassland_Herbaceous_200 + lc.data.cast$Pasture_Hay_200 + lc.data.cast$Shrub_Scrub_200
lc.data.cast$grassland_500<- lc.data.cast$Cultivated_Crops_500 + lc.data.cast$Grassland_Herbaceous_500 + lc.data.cast$Pasture_Hay_500 + lc.data.cast$Shrub_Scrub_500
lc.data.cast$grassland_1000<- lc.data.cast$Cultivated_Crops_1000 + lc.data.cast$Grassland_Herbaceous_1000 + lc.data.cast$Pasture_Hay_1000 + lc.data.cast$Shrub_Scrub_1000

#developed
lc.data.cast$developed_200<- lc.data.cast$Developed_High_Intensity_200 + lc.data.cast$Developed_Medium_Intensity_200 + lc.data.cast$Developed_Low_Intensity_200 + lc.data.cast$Developed_Open_Space_200
lc.data.cast$developed_500<- lc.data.cast$Developed_High_Intensity_500 + lc.data.cast$Developed_Medium_Intensity_500 + lc.data.cast$Developed_Low_Intensity_500 + lc.data.cast$Developed_Open_Space_500
lc.data.cast$developed_1000<- lc.data.cast$Developed_High_Intensity_1000 + lc.data.cast$Developed_Medium_Intensity_1000 + lc.data.cast$Developed_Low_Intensity_1000 + lc.data.cast$Developed_Open_Space_1000

#wetland
lc.data.cast$wetland_200<-lc.data.cast$Emergent_Herbaceous_Wetlands_200 + lc.data.cast$Open_Water_200 + lc.data.cast$Woody_Wetlands_200
lc.data.cast$wetland_500<-lc.data.cast$Emergent_Herbaceous_Wetlands_500 + lc.data.cast$Open_Water_500 + lc.data.cast$Woody_Wetlands_500
lc.data.cast$wetland_1000<-lc.data.cast$Emergent_Herbaceous_Wetlands_1000 + lc.data.cast$Open_Water_1000 + lc.data.cast$Woody_Wetlands_1000

#now scale land cover covariates

#forest
lc.data.cast$forest_200_scale<-scale(lc.data.cast$forest_200)
lc.data.cast$forest_500_scale<-scale(lc.data.cast$forest_500)
lc.data.cast$forest_1000_scale<-scale(lc.data.cast$forest_1000)

#grassland
lc.data.cast$grassland_200_scale<- scale(lc.data.cast$grassland_200)
lc.data.cast$grassland_500_scale<- scale(lc.data.cast$grassland_500)
lc.data.cast$grassland_1000_scale<- scale(lc.data.cast$grassland_1000)

#developed
lc.data.cast$developed_200_scale<- scale(lc.data.cast$developed_200)
lc.data.cast$developed_500_scale<- scale(lc.data.cast$developed_500)
lc.data.cast$developed_1000_scale<- scale(lc.data.cast$developed_1000)

#wetland
lc.data.cast$wetland_200_scale<-scale(lc.data.cast$wetland_200)
lc.data.cast$wetland_500_scale<-scale(lc.data.cast$wetland_500)
lc.data.cast$wetland_1000_scale<-scale(lc.data.cast$wetland_1000)

  #merge with data
  new.data.4<-merge(new.data.3, lc.data.cast, by=c("site"), all.x=TRUE)
  
  
  #now merge with all data
  new.data.4$site.week.year<-paste(new.data.4$site, new.data.4$week_num, new.data.4$year, sep="_")
  
  new.data.5<-new.data.4
  
  #fix columns
  # new.data.5$site.y<-NULL
  # new.data.5$week<-NULL
  # new.data.5$year.y<-NULL
  # names(new.data.5)[names(new.data.5) == 'site.x'] <- 'site'
  # names(new.data.5)[names(new.data.5) == 'year.x'] <- "year"
  
  #########################
  #save as new .csv file
  write.csv(new.data.5, paste(getwd(),"Data", "Bee.formatted.data.csv",sep="/"),row.names=FALSE)

  #now get counts per site per week for each speceis
  #new.data.3

  #fix some species names
  new.data.5$full_species_name<-as.character(new.data.5$full_species_name)
  new.data.5$full_species_name[new.data.5$full_species_name=="Halictus poeyi_ligatus"]<- "Halictus poeyiligatus"
  new.data.5$full_species_name[new.data.5$full_species_name=="Nomada sayi_illinoensis"]<- "Nomada sayiillinoensis"
  new.data.5$full_species_name[new.data.5$full_species_name=="Hylaeus affinis_modestus"]<- "Hylaeus affinis modestus"
  new.data.5$full_species_name[new.data.5$full_species_name=="Andrena arabis_algida"]<- "Andrena arabis algida"
  new.data.5$full_species_name[new.data.5$full_species_name=="Andrena imitatrix_morrisonella"]<- "Andrena imitatrix morrisonella"
  new.data.5$full_species_name[new.data.5$full_species_name=="Nomada bidentate_group"]<- "Nomada bidentate group"
  
  
  #make a site_week_species_sex category
  new.data.5$year.site.week.species.sex<-paste(new.data.5$year,new.data.5$site, new.data.5$week_number, new.data.5$full_species_name, new.data.5$sex, sep="_")
  # new.data.5$sex<-as.character(new.data.5$sex)
  # new.data.5$sex[new.data.5$sex==""]<-"unknown"
  # levels(new.data.5$sex)
  
  #remove all unknown sex 

  
  #create a table of counts of each species per week
  freq.table<-table(new.data.5$year.site.week.species.sex)
  count.data<-as.data.frame(freq.table)

  #rename column headers
  colnames(count.data)<-c("year.site.week.species.sex","count")
  head(count.data, 46)

  #separate out week from spcies name
  covs<-read.table(text=as.character(count.data$year.site.week.species.sex),colClasses="character", sep="_")
  colnames(covs)<-c("year","site","week_number","full_species_name","sex")

  count.data.1<-data.frame(covs,count.data)
  head(count.data.1)

  ##################################################################
  #get Site covariates (add ordinal day)
  
  new.data.5.names<-data.frame(number=seq(1, length(names(new.data.5))), columnNames=names(new.data.5))
  
  siteCovsnames<-names(new.data.5[,29:99])
  site.data<-new.data.5[,c("site","latitude","longitude","state","county","site_id","year","genus","sex",siteCovsnames)]

  
  #get unique list of site covariates
  site.covs<-unique(site.data)

  
  
  #save site.covs
  write.csv(site.covs,paste(getwd(),"Data", "Bee.data.site.covs.csv",sep="/"),row.names=FALSE)
  ##################################################################
  #make site.year in count.data.1
  count.data.1$site.year<-paste(count.data.1$site, count.data.1$year, sep="_")
  count.data.1$site.year<-as.character(count.data.1$site.year)
  #add site.week column
  site.covs$site.year<-paste(site.covs$site, site.covs$year, sep="_")
  
  
  #merge site.covs with count.data.1
  count.data.2<-merge(count.data.1, site.covs, by=c("site.year"),all.x=TRUE)
  #tidy up columns
  count.data.2$site.y<-NULL
  count.data.2$week_number.y<-NULL
  count.data.2$sex.y<-NULL
  count.data.2$year.y<-NULL
  colnames(count.data.2)[2]<-"year"
  colnames(count.data.2)[3]<-"site"
  names(count.data.2)[names(count.data.2)=="sex.x"]<-"sex"
  

  write.csv(count.data.2, paste(getwd(),"Data", "Bee.formatted.count.data.csv",sep="/"),row.names=FALSE)

  return(count.data.2)
}
