#save and plot abundance estimates
getModelOutput<-function(dataIn, modelIn, speciesName){

  message("Compiling and saving model results.")
  new.data<-dataIn

  stateName<-unique(as.character(new.data$state))

  mod.1.out<-predict(modelIn, type="state", appendData=TRUE)

  #get columns to create table of Abundance estimates
  species.abun<-unique(mod.1.out[, c("year","Region","state","county","Predicted","SE","lower","upper")])

  #add column with model formula
  model.formula<-paste(deparse(modelIn@call)[1], deparse(modelIn@call)[2],sep="")
  species.abun$model.formula<-model.formula

  #reorder table of results
  species.abun.table<-species.abun[order(species.abun$county),]

  #create filepath for saving unmarked results
  abundanceFilepath<-paste(getwd(), "Results","Abundance_Estimates",sep="/")

  #create folder for each species
  dir.create(paste(abundanceFilepath, stateName,speciesName, sep="/"))

  #save table as .csv file
  write.csv(species.abun.table, file=paste(abundanceFilepath, stateName, speciesName,paste(speciesName,stateName,"abundance.by.county.csv",sep="."),sep="/"),row.names=FALSE)

  #save detection probs
  det.probs<-unique(getP(modelIn))
  write.csv(det.probs,file=paste(abundanceFilepath, stateName,speciesName,paste(speciesName,stateName,"detectionProb.by.week.csv",sep="."),sep="/"),row.names=FALSE)

  #save model information and coefficients
  mod.coefs<-coef(modelIn)
  dput( list(coefs=mod.coefs),
        file=paste(abundanceFilepath,stateName, speciesName, paste(speciesName,stateName,"_","model_coefficients",".R", sep=''),sep="/")   )

  mod.coefs.df<-as.data.frame(mod.coefs)
  mod.coefs.df.2<-data.frame(State=stateName,Species=speciesName,Coefficient=row.names(mod.coefs.df), Value=mod.coefs.df$mod.coefs,Model=model.formula)

  #now save for later
  write.csv(mod.coefs.df.2, file=paste(abundanceFilepath, stateName, speciesName,paste(speciesName,stateName,"coefficients.by.unit.csv",sep="."),sep="/"),row.names=FALSE)


  #NOW PLOT

  #get number of counties
  plot.units<-if((length(unique(species.abun$county)) < 7)==TRUE){
    round(length(unique(species.abun$county))/2,0)
  }else{
    round(length(unique(species.abun$county))/4,0)
  }

  #reorder factor levels for county

  #sort highest to lowest by site
  species.abun.sort<-species.abun[order(species.abun$Predicted,decreasing=TRUE),]
  county.levels<-species.abun.sort$county
  species.abun.sort$order <- factor(species.abun.sort$county, levels = county.levels)

  #make Week.Number a factor
  species.abun.sort$county<-as.factor(as.character(species.abun.sort$county))

  #plot abundance estimates by year and unit
  plot.abun<-ggplot(data=species.abun, aes(x=county, y=Predicted, ymin=Predicted-SE, ymax=Predicted+SE))+
    coord_flip()+
    geom_errorbar(width=0, size=1, aes(color=county))+
    geom_point(stat="identity",size=4,aes(color=county),alpha=1)+
    ggtitle(paste(speciesName,"abundance in", stateName, " by county",sep=" "))+
    #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(), panel.border=element_blank())+
    #theme(panel.background=element_rect(fill='white'))+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5,vjust=0.5, size=14, color="black"),
          axis.text.y = element_text(size=14, color="black"),
          axis.title.x = element_text(size=16, hjust=0.5, vjust=0.2),
          axis.title.y = element_text(angle = 90, vjust=1.2, size=16))+
    theme(panel.border=element_rect(color="black",fill=NA))+
    theme(legend.position="none")+
    #scale_fill_brewer(palette="Set2")+
    #scale_color_brewer(palette="Set2")+
    labs(x="County", y="Estimated abundance")+
    scale_x_discrete(limits=rev(levels(species.abun.sort$order)))
  # guides(fill = guide_legend(reverse = TRUE), color=guide_legend(reverse = TRUE))
  #abun.plot.out<-plot.abun+facet_wrap(~county,ncol=plot.units,scales="free_x")
  plot.abun
  print(plot.abun)

  #save plot of abundance estimates
  myFilepath<-paste(abundanceFilepath,stateName,speciesName,sep="/")
  #ggsave(plot.abun, filename=paste(speciesName,"summary.abun.fig.pdf",sep="."),path=myFilepath, width=7.5,height=5, limitsize=FALSE)
  ggsave(plot.abun, filename=paste(speciesName,stateName,"summary.abun.fig.png",sep="."),path=myFilepath, width=8.5,height=11, limitsize=FALSE)



}

#End
