#use 'unmarked' to run frequentist hierarchical linear models using gmultmix function
runUnmarked<-function(dataIn, speciesName){

  new.data<-dataIn

  #make umf
  umf<-makePcountUmf(dataIn=new.data,species=speciesName)

  #run model in unmarked
  mod.1<-tryCatch({
    message("Running model with mixture=P and Week as detection covariate.")
    cond1=pcount(~Week ~county, data=umf, mixture="P")
    cond1
    },error=function(cond2){
      message("Running model with mixture=NB and Week as detection covariate.")
      cond2=pcount(~Week ~county, data=umf, mixture="NB")
      cond2
    },error=function(cond3){
      message("Running model with mixture=ZIP and Week as detection covariate.")
      cond3=pcount(~Week ~county, data=umf, mixture="ZIP")
      cond3
    },error=function(cond4){
      message("Running model with mixture=P and no detection covariates.")
      cond4=pcount(~1 ~county, data=umf, mixture="ZIP")
      cond4
    }
  )

  return(mod.1)
}

#End