##################################################
## Project: SoilDepth
## Script purpose: computes the soil depth values as reported in Hunt et al.: "Prediction of soil formation as a function of age using the precolation theory approach"
## Date: Mai, 2018
## Author: Curdin Derungs, Markus Egli
##################################################

#clean environment
rm(list=ls())

####loading input file----

#input file for the alps
#contains all the input parameters for modelling soil depth
#
#"input/inputAlps.csv": contains the input parameters of the Alps
#"input/inputMediter.csv": contains the input parameters of the Mediterranian
input<-read.csv("input/inputAlps.csv",sep=";",header=T)

#for performance reasons only selecting ten random parameter settings
input.s<-input[sample(1:nrow(input),size = 10,replace = F),]

####function to compute soil depth----

soilDepthComp<-function(part,I,erosion,yearMax){
  ####compute----
  
  year<-1:yearMax
  
  soilDepth<-numeric()
  soilProd<-numeric()
  soilDepthEr<-numeric()
  
  soilDepth[1]<-part*(year[1]/(part/I))^0.53
  soilDepth[2]<-part*(year[2]/(part/I))^0.53
  
  soilProd[1]<-NA
  soilProd[2]<-I*(1/1.87)*(part/soilDepth[2])^.87
  
  soilDepthEr[1]<-NA
  soilDepthEr[2]<-soilDepth[1]
  
  for(i in 3:max(year)){
    soilDepth[i]<-part*(year[i]/(part/I))^0.53
    soilProd[i]<-I*(1/1.87)*(part/soilDepthEr[(i-1)])^0.87
    soilDepthEr[i]<-soilDepthEr[(i-1)]+(soilProd[i])-(erosion)
  }
  
  
  results<-data.frame(year=year,sP=soilProd,sDE=soilDepthEr)
  
  return(results)
}

#empty data.frame with all required information
all<-data.frame(Site=NA,
                ProfileName=NA,
                part=NA,
                I=NA,
                erosion=NA,
                yearMax=NA,
                year=NA,
                sP=NA,
                sDE=NA)


####computing soil depth----

#executing soildepth-function for the small dataset
for(i in 1:nrow(input.s)){
  print(i)
  results<-soilDepthComp(input$part[i],input$I[i],input$erosion[i],input$yearMax[i])
  resI<-results[results$year==input$yearMax[i],]
  all<-rbind(all,cbind(input[i,],resI))
}

all<-all[-1,]
all.df<-all


####writing results to csv----

write.csv(all.df, "/output/soilDepth.txt")


