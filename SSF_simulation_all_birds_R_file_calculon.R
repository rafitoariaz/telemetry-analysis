## ---- include=FALSE------------------------------------------------------
#Load libraries
library("openxlsx")
library("NISTunits")
library("rgdal")
#library("ggmap")
library("SDMTools")
library("raster")
#library("MetaLandSim") # need to run code source("https://bioconductor.org/biocLite.R") biocLite("Biobase")
#library("rgeos")
library("maps")
#library("maptools")
#library("geosphere")
library("amt")
library("tidyr")
#library("cowplot")
#library("reshape2")
library("dplyr")
library("ggplot2")
#library("ggrepel")
library("GGally")
library("parallel")
library("doParallel")
library("foreach")
library("sp")
library("stats")
#library("knitr")



## ------------------------------------------------------------------------
#Soruce simulation functions
source("SSF_simulation_functions_all_birds_calculon.R")

#Load workspace for simulation modelling
load("/scratch/ariasmed/simulation_modelling_workspace.RData")

#Set up the number of simulations per patch and also number of simulations per starting point
num.sim.points.per.patch<-30
num.sim.per.point<-1

#Make a list of the
results.simulations.list <- split(parameters.simulations.model$id.simulation,parameters.simulations.model$id.simulation)

registerDoParallel(detectCores())
#registerDoParallel(40)

#1,2,4,8,13,15,17,18
#for (i in 1:nrow(parameters.simulations)){
results.simulations.list <- foreach (i= c(1,2,4,9,11,13,14),
                                .packages=c("amt","raster")) %dopar% {

simulate.movement(focal.patch=forest.raster.cropped[[i]]$focal.patch.label.cropped,
                  tot.num.minutes=60,
                  fix.rate=5,
                  parameters.simulations=parameters.simulations.model,
                  num.sim.points.per.patch=num.sim.points.per.patch, #I tried to only put a number and then save the number I put with the name of the variable but I could not do it, so that is why I specifya outside the function the value and store in in the variable
                  num.sim.per.point=num.sim.per.point, #I tried to only put a number and then save the number I put with the name of the variable but I could not do it, so that is why I specifya outside the function the value and store in in the variable
                  map.full=land_use_raster,
                  map.cropped=forest.raster.cropped[[i]]$forest.raster.cropped,
                  map.cropped.labeled=forest.raster.cropped[[i]]$forest.raster.cropped.labeled,
                  landscape.variables=forest.raster.cropped[[i]]$landscape.variables,
                  name.output.list=raw.results.emigration) #Remember now that the output data frame is results.simulations.list


  
}



stopImplicitCluster()

dput(results.simulations.list,"/scratch/ariasmed/results.simulations.list")
