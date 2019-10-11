####FUNCTION FOR SIMULATING MOVEMENT BEHAVIOR OF INDIVIDUALS####
#This function simlates movement behavior of an animal and needs the following information 
#focal.patch: the patches where the simulations will be made
#focal.patch.immigration: this is the focal patch that indicates te patch where the seed should land to be considered an immigrated seed 
#fix.rate: the time at which each point will be considered
#parameters.simulations: name of the data frame that will have the parameters for each simulation
#num.sim.points.per.patch: number of simulations that will be made starting from each focal patch
#num.sim.per.point: number of simulations that will be made per starting point
#map.full: full map with vegetation cover
#map.cropped: vegetation map cropped to the area where the simulation is going to be made
#map.cropped.labeled: same as map.cropped but instead of having vegetation cover id it has patch id for each forest patch
#name.output.list: the name of the list generated 


simulate.movement<-function(focal.patch=NA,
                            focal.patch.immigration=NA,
                            tot.num.minutes=NA,
                            fix.rate=NA,
                            parameters.simulations=NA,
                            num.sim.points.per.patch=NA,
                            num.sim.per.point=NA,
                            map.full=NA,
                            map.cropped=NA,
                            map.cropped.labeled=NA,
                            landscape.variables=NA,
                            name.output.list=NA){
  
  #time.location.regurgitation<<-as.list(focal.patch)
  time.location.regurgitation<<-as.list(parameters.simulations$id.simulation)
  #names(time.location.regurgitation)<<-focal.patch #For now the names are in a character format
  
  
  #Calculate total number of steps
  tot.num.steps<-tot.num.minutes/fix.rate
  
  #Create a data frame with the information needed from the output
  one.sim <<-data.frame(focal.patch=0,
                        num.simulation=0, #bird id. ##***I do not know if this is useful
                        param.shape.step.length=0,
                        param.scale.step.length=0,
                        param.habitat.utilization=0,
                        time=rep(seq(from=0,to=tot.num.minutes,by=fix.rate)), #How many minutes have passed since the bird started to move?
                        x=0, #x coordinate
                        y=0, #y coordinate
                        raster.value=0,
                        patch.emigrated=0,
                        same.patch=0,
                        matrix=0,
                        new.patch=0,#value of the raster (type of vegetation)
                        patch.size=0,###ADDED THIS CODE
                        percent.forest=0,
                        elevation=0,
                        patch.id.adam=0) 
  
  for (q in 1:nrow(parameters.simulations)){
    
    one.sim$id.simulation=parameters.simulations$id.simulation[[q]]
    one.sim$param.shape.step.length=parameters.simulations$shape.step.length[[q]]
    one.sim$param.scale.step.length=parameters.simulations$scale.step.length[[q]]
    one.sim$param.habitat.utilization=parameters.simulations$habitat.utilization[[q]]
    
    #ADDED THIS CODE
    one.sim$patch.size<-landscape.variables$patch_size
    one.sim$percent.forest<-landscape.variables$percent_forest
    one.sim$elevation<-landscape.variables$elevation
    one.sim$patch.id.adam <- landscape.variables$Patch
    
    
    
    #Store movment kernel
    movement.kernel <- movement_kernel(parameters.simulations$scale.step.length[[q]],
                                       parameters.simulations$shape.step.length[[q]], 
                                       map.cropped)
    
    #Store habitat kernel
    habitat.kernel <- habitat_kernel(list(forest27 = parameters.simulations$habitat.utilization[[q]]),
                                     map.cropped)
    
    
    #for (z in focal.patch){
    for (z in 1:length(focal.patch)){
      
      time.location.regurgitation[[q]]<<-as.list(focal.patch)
      #names(time.location.regurgitation[[q]][[z]])<<-focal.patch[[z]] #For now the names are in a character format
      time.location.regurgitation[[q]][[z]]<<-list()
      
      for (g in 1:num.sim.points.per.patch){
        
        #It will do a list every time a simulation is made in each patch
        time.location.regurgitation[[q]][[z]][[g]]<<-list()
        
        for (l in 1:num.sim.per.point){
          
          #Copies a base data frame with information that I need to do the simulation
          time.location.regurgitation[[q]][[z]][[g]][[l]] <<- one.sim
          
          #Adds the bird id
          time.location.regurgitation[[q]][[z]][[g]][[l]]$num.simulation<<-g
          
          #Add the id of the focal patch
          time.location.regurgitation[[q]][[z]][[g]][[l]]$focal.patch<<-focal.patch[z]
          
          #Add the id of the patch emigrated of the first row, which would be the same as the starting point
          time.location.regurgitation[[q]][[z]][[g]][[l]]$patch.emigrated<<-focal.patch[z]
          
          #IF I am starting with the first step, then it will generate a random starting point in the same patch. ELSE will run for the following simulations of movement behavior but it will not generate a new starting point since I want to simulate movement behavior on the same starting point.
          if (l==1){
            #Generate a random starting point.
            initial.starting.point<-data.frame(sampleStratified(map.cropped.labeled, 
                                                                1, xy=T))
            initial.starting.point<-subset(initial.starting.point,
                                           initial.starting.point$forest27==focal.patch[z])[,c("x","y")]
            
            #Put information on data frame of where the bird started its track and the raster value
            time.location.regurgitation[[q]][[z]][[g]][[l]][1,"x"]<<-initial.starting.point[1]
            time.location.regurgitation[[q]][[z]][[g]][[l]][1,"y"]<<-initial.starting.point[2]
            
            time.location.regurgitation[[q]][[z]][[g]][[l]][1,"raster.value"]<<-raster::extract(map.cropped,
                                                                                                cbind(initial.starting.point[1],initial.starting.point[2])) 
          }else{
            
            initial.starting.point[1]<<-time.location.regurgitation[[q]][[z]][[g]][[1]][1,"x"]
            initial.starting.point[2]<<-time.location.regurgitation[[q]][[z]][[g]][[1]][1,"y"]
            
            #Put information on data frame of where the bird started its track and the raster value
            time.location.regurgitation[[q]][[z]][[g]][[l]][1,"x"]<<-initial.starting.point[1]
            time.location.regurgitation[[q]][[z]][[g]][[l]][1,"y"]<<-initial.starting.point[2]
            
            time.location.regurgitation[[q]][[z]][[g]][[l]][1,"raster.value"]<<-raster::extract(map.cropped,
                                                                                                cbind(initial.starting.point[1],initial.starting.point[2])) 
            time.location.regurgitation[[q]][[z]][[g]][[l]]$patch.emigrated<<-time.location.regurgitation[[q]][[z]][[g]][[l]]$focal.patch[1]
          }
          
          
          
          for (k in 1:tot.num.steps){ #***CHECK IF  HAVE TO PUT 1 OR 2. I WOULD SAY 2 BUT WHEN I PUT IT IT DOES NOT CONSIDER THE 2 AND STARTS THE SEQUENCE AT 3 
            
            #tud <- wet_c #Copy the cropped raster map
            tud <- map.full #Use the full raster map!
            tud[] <- 0 #Putting 0's to map
        
            
            #Here is obtaining the stud kernel for a specific point. It simulates n steps (72 in this example) using the movement (mk) and the habitat (hk) kernels and starting in a specific point (as.numeric) and then estimats the utilization distribution (utilization distribution is a probability distribution giving the probability density that an animal is found at a given point in space. Therefore, in each simulation all the values sum to 1).
            
            #At the end of the simulation I store the next point in which the bird moved (starting.point). So here I am saying that IF I am simulating the first step, I will do the kernel from the initial.starting.point. ELSE: if I am doing the kernel for the following points after doing the kernel for the first point, I will use the starting.point that was selected at the end of this chunk
            if (k==1){
              repeat{
              system.time(for(i in 1:1e3) 
                tud <- tud +
                  simulate_ud(movement.kernel,habitat.kernel,initial.starting.point,n= tot.num.steps)) 
                #Since we did x simulations (in this case 5000) and the probability of each simulation summed to 1, all the cells now sum 5000. We have no normilize the information
                tud[] <- tud[] / sum(tud[]) 
                
                #We can use this command to see the histogram of probabilities. 
                #hist(tud.without.zeros,breaks=1000)
                
                #Sample from the probabilites in the tud kernel with weights equal to the probability values of each cell
                sampCellProb<-sample(x=1:length(tud[]),prob=tud[], size=1)
                
                #Obtain coordinates from sampled cell. ***IF THERE ARE MORE CELLS THAT HAVE THE SAME PROBABILITY, I WILL CHOOSE THE FIRST ROW. IS THAT THE WAY TO DO A RANDOM SAMPLE?
                new.coord<<-xyFromCell(tud, sampCellProb)[1,]
                
                test<-cellFromXY(land_use_raster_labeled,new.coord)
                
                if (is.na(land_use_raster_labeled[test])=="FALSE") {
                  break
                }
                
                }
                
                }else{
              repeat{
              system.time(for(i in 1:1e3) 
                tud <- tud +
                  simulate_ud(movement.kernel,habitat.kernel,starting.point,n= tot.num.steps)) 
            
            
            #Since we did x simulations (in this case 5000) and the probability of each simulation summed to 1, all the cells now sum 5000. We have no normilize the information
            tud[] <- tud[] / sum(tud[]) 
            
            #We can use this command to see the histogram of probabilities. 
            #hist(tud.without.zeros,breaks=1000)
            
            #Sample from the probabilites in the tud kernel with weights equal to the probability values of each cell
            sampCellProb<-sample(x=1:length(tud[]),prob=tud[], size=1)
            
            #Obtain coordinates from sampled cell. ***IF THERE ARE MORE CELLS THAT HAVE THE SAME PROBABILITY, I WILL CHOOSE THE FIRST ROW. IS THAT THE WAY TO DO A RANDOM SAMPLE?
            new.coord<<-xyFromCell(tud, sampCellProb)[1,]
            
            test<-cellFromXY(land_use_raster_labeled,new.coord)
            
            if (is.na(land_use_raster_labeled[test])=="FALSE") {
              break
            }
              }
                }
              
            
            #TEST. data frame with various cells** I NEED TO PUT TO WORK THIS FUNCTION SINCE IT PUT ME AN ERROR THAT THERE ARE MANY LOCATIONS WITH THE SAME PROBABILITY
            #ifelse(isTRUE(nrow(new.coord)==1)=="TRUE", new.coord,subset(new.coord,new.coord[,"x"]==sample(new.coord[,"x"])[1]))
            
            #Save spatial point in a data frame
            time.location.regurgitation[[q]][[z]][[g]][[l]][k+1,"x"]<<-new.coord[1]
            time.location.regurgitation[[q]][[z]][[g]][[l]][k+1,"y"]<<-new.coord[2]
            time.location.regurgitation[[q]][[z]][[g]][[l]][k+1,"raster.value"]<<-raster::extract(map.cropped,
                                                                                                cbind(new.coord[1],new.coord[2])) 
            
            #Obtain the cell number from the coordinates where the bird went (new coordinate)
            regurgitation.xy<-cellFromXY(map.cropped.labeled,new.coord)
            
            #}
            
            #Obtain the patch id where the seed was deposited
            time.location.regurgitation[[q]][[z]][[g]][[l]][k+1,"patch.emigrated"]<<-map.cropped.labeled[regurgitation.xy]
    
            
            #If patch where it emigrated is equal to patch where it landed, it stayed on the same patch
            time.location.regurgitation[[q]][[z]][[g]][[l]]$same.patch<<-ifelse(time.location.regurgitation[[q]][[z]][[g]][[l]]$patch.emigrated==time.location.regurgitation[[q]][[z]][[g]][[l]]$focal.patch,1,0)
            
            #If patch where it emigrated is equal to 0 (matrix), then the seed is lost
            time.location.regurgitation[[q]][[z]][[g]][[l]]$matrix<<-ifelse(time.location.regurgitation[[q]][[z]][[g]][[l]]$patch.emigrated==0,1,0)
            
            #If patch where it emigrated is different that where it originated, it emigrated
            time.location.regurgitation[[q]][[z]][[g]][[l]]$new.patch<<-ifelse(time.location.regurgitation[[q]][[z]][[g]][[l]]$patch.emigrated>0 & time.location.regurgitation[[q]][[z]][[g]][[l]]$patch.emigrated!=time.location.regurgitation[[q]][[z]][[g]][[l]]$focal.patch,1,0)
            
            #This two lines of codes are specifically for immigration. 
            #If there is a value for focal.patch.immigration, then it will create a column (immigrated) which will have a 1 if the seed landed in the focal patch
            #If no value is inputed, then it will not generate the column
            if(is.na(focal.patch.immigration)==FALSE){
              #If the patch where the seed lands is the focal patch, then it immigrated to the focal patch
              time.location.regurgitation[[q]][[z]][[g]][[l]]$immigrated<<-ifelse(time.location.regurgitation[[q]][[z]][[g]][[l]]$patch.emigrated==focal.patch.immigration,1,0)
              
              #Here I am correcting for the fact that if it landed in the focal patch, then it will register too that it landed in a new patch. 
              #So I am stating that if it immigrated then put a 0 on new patch
              time.location.regurgitation[[q]][[z]][[g]][[l]]$new.patch<<-ifelse(time.location.regurgitation[[q]][[z]][[g]][[l]]$immigrated==1,0,time.location.regurgitation[[q]][[z]][[g]][[l]]$new.patch)
              
              
            }
            
            #time.location.regurgitation[[z]][[g]][[l]]$seed.faith<-ifelse(time.location.regurgitation[[z]][[g]][[l]]$patch.emigrated==time.location.regurgitation[[z]][[g]][[l]]$focal.patch,"stayed",
            #ifelse(time.location.regurgitation[[z]][[g]][[l]]$patch.emigrated==0,"died","emigrated"))
            
            
            #Convert new.coord to numeric so that it can be used again in the simulation
            starting.point<-new.coord
            #as.numeric(xyFromCell(tud, cell.sampled))
            
            match(z,focal.patch)
            
            
            
            
            #Give the name of the data frame to the name that I stated on the function
            assign(deparse(substitute(name.output.list)),time.location.regurgitation, envir=.GlobalEnv)
            
            
            #Print information
            #print(paste((sum(1:z)*g*l*k),"th simulation of",length(focal.patch)*length(num.sim.points.per.patch)*length(num.sim.per.point)*length(tot.num.steps),"simulations"))
            print(paste("Patch",z,"of",length(focal.patch),"patches. Patch id analyzed now:",focal.patch[z]))
            #print(paste(q,"th parameter simulation of",nrow(parameters.simulations.model,"parameters")))
            print(paste("Point",g,"of",num.sim.points.per.patch,"points per patch"))
            #print(paste("Point",g,"of",num.sim.points.per.patch,"starting points per patch"))
            print(paste("Point",l,"of",num.sim.per.point,"simulations in the same starting point"))
            print(paste("Step",k,"of",tot.num.steps,"steps"))
          }}}}}
  
  time.location.regurgitation
  
}




####FUNCTION FOR OBTAINING THE MAP FOR THE SIMULATIONS MADE####
#This functions needs the following information
#focal.patch: the patches where the information of emigration/immigration will be obtained
#num.sim.points.per.patch: number of simulations that will be made starting from each focal patch
#num.sim.per.point: number of simulations that will be made per starting point
#dataframe.results: the name of the data frame that has the results from the movement behavior simulation
#offset: how much more the map needs to be expanded so that all points fall on the map 
#map.full: full map with vegetation cover

map.simulations<-function(focal.patch=NA,
                          num.sim.points.per.patch=NA,
                          num.sim.per.point=NA,
                          dataframe.results=NA,
                          offset=NA,
                          map.full=NA){
#I was trying to get the range of values of x and y coordinates but I could not do it. So I increased the offset so that I could plot all the points in the map.
for (z in 1:length(focal.patch)){
  
  for (g in 1:num.sim.points.per.patch){
    
    for(l in 1:num.sim.per.point) {
      range.x<-range(dataframe.results[[z]][[g]][[l]]$x)
      range.x<-range(range.x)
      #print(range.x)
      
    }
  }
}


for (z in 1:length(focal.patch)){
  
  for (g in 1:num.sim.points.per.patch){
    
    for(l in 1:num.sim.per.point) {
      range.y<-range(dataframe.results[[z]][[g]][[l]]$y)
      range.y<-range.y
      #print(range.y)
    }
  }
}

#range.x <- range(unlist(lapply(time.location.regurgitation, function(m) range(m$"x"))))
#range.y <- range(unlist(lapply(time.location.regurgitation, function(m) range(m$"y"))))
Extent <- t(cbind(range.x, range.y))
Extent <- Extent + rep(c(-offset,offset),each=2)

#jpeg(paste("Graphs/emigration bird ",id.freq,".jpg",sep=""),quality=100,height=800,width=800)

#jpeg("Graphs/emigration bird id 851.jpg",quality=100,height=500,width=500)

plot(crop(land_use,extent(Extent)), xlim=Extent[1,], ylim=Extent[2,],cex=1.5)

for (z in 1:length(focal.patch)){
  for (g in 1:num.sim.points.per.patch){
    
    for(l in 1:num.sim.per.point)
    {
      #Plot points where the animal moved
      points(dataframe.results[[z]][[g]][[l]]$x,dataframe.results[[z]][[g]][[l]]$y,
             pch=19,cex=0.5, type="b")
      lines(dataframe.results[[z]][[g]][[l]]$x,dataframe.results[[z]][[g]][[l]]$y,
            col=dataframe.results[[z]][[g]][[l]]$num.simulation,lwd=2)
      
      #Ploints plot in yellow where the animal started and in red where the animal stopped
      points(dataframe.results[[z]][[g]][[l]]$x[c(1,length(dataframe.results[[z]][[g]][[l]]$x))],
             dataframe.results[[z]][[g]][[l]]$y[c(1,length(dataframe.results[[z]][[g]][[l]]$y))],
             pch=19,col=c("coral","red"))
      
    }
  }
}

#legend("topright", legend=c("Starting point", "Ending point"),
       #col=c("coral", "red"), cex=1.5,pch=16)

#dev.off()

}

####FUNCTION FOR OBTAINING INDIVIDUAL MAPS FOR THE SIMULATIONS MADE####
#This functions needs the following information
#focal.patch: the patches where the information of emigration/immigration will be obtained
#num.sim.points.per.patch: number of simulations that will be made starting from each focal patch
#num.map.start=number of the first map that will be plotted
#num.map.end=number of the last map that will be plotted
#num.sim.per.point: number of simulations that will be made per starting point
#dataframe.results: the name of the data frame that has the results from the movement behavior simulation
#offset: how much more the map needs to be expanded so that all points fall on the map 
#map.full: full map with vegetation cover

individual.map.simulations<-function(focal.patch=NA,
                          num.sim.points.per.patch=NA,
                          num.sim.per.point=NA,
                          num.map.start=1,
                          num.map.end=1,
                          dataframe.results=NA,
                          offset=NA,
                          map.full=NA){
  #I was trying to get the range of values of x and y coordinates but I could not do it. So I increased the offset so that I could plot all the points in the map.
  for (z in 1:length(focal.patch)){
    
    for (g in 1:num.sim.points.per.patch){
      
      for(l in 1:num.sim.per.point) {
        range.x<-range(dataframe.results[[z]][[g]][[l]]$x)
        range.x<-range(range.x)
        #print(range.x)
        
      }
    }
  }
  
  
  for (z in 1:length(focal.patch)){
    
    for (g in 1:num.sim.points.per.patch){
      
      for(l in 1:num.sim.per.point) {
        range.y<-range(dataframe.results[[z]][[g]][[l]]$y)
        range.y<-range.y
        #print(range.y)
      }
    }
  }
  
  #range.x <- range(unlist(lapply(time.location.regurgitation, function(m) range(m$"x"))))
  #range.y <- range(unlist(lapply(time.location.regurgitation, function(m) range(m$"y"))))
  Extent <- t(cbind(range.x, range.y))
  Extent <- Extent + rep(c(-offset,offset),each=2)
  
  #jpeg(paste("Graphs/emigration bird ",id.freq,".jpg",sep=""),quality=100,height=800,width=800)
  
  #jpeg("Graphs/emigration bird id 851.jpg",quality=100,height=500,width=500)
  
  #If I want to overlay each simulation in a single map, then run this line outside the function
  #plot(crop(land_use,extent(Extent)), xlim=Extent[1,], ylim=Extent[2,],cex=1.5)
  
  for (z in 1:length(focal.patch)){
    for (g in num.map.start:num.map.end){
      
      for(l in 1:num.sim.per.point)
      {
        
        #If I want each simulation in a different map, then run this line inside the function
        plot(crop(land_use,extent(Extent)), xlim=Extent[1,], ylim=Extent[2,],cex=1.5)
        
        #Plot points where the animal moved
        points(dataframe.results[[z]][[g]][[l]]$x,dataframe.results[[z]][[g]][[l]]$y,
               pch=19,cex=0.5, type="b")
        lines(dataframe.results[[z]][[g]][[l]]$x,dataframe.results[[z]][[g]][[l]]$y,
              col=dataframe.results[[z]][[g]][[l]]$num.simulation,lwd=2)
        
        #Ploints plot in yellow where the animal started and in red where the animal stopped
        points(dataframe.results[[z]][[g]][[l]]$x[c(1,length(dataframe.results[[z]][[g]][[l]]$x))],
               dataframe.results[[z]][[g]][[l]]$y[c(1,length(dataframe.results[[z]][[g]][[l]]$y))],
               pch=19,col=c("coral","red"))
        
        #Plot points at 20, 40, 60 minutes. If it landed in a patch plot it blue and if it landed in the matrix plot it red
        points(dataframe.results[[z]][[g]][[l]]$x[c(5,9,length(dataframe.results[[z]][[g]][[l]]$x))],
               dataframe.results[[z]][[g]][[l]]$y[c(5,9,length(dataframe.results[[z]][[g]][[l]]$y))],
               pch=19,col=c(ifelse(dataframe.results[[z]][[g]][[l]][5,"matrix"]==1,"red","blue"),
                            ifelse(dataframe.results[[z]][[g]][[l]][9,"matrix"]==1,"red","blue"),
                            ifelse(dataframe.results[[z]][[g]][[l]][length(dataframe.results[[z]][[g]][[l]]$x),"matrix"]==1,"red","blue")))
        
        scalebar(1000,type="bar",below="Meters")
      }
    }
  }
  
  #legend("topright", legend=c("Starting point", "Ending point"),
  #col=c("coral", "red"), cex=1.5,pch=16)
  
  #dev.off()
  
}


####FUNCTION FOR FORMATTING THE LIST INTO A DATA BASE####
raw.dataframe<-function(for.immigration="N",
                        focal.patch=NA,
                        name.input.database=NA,
                        name.output.dataframe=NA){
  
  #Create a data frame with 1 row and the number of columns so that it matches the data frame 
  #I generated for each simulation (in this case is called results.simulations2). I am using the
  #first element of the list as a molde
  database.simulations<-data.frame(matrix(0,
                                          ncol=ncol(name.input.database[[1]][[1]][[1]][[1]]),
                                          nrow=1)) 
  
  #Name columns
  names(database.simulations)<-names(name.input.database[[1]][[1]][[1]][[1]])
  
  #If the function is used for immigration, then it will add a column named immigrated
  if(for.immigration=="Y"){
    database.simulations$immigrated<-0
  }


  #Paste all data bases
  for (z in 1:length(name.input.database)){
    
    for(a in 1:length(name.input.database[[z]])){
      
      for (g in 1:num.sim.points.per.patch){
        
        for(l in 1:num.sim.per.point)
      {
        
        df<-name.input.database[[z]][[a]][[g]][[l]]
        
        database.simulations<-rbind(df,database.simulations)
        
      }
    }
  }
  }
  
  
  #Delete last observation which has nothing
  database.simulations<-database.simulations[-nrow(database.simulations),]
  
  #Assign the data frame the name of the data frame I put at the beginning of the function
  assign(deparse(substitute(name.output.dataframe)),
         results.simulations, envir=.GlobalEnv)
  
  #write.csv(results.emigration,"borrar.csv",row.names=F)
  #write.csv(results.simulations,"immigration results focal patch 13 freq bird 851.csv",row.names=F)
}

####FUNCTION FOR OBTAINING THE FORMATED DATA BASE####
#These function needs the following information:
#name.input.database: name of the data base that has the information from the movement behavior simulation
#name.output.dataframe: name for the generated data base
final.database<-function(for.immigration="N",
                         focal.patch=NA,
                         times=unique(name.input.database$time),
                         name.input.database=NA,
                         name.output.dataframe=NA){

#Formating data base  
  results.simulations <- name.input.database %>% 
    select(-raster.value) %>% 
    gather(fate,yes.no,same.patch:new.patch) %>% #Convert from wide to long
    group_by(focal.patch,patch.id.adam,param.shape.step.length,param.scale.step.length,param.habitat.utilization,time,fate) %>% 
    summarise(count=sum(yes.no)) %>% #Count number of fruits thtat landed in the matrix, same patch or new patch
    ungroup() %>% 
    spread(fate, count) %>%  #Convert from long to wide
    inner_join(name.input.database %>% 
                 select(focal.patch,patch.size,percent.forest,elevation,patch.id.adam) %>% 
                 distinct(),
               by=c("focal.patch","patch.id.adam"))
  
  #If the function is used for the immigration part, then it has to consider the immigration column generated
  if(for.immigration=="Y"){
    #Obtain the proportion of seeds that emigrated for each specific column
    results.simulations<-cbind(results.simulations,
                               prop.table(as.matrix(results.simulations[,c("same.patch","matrix","new.patch","immigrated")]),1) %>% 
                                 set_colnames(c("prop.same.patch","prop.matrix","prop.new.patch"))) #rename columns
  }
  
  #If the function is used for emmigration, then the immigration column is not generated so it is not considered in the code
  else{
    
    #Obtain the proportion of seeds that emigrated for each specific column
    results.simulations<-cbind(results.simulations,
                               prop.table(as.matrix(results.simulations[,c("same.patch","matrix","new.patch")]),1) %>% 
                                 set_colnames(c("prop.same.patch","prop.matrix","prop.new.patch")))  #rename columns

  }
  
  #Assign the data frame the name of the data frame I put at the beginning of the function
  assign(deparse(substitute(name.output.dataframe)),
         results.simulations, envir=.GlobalEnv)
  
  #write.csv(results.emigration,"borrar.csv",row.names=F)
  #write.csv(results.simulations,"immigration results focal patch 13 freq bird 851.csv",row.names=F)
  
}

####FUNCTION FOR OBTAINING THE DISPERSAL DISTANCE####
dispersal.distance<-function(focal.patch=NA,
                             name.input.database=NA){
  
  df<-name.input.database
  
  for (z in 1:length(name.input.database)){
    
    for(a in 1:length(name.input.database[[z]])){
      
      for (g in 1:length(name.input.database[[z]][[a]])){
        
        for (l in 1:length(name.input.database[[z]][[a]][[g]]))
          
          for (q in 1:length(name.input.database[[z]][[a]][[g]][[l]]))
      {
        #Create a column for the dispersal distance
        #df[[z]][[g]][[l]]$disperal.distance<-0
        
        for (k in 2:nrow(df[[z]][[a]][[g]][[l]][[q]]))
        #It will use the first xy values row and obtain the distance in the x y values of the n row of the data frame
          df[[z]][[a]][[g]][[l]][[q]][k,"dispersal.distance"]<-pointDistance(df[[z]][[a]][[g]][[l]][[q]][1,c("x","y")], #Always select the first row since it is the starting point
                                                                   df[[z]][[a]][[g]][[l]][[q]][k,c("x","y")], #Select row k
                                                                   lonlat=F)
        df[[z]][[a]][[g]][[l]][[q]][1,"dispersal.distance"]<-0
        
        #Assign the data frame the name of the data frame I put at the beginning of the function
        assign(deparse(substitute(name.input.database)),
               df, envir=.GlobalEnv)
      }
    }
  }
  }
  
}
