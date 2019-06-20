prop.forest.30.500<-function(){
  # loop through every row in prueba and calculate prop.for30 and prop.for500
for (i in c(1:nrow(utm_formated))) {
  
  cen <- utm_formated[i,3:4]
  
  # Identify all cells that lie within buffer around site i:
  Buffer.cells.30 <- raster::extract(forest, cen, cellnumbers=TRUE, 
                          buffer=30)[[1]][,1]
  Buffer.cells.500 <- raster::extract(forest, cen, cellnumbers=TRUE, 
                             buffer=500)[[1]][,1]

  # Copy land cover map and delete all values outside of buffer:
  Buffer.forest.30 <- Buffer.forest.500 <- forest==2
  values(Buffer.forest.30)[-Buffer.cells.30] <- NA
  values(Buffer.forest.500)[-Buffer.cells.500] <- NA
  sample.prop.for30 <- mean(values(Buffer.forest.30), na.rm=TRUE)  
  sample.prop.for500 <- mean(values(Buffer.forest.500), na.rm=TRUE)
    
  # # Copy land cover map and delete all values outside of buffer:
  # Buffer.forest.30 <- Buffer.forest.500 <- forest
  # values(Buffer.forest.30)[-Buffer.cells.30] <- NA
  # values(Buffer.forest.500)[-Buffer.cells.500] <- NA
  # 
   # just checking! Better: export as jpeg (or png), add points.
   #plot(Buffer.forest.500, ext=unlist(c(cen[1]-500, cen[1]+500,
                                    #cen[2]-500, cen[2]+500)))
  # 
  # # Calculate class-level metrics for cells within buffer:
  # ClassStat.30 <- ClassStat(Buffer.forest.30,cellsize=10)
  # ClassStat.500 <- ClassStat(Buffer.forest.500,cellsize=10)
  # 
  # Class <- data.frame(Class.ID=c(1,2))
  # Result.30 <- merge(Class, ClassStat.30, 
  #                 all=TRUE, by.x="Class.ID", by.y="class")
  # Result.30[is.na(Result.30)] <- 0
  # Result.500 <- merge(Class, ClassStat.500, 
  #                    all=TRUE, by.x="Class.ID", by.y="class")
  # Result.500[is.na(Result.500)] <- 0
  # 
  # sample.prop.for30 <- Result.30[2,4]  
  # sample.prop.for500 <- Result.500[2,4]
  
    
  # fill empty matrix for row i
  points.info[i,3] <<- utm_formated[i,1]
  points.info[i,4] <<- utm_formated[i,2]
  points.info[i,5] <<- sample.prop.for30
  points.info[i,6] <<- sample.prop.for500
  
  print(paste(i," of ",nrow(points.info),": ", 
              round(sample.prop.for30,3), ", ",
              round(sample.prop.for500,3), sep=""))
}
}