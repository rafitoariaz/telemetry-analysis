clean.tel.data<-function (){
  
  
  #Load file. 
  prueba<<-read.xlsx("Data bases/Datos telemetria Julio Agosto 2016 2.xlsx",sheet=1)
  
  #See which frequencies I have
  table(prueba$FREQ)
  #Subset if there are NA and convert into numeric string the locations and orientation
  #NA from location.x appear because they registered the time at which they started to track the bird
  #but could not find it until hours later, so the next row after it is the one were they located
  #the bird. So it is ok to take these observations.
  prueba<<-subset(prueba,is.na(prueba$LOCATION.X)=="FALSE")
  prueba$LOCATION.X<<-as.numeric(as.character(prueba$LOCATION.X))
  
  #Because I made a subset on location.x, there are no rows without informaion now, but still do the
  #process just in case
  prueba<<-subset(prueba,is.na(prueba$LOCATION.Y)=="FALSE")
  prueba$LOCATION.Y<<-as.numeric(as.character(prueba$LOCATION.Y))
  
  #Here two rows (282 and 1059) have NA. The first one they were looking for the bird. In the
  # second one they stopped following bird because of the rain
  prueba<<-subset(prueba,is.na(prueba$ORIENT)=="FALSE")
  prueba$ORIENT<<-as.numeric(as.character(prueba$ORIENT))
  
  #Displace the points using bearings and distance where the point was taken. Adam gave me this formula
  prueba$LOCATION.X<<-(cos(NISTdegTOradian(prueba$ORIENT))*prueba$DIST)+prueba$LOCATION.X
  prueba$LOCATION.Y<<-(sin(NISTdegTOradian(prueba$ORIENT))*prueba$DIST)+prueba$LOCATION.Y
  
  #Set date format. Origin according to excel criterias from windows. I had to modify manually the dates. I took the 
  #Datos telemetria Julio Agosto 2016 file and copied the dates so that the column had the same format and then
  #modified the dates manually
  prueba$DATE<<-as.Date(prueba$DATE,origin="1899-12-30")
  #Deleted original Dates=DATE2
  prueba<<-prueba[,-3]
  
  #Format time. Since R reads time as numeric, multiply it by number of seconds in a day (60*60*24=8640).
  #First convert into numeric
  prueba$START<<-as.numeric(as.character(prueba$START))
  prueba$START<<-format(as.POSIXct((prueba$START) * 86400, origin = "1970-01-01", tz = "CST"), "%H:%M")
  
  prueba$STOP<<-as.numeric(as.character(prueba$STOP))
  prueba$STOP<<-format(as.POSIXct((prueba$STOP) * 86400, origin = "1970-01-01", tz = "CST"), "%H:%M")
  
  prueba$TIME<<-as.numeric(as.character(prueba$TIME))
  prueba$TIME<<-format(as.POSIXct((prueba$TIME) * 86400, origin = "1970-01-01", tz = "CST"), "%H:%M")
  
  prueba$MOVE_TIME<<-as.numeric(as.character(prueba$MOVE_TIME))
  prueba$MOVE_TIME<<-format(as.POSIXct((prueba$MOVE_TIME) * 86400, origin = "1970-01-01", tz = "CST"), "%H:%M")
  
  #Does not want to convert to POIXct object, so we do another converson
  prueba$START<<-as.POSIXct(strptime(paste(prueba$DATE,prueba$START,sep=" "),format="%Y-%m-%d %H:%M"),tz="CST")
  prueba$STOP<<-as.POSIXct(strptime(paste(prueba$DATE,prueba$STOP,sep=" "),format="%Y-%m-%d %H:%M"),tz="CST")
  prueba$TIME<<-as.POSIXct(strptime(paste(prueba$DATE,prueba$TIME,sep=" "),format="%Y-%m-%d %H:%M"),tz="CST")
  prueba$MOVE_TIME<<-as.POSIXct(strptime(paste(prueba$DATE,prueba$MOVE_TIME,sep=" "),format="%Y-%m-%d %H:%M"),tz="CST")
  
  #Verify dates in data base. DATES MUST BE BETWEEN JULY 9 AND SEPTEMBER 12 2016
  #table(substr(prueba$START,1,10))
  #table(substr(prueba$STOP,1,10))
  #table(substr(prueba$TIME,1,10))
  #table(substr(prueba$MOVE_TIME,1,10))
  
  #Verify times in data base. TIMES MUST BE BETWEEN 7:00 AND 13:00. Had to change manually some hours
  #that were not in the correct format
  #table(substr(prueba$START,12,16))
  #table(substr(prueba$STOP,12,16))
  
  #For TIME there is a time which is 0:00 but I cannot find the original notebook to modify this mistake.
  #I can take it out since the last observation was taken in the same location, so this point might
  #be a confirmation that the bird is still in the same place
  #table(substr(prueba$TIME,12,16))
  
  #Here in some rows they put in MOVE_TIME the number of minutes spend in a location. For example, 
  #if they located the bird at 10:00, they put 20 because it stayed for 20 minutes, but they should
  #have put 10:20 instead. I modified it manually
  #table(substr(prueba$MOVE_TIME,12,16))
  
  #Check concordance between times in which observation was made and times in which the bird was followed
  #subset(prueba,prueba$START-prueba$STOP>0)[,c(1:6,9,12)] #VERIFY STOP TIME IS NOT BEFORE START TIME
  
  #VERIFY THAT OBSERVATION ARE MADE AFTER START TIME. In this data, the observer located a bird
  #before the starting time, so I modify it manualy by setting the time of the first 
  #observtion to the start time. In all rows the observer was MAP
  #FREQ      #DATE
  #232       2016-07-12
  #312       2016-07-13
  #352       2016-07-22
  #672       2016-08-18
  #731       2016-09-02
  #subset(prueba,prueba$START-prueba$TIME>0)[,c(1:6,8,12)] 
  #subset(prueba,prueba$START-prueba$MOVE_TIME>0)[,c(1:6,8,12)] #VERIFY THAT OBSERVATION ARE MADE AFTER START TIME# CHECK
  #subset(prueba,prueba$STOP-prueba$TIME<0)[,c(1,2,4,5,8,9,11)] #VERIFY THAT OBSERVATION ARE MADE BEFORE STOP TIME
  
  #VERIFY THAT OBSERVATION ARE MADE BEFORE STOP TIME. Changed MAP FREQ 821 DATE 2016-07-11
  #because they finished tracking the bird at 12:00 but put in stop time 11:30
  subset(prueba,prueba$STOP-prueba$MOVE_TIME<0)[,c(1:6,9,12)] 
  #Check how to do it
  #subset(prueba,prueba$TIME-prueba$MOVE_TIME<0)[,c(1:6,9,12)] #VERIFY THAT MOVE TIME BETWEEN ACTUAL OBSERVATION AND NEXT
  #OBSERVATION DO NOT OVERLAP
  
  
}





#Function for cleaning the frequencies
clean.freq<-function(){
  
  
  #####################################
  #FREQUENCY 821
  ####################################
  
  #Specify frequency
  frequency<-821
  
  #Subset specific frequency
  subset_freq<-subset(prueba,prueba$FREQ==frequency) #821
  
  #Look at the subset for anormalities ###There is a problem here because this bird was not followed in 2016-07-14 
  #summary(subset_freq)
  
  #Look at dates in which telemetry was made by individual
  #table(subset_freq$OBSERVER,subset_freq$DATE)
  
  #There are duplicated points, so I will take them out manually. Mainly the bird
  #stayed in the same place but they took GPS point and wrote it down to double check
  #that it was in the same place. The next line indicates which rows are duplicated
  #subset_821[c("951", "952","953","954","955","956","962","963","964","965"),]
  #I am taking out duplicated rows
  subset_freq<-subset_freq[c(-37,-38,-40,-41,-48,-50),]
  
  
  #There are duplicated times and I checked the original data base and instead of saying 12:05 in the R data base it says 12:04, so that's why it's duplicated. I will change the value here
  subset_freq[33,8]=as.POSIXct("2016-07-14 12:05:00",tz="CST")
  
  #Save this new data frame without errors 
  db.freq.clean<-subset_freq
  
  
  
  #####################################
  #FREQUENCY 232
  ####################################
  
  #Specify frequency
  frequency<-232
  
  #Subset specific frequency
  subset_freq<-subset(prueba,prueba$FREQ==frequency) 
  
  #Look at the subset for anormalities ###There is a problem here because this bird was not followed in 2016-07-14 
  #summary(subset_freq)
  
  #This row is duplicated but in time (9:40) but in the original spread sheet it has another time (9:41). So I am changing manually in R the time
  subset_freq[21,8]<-as.POSIXct("2016-07-13 09:41:00",tz="CST")
  
  
  #Look at dates in which telemetry was made by individual
  #table(subset_freq$OBSERVER,subset_freq$DATE) #I see that the telemetry data that I took is not complete. Check it when I have time
  
  #Save this new data frame without errors 
  db.freq.clean<-rbind(db.freq.clean,subset_freq)
  
  
  
  
  #####################################
  #FREQUENCY 851
  ####################################
  
  #Specify frequency
  frequency<-851
  
  #Subset specific frequency
  subset_freq<-subset(prueba,prueba$FREQ==frequency) 
  
  #Look at the subset for anormalities ###There is a problem here because this bird was not followed in 2016-07-14 
  #summary(subset_freq)
  
  #This row is duplicated but in time (9:40) but in the original spread sheet it has another time (9:41). So I am changing manually in R the time
  subset_freq[17,8]<-as.POSIXct("2016-07-13 09:41:00",tz="CST")
  
  
  #Look at dates in which telemetry was made by individual
  #table(subset_freq$OBSERVER,subset_freq$DATE) #I see that the telemetry data that I took is not complete. Check it when I have time
  
  #Save this new data frame without errors 
  db.freq.clean<-rbind(db.freq.clean,subset_freq)
  
  
  
  #####################################
  #FREQUENCY 171
  ####################################
  
  #Specify frequency
  frequency<-171
  
  #Subset specific frequency
  subset_freq<-subset(prueba,prueba$FREQ==frequency) 
  
  #Look at the subset for anormalities ###There is a problem here because this bird was not followed in 2016-07-14 
  #summary(subset_freq)
  
  #In the original data set, there are duplicated data points. I will remove one of them since I think they are in the same place but they are double checking
  subset_freq<-subset_freq[-2,]
  #There are also typing errors when in Location.y. They put 282xxx and should be 967xxx. I changed them manually
  
  #Save this new data frame without errors 
  db.freq.clean<-rbind(db.freq.clean,subset_freq)
  
  
  
  #####################################
  #FREQUENCY 392
  ####################################
  
  #Specify frequency
  frequency<-392
  
  #Subset specific frequency
  subset_freq<-subset(prueba,prueba$FREQ==frequency) 
  
  #Look at the subset for anormalities ###There is a problem here because this bird was not followed in 2016-07-14 
  #summary(subset_freq)
  
  #Time repeated but in the original data base they are different by 1 minute. I will change them
  subset_freq[5,8]<-as.POSIXct("2016-07-25 09:59:00",tz="CST")
  
  #Save this new data frame without errors 
  db.freq.clean<-rbind(db.freq.clean,subset_freq)
  
  
  
  
  #####################################
  #FREQUENCY 973
  ####################################
  #There seems to be an outlier on location.y, so hulls are not generated very well
  
  #Specify frequency
  frequency<-973
  
  #Subset specific frequency
  subset_freq<-subset(prueba,prueba$FREQ==frequency) 
  
  #Look at the subset for anormalities ###There is a problem here because this bird was not followed in 2016-07-14 
  #summary(subset_freq)
  
  #This radio tag was first used in patch 32 and re-used in patch 34 since it was eaten by a hawk in patch 32. I am subseting the information from patch 34
  subset_freq$month<-substr(subset_freq$TIME,6,7)
  subset_freq<-subset(subset_freq,subset_freq$month=="08")
  subset_freq$month<-NULL
  
  #It also had location.y values wrong. I manually changed 288922 and 288933 to 968922 and 968933. Also I manually changed 288962 to 988962
  #Location.y values too extremes. Even though they are written like that in the notebook, I changed manully 988962 for 968962
  
  
  #Save this new data frame without errors 
  db.freq.clean<-rbind(db.freq.clean,subset_freq)
  
  
  
  #####################################
  #FREQUENCY 881
  ####################################
  #Real outliers but might distort home range. I followed the bird with Michael
  
  #Specify frequency
  frequency<-881
  
  #Subset specific frequency
  subset_freq<-subset(prueba,prueba$FREQ==frequency) 
  
  #Look at the subset for anormalities ###There is a problem here because this bird was not followed in 2016-07-14 
  #summary(subset_freq)
  
  #Location.y had value of 97273 and I changed it manually to 972273. I also changed manually x locations that were wrongly written (274714 I changed it to 279714 between 8:38 and 9:15) and 274764 for 279764
  #I will delete dubplicated locations temporary, but I have to check them in the second round because this bird was followed by two people, so that is why there are true duplicated times
  subset_freq$take_out<-0
  
  
  subset_freq$take_out<-ifelse(subset_freq$ORIENT==80 & subset_freq$DIST==50,1,subset_freq$take_out)
  subset_freq$take_out<-ifelse(subset_freq$ORIENT==32 & subset_freq$DIST==70,1,subset_freq$take_out)
  subset_freq$take_out<-ifelse(subset_freq$ORIENT==20 & subset_freq$DIST==200,1,subset_freq$take_out)
  subset_freq$take_out<-ifelse(subset_freq$ORIENT==54 & subset_freq$DIST==250,1,subset_freq$take_out)
  
  subset_freq<-subset(subset_freq,subset_freq$take_out==0)
  
  subset_freq$take_out<-NULL
  
  
  
  #Save this new data frame without errors 
  db.freq.clean<-rbind(db.freq.clean,subset_freq)
  
  
  
  #####################################
  #FREQUENCY 672
  ####################################
  #Problem time stamps
  
  #Specify frequency
  frequency<-672
  
  #Subset specific frequency
  subset_freq<-subset(prueba,prueba$FREQ==frequency) 
  
  #Look at the subset for anormalities ###There is a problem here because this bird was not followed in 2016-07-14 
  #summary(subset_freq)
  
  #There are 3 true outliers that might distort the home range. I will take them out
  subset_freq<-subset(subset_freq,subset_freq$LOCATION.X<287000)
  
  #Save this new data frame without errors 
  db.freq.clean<-rbind(db.freq.clean,subset_freq)
  
  
  table(prueba$FREQ)
  
  ####Take out from the original data set the frequencies that I cleaned so that then I can add the cleaned frequencies
  
  for (i in unique(db.freq.clean$FREQ)){
    
    prueba<<-subset(prueba,prueba$FREQ!=i)
    
  }
  
  #Rbind data bases to have a clean one
  prueba<<-rbind(prueba,db.freq.clean)
  
  table(prueba$FREQ)
  
}