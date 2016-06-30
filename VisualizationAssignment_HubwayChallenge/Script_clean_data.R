#------Read Hubway_stations.csv file-------------
stations<- read.csv(file.choose(),header = T)
stations_dframe<-data.frame(stations)

#---------Read Hubway_Trips.csv file----------
trips<- read.csv(file.choose(),header = T)
trips_dframe<-data.frame(trips)

#-------Merge data from two files-----------
merged.data <- merge(trips_dframe, stations_dframe, by.x="strt_statn", by.y="id")
merged.data_final <- merge( merged.data, stations_dframe, by.x="end_statn", by.y="id")

#-------Create data frame------------
dframe<-data.frame(merged.data_final)
library(lubridate)

#------Convert date into specific format--------------
start_date <- mdy_hms(as.character(dframe$start_date)) 
end_date<- mdy_hms(as.character(dframe$end_date)) 

#------difference of start and end date-------
diff <- as.numeric(end_date-start_date)

#----------bind dframe and diff together--------
dbframe<-cbind(dframe,diff)

#-------Delete the rows with duration less than or equal to zero
dframe<-dbframe[!(dbframe$diff<=0),]

#-----Create final data frame------------
finaldf<-data.frame(Seq_ID = dframe$seq_id,Hubway_id=dframe$hubway_id,Bicycle_id=dframe$bike_nr,Start_Date=dframe$start_date,End_Date=dframe$end_date,Duration=dframe$diff,Start_Stn_id=dframe$strt_statn,Start_stn_Name=dframe$station.x,Start_stn_status=dframe$status.x,Start_stn_lat=dframe$lat.x,Start_stn_lon=dframe$lng.x,Stn_City=dframe$municipal.x,End_stn_id=dframe$end_statn,End_stn_name=dframe$station.y,End_Station_status=dframe$status.y,End_stn_lat=dframe$lat.y,End_stn_lon=dframe$lng.y,Stn_city_End=dframe$municipal.y,Subscription=dframe$subsc_type)

#------Check if any value of Bicycle is N/A or zero--------
finaldf<-finaldf[!(is.na(finaldf$Bicycle_id) | finaldf$Bicycle_id==""), ]

#-------------View finaldf-------------
View(finaldf)

#-----write data to csv file---------------------
write.csv(finaldf,"cleaneddatasetfinal.csv",row.names = FALSE)

