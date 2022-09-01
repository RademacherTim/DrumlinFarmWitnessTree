
library(jsonlite)
library(tidyverse)
library(scales)

## Notable Weather Stations ##
#CAS Roof: KMABOSTO333
#Beacon and Carlton St (Brookline, closest to BU): KMABROOK5
#Northeastern University Weather Station: KMABOSTO269
#Blue Hills #1: KMAMILTO3 garbage
#Blue Hills #2: KMAQUINC52
#Near Arnold Arboretum: KMAROSLI14
#Near Harvard Forest: KMAPETER2
#Near Hammond Pond: KMACHEST3
#Near NIST: KMIEASTL10

#specify station of interest
#station <- 'KMABOSTO269'
#station <- 'KMIEASTL10'
#station <- 'KMDGAITH64'
#station <- 'KMDGAITH104'
#station <- 'KMABOSTO333'
#station <- 'KMDCATON32'
#station <- 'KMABROOK70'
station <- "KMAWAYLA31"  #Lincoln, MA near Drumlin Farm

#specify the dates of interest
dates <- gsub('-','',seq(as.Date("2022-08-10"), as.Date("2022-08-15"), by="days"))

#do you want the data in daily CSVs or in one CSV for the whole period? FALSE if one CSV, TRUE if daily CSVs.
daily <- TRUE

#if TRUE, create a nickname for the output file
date_nickname <- station  #create a nickname for the date range: this will be used for output file creation

# create and specify the directory where you want the data stored
#dir.create("bu_met/")
directory <- paste0("/projectnb/buultra/weather_station_data/",station,"/2022/")

###########################################################################################################################################
# Run this chunk to process the weather data # 
###########################################################################################################################################

#import web address
geturl <- function(date) {
  paste0('https://api.weather.com/v2/pws/history/all?stationId=',station,'&format=json&units=m&date=',
         date,'&apiKey=57ff34cd8d004be1bf34cd8d002be183&numericPrecision=decimal')}
#now run this loop - it may take a minute or two
for(d in 1:length(dates)){
  print(d)
  #get the data from online, and then once saved close the connection so it doesn't throw a warning
  raw <- readLines(url(geturl(dates[d])))
  closeAllConnections()  
  #clean up headers
  cleaned <- gsub("\\{\"observations\":",'',raw)
  cleaned <- substr(cleaned, 1, nchar(cleaned)-1)
  #convert from JSON into dataframe
  dataset <- fromJSON(cleaned, flatten=TRUE) %>% as.data.frame
  #add a column with the decimal date and decimal hour
  day <- strptime(dataset$obsTimeLocal,"%Y-%m-%d %H:%M:%S")
  newcol <-matrix(NA, ncol = 2, nrow = nrow(dataset))
  colnames(newcol) = c('DeciDate','DeciHour')
  dataset <- cbind(dataset, newcol)
  for(x in 1:nrow(dataset)){
    dataset[x,]$DeciDate <- (day[x]$yday) + (day[x]$hour/24) + (day[x]$min/(24*60))
    dataset[x,]$DeciHour <- (day[x]$hour) + (day[x]$min/60) + (day[x]$sec/3600)}
  #append them together, making one large dataset
  if(d==1){final <- dataset}else{
    final <- rbind(final, dataset)}
  #write the CSV either as one large CSV or as individual CSVs for each date - this was an earlier input
  if(daily==FALSE){
    write.csv(final, file = paste0(directory,date_nickname,".csv"))}else
    {write.csv(dataset, file = paste0(directory,dates[d],".csv"))}
}
