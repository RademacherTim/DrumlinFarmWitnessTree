#=======================================================================================
# This script updates the local copy of the data needed to run each witness tree, such 
# as meteorological data from the specific met station. For example, we download 
# meteorological data from the Fisher meteorological station for the the Witness Tree at 
# Harvard Forest.
# 
# Weather data is from a weatherunderground station with the script largely based on 
# work by Taylor Jones (wunderground_download.R).
# 
#---------------------------------------------------------------------------------------

# get arguments from command line (i.e., absolute path to working directory) -----------
args = commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("Error: At least one argument must be supplied (path to witnessTree directory).",
        call. = FALSE)
} else if (length(args) >= 1) {
  path = args[1] # absolute path
} else {
  stop("Error: Too many command line arguments supplied to R.")
}
#print(path)

# load module specific dependencies -----------------------------------------------------
if (!existsFunction("esat"))      suppressPackageStartupMessages(library("plantecophys")) # for calculation of vapour pressure deficit
if (!existsFunction("as_date"))   suppressPackageStartupMessages(library("lubridate"))
if (!existsFunction("%>%"))      suppressPackageStartupMessages(library("tidyverse"))
if (!existsFunction("fromJSON"))  suppressPackageStartupMessages(library("jsonlite"))

# specify weather station of interest ---------------------------------------------------
#station <- "KMAWAYLA31"  # Full Moon Farm, Lincoln, MA near Drumlin Farm (only since 2021)
station <- "KMALINCO3" # Lincoln Center, Lincoln, MA near Drumlin Farm (2014-04-14)
#station <- "KMACONCO62" # The Champ, Conchord, MA, 
#station <- "KMACONCO67" # Hubbardville House, Conchord, MA, 
#station <- "KMALINCO15"  # Tyler's Weather Station #1, Lincoln, MA
#station <- "KMAWESTO43" # Weston Home, Lincoln
#station <- "KMAWESTO44" # Ogilvie, Weston
#station <- "KMALINCO8" # Brooks Hill, Hanscom Airfield, Lincoln, MA (2018-01-11)


# read previously downloaded data -------------------------------------------------------
hist_data <- read_csv(paste0("../data/",station,"_hist_2014-04-14.csv"),
                      col_types = cols()) %>% 
  select(-n, -stationID, -tz, -obsTimeUtc,-epoch, -lat, -lon, -solarRadiationHigh, 
         -uvHigh, -metric.windchillHigh, -metric.windchillLow, -metric.windchillAvg,
         -metric.heatindexHigh, -metric.heatindexLow, -metric.heatindexAvg) %>% 
  rename(datetime = obsTimeLocal)

# identify last day of downloaded data --------------------------------------------------
end_date <- as_date(tail(hist_data$datetime, n = 1))

# specify the dates for which data is still missing -------------------------------------
dates <- gsub('-', '', seq(end_date, Sys.Date(), by = "days"))

# if TRUE, create a nickname for the output files ---------------------------------------
date_nickname <- station  # create a nickname for the date range: this will be used for 
                          # output file creation

# create and specify the directory where you want the data stored -----------------------
directory <- paste0("../data/")

# import web address --------------------------------------------------------------------
geturl <- function(date) {
  paste0('https://api.weather.com/v2/pws/history/all?stationId=',station,'&format=json&units=m&date=',
         date,'&apiKey=57ff34cd8d004be1bf34cd8d002be183&numericPrecision=decimal')}

# turn off warnings temporarily ---------------------------------------------------------
options(warn = -1)

# now run loop to download and read files -----------------------------------------------
for (d in 1:length(dates)) {
  print(d)
  
  # get the data from online, and then once saved close the connection so it doesn't 
  # throw a warning --------------------------------------------------------------------
  raw <- readLines(url(geturl(dates[d])))
  on.exit(close(url(geturl(dates[d]))))  
  
  # clean up headers -------------------------------------------------------------------
  cleaned <- gsub("\\{\"observations\":", '', raw)
  cleaned <- substr(cleaned, 1, nchar(cleaned)-1)
  
  # convert from JSON into dataframe ---------------------------------------------------
  dataset <- fromJSON(cleaned, flatten=TRUE) %>% as.data.frame
  
  # add a column with the decimal date and decimal hour --------------------------------
  day <- strptime(dataset$obsTimeLocal,"%Y-%m-%d %H:%M:%S")
  newcol <-matrix(NA, ncol = 2, nrow = nrow(dataset))
  colnames(newcol) = c('DeciDate','DeciHour')
  dataset <- cbind(dataset, newcol)
  for (x in 1:nrow(dataset)) {
    dataset[x,]$DeciDate <- (day[x]$yday) + (day[x]$hour/24) + (day[x]$min/(24*60))
    dataset[x,]$DeciHour <- (day[x]$hour) + (day[x]$min/60) + (day[x]$sec/3600)
  }
  
  # append them together, making one large dataset --------------------------------------
  if (d == 1) {
    final <- dataset
  } else {
    final <- rbind(final, dataset)
  }
  
  # write the CSV either as one large CSV or as individual CSVs for each date -----------
  # this was an earlier input
  if (daily == FALSE) {
    write.csv(final, file = paste0(directory, date_nickname, ".csv"))
  } else {
    write.csv(dataset, file = paste0(directory, dates[d],".csv"))
  }
}

# turn warnings back on -----------------------------------------------------------------
options(warn = 0)

# extract variables of interest ---------------------------------------------------------


# add variable for different aggregation periods to airt (i.e. day, week, month, year)
#----------------------------------------------------------------------------------------

# create mean airt over varying aggregation periods (i.e. day, week, month, year)
#----------------------------------------------------------------------------------------

# rank intervals from highest to lowest
#----------------------------------------------------------------------------------------


# add variable for different aggregation period to prec (i.e. day, week, month, year)
#----------------------------------------------------------------------------------------
prec <- prec [-1, ] # delete first row, which was pre-1964
prec <- add_column (prec, day   = format (prec [['TIMESTAMP']], '%Y-%m-%d'))
# the ISO 8601 week started two days before the data, therefore we need the two day 
# offset (2 * 60.0 * 60.0 * 24.0)


# create total prec over varying aggregation periods (i.e. day, week, month, year)
#----------------------------------------------------------------------------------------


# rank intervals from highest to lowest
#----------------------------------------------------------------------------------------


# add variable for different aggregation period to snow (i.e. day, week, month)
#----------------------------------------------------------------------------------------


# create mean snow over varying aggregation periods (i.e. day, week, month)
#----------------------------------------------------------------------------------------


# add variable for different aggregation periods to wind and gust (i.e. day, week, month, year)
#----------------------------------------------------------------------------------------


# create daily max wind speed over
#----------------------------------------------------------------------------------------


# add variable for day to rehu to get mean daily relative humidity
#----------------------------------------------------------------------------------------


# calculate daily vapour pressure deficit
#----------------------------------------------------------------------------------------

# write csv files of the main variables -------------------------------------------------
readr::write_csv (x = airt, file = sprintf ("%sdata/airt.csv", path))
readr::write_csv (x = gust, file = sprintf ("%sdata/gust.csv", path))
readr::write_csv (x = prec, file = sprintf ("%sdata/prec.csv", path))
readr::write_csv (x = snow, file = sprintf ("%sdata/snow.csv", path))

readr::write_csv (x = dailyAirt, file = sprintf ("%sdata/dailyAirt.csv", path))
readr::write_csv (x = dailyMaxAirt, file = sprintf ("%sdata/dailyMaxAirt.csv", path))
readr::write_csv (x = dailyPrec, file = sprintf ("%sdata/dailyPrec.csv", path))
readr::write_csv (x = dailyReHu, file = sprintf ("%sdata/dailyReHu.csv", path))
readr::write_csv (x = dailySnow, file = sprintf ("%sdata/dailySnow.csv", path))
readr::write_csv (x = dailyVPD, file = sprintf ("%sdata/dailyVPD.csv", path))
readr::write_csv (x = dailyWind, file = sprintf ("%sdata/dailyWind.csv", path))

readr::write_csv (x = weeklyAirt, file = sprintf ("%sdata/weeklyAirt.csv", path))
readr::write_csv (x = weeklyPrec, file = sprintf ("%sdata/weeklyPrec.csv", path))
readr::write_csv (x = weeklySnow, file = sprintf ("%sdata/weeklySnow.csv", path))

readr::write_csv (x = monthlyAirt, file = sprintf ("%sdata/monthlyAirt.csv", path))
readr::write_csv (x = monthlyPrec, file = sprintf ("%sdata/monthlyPrec.csv", path))
readr::write_csv (x = monthlySnow, file = sprintf ("%sdata/monthlySnow.csv", path))

readr::write_csv (x = yearlyAirt, file = sprintf ("%sdata/yearlyAirt.csv", path))
readr::write_csv (x = yearlyPrec, file = sprintf ("%sdata/yearlyPrec.csv", path))

# delete temporary variables ------------------------------------------------------------
rm ()
#========================================================================================