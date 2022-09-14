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
  WU_API_key = args[2] # weather underground API key
} else {
  stop("Error: Too many command line arguments supplied to R.")
}
#print(path)

# load module specific dependencies -----------------------------------------------------
if (!existsFunction("esat"))      suppressPackageStartupMessages(library("plantecophys")) # for calculation of vapour pressure deficit
if (!existsFunction("as_date"))   suppressPackageStartupMessages(library("lubridate"))
if (!existsFunction("%>%"))       suppressPackageStartupMessages(library("tidyverse"))
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

# set daily to false so that one single csv file is generated ---------------------------
daily <- FALSE

# read previously downloaded data -------------------------------------------------------
hist_data <- read_csv(paste0("./data/",station,"_hist_2014-04-14.csv"),
                      col_types = cols()) 

# identify last day of downloaded data --------------------------------------------------
end_datetime <- as_datetime(tail(hist_data$datetime, n = 1))
end_date <- as_date(tail(hist_data$datetime, n = 1))

# specify the dates for which data is still missing -------------------------------------
dates <- gsub('-', '', seq(end_date, Sys.Date(), by = "days"))

# if TRUE, create a nickname for the output files ---------------------------------------
date_nickname <- station  # create a nickname for the date range: this will be used for 
                          # output file creation

# create and specify the directory where you want the data stored -----------------------
directory <- paste0("./data/")

# import web address --------------------------------------------------------------------
geturl <- function(date) {
  paste0('https://api.weather.com/v2/pws/history/all?stationId=',station,'&format=json&units=m&date=',
         date,'&apiKey=',WU_API_key,'&numericPrecision=decimal')}

# turn off warnings temporarily ---------------------------------------------------------
options(warn = -1)

# now run loop to download and read newer files -----------------------------------------
for (d in 1:length(dates)) {
  #print(d)
  
  # get the data from online, and then once saved close the connection so it doesn't 
  # throw a warning --------------------------------------------------------------------
  raw <- readLines(url(geturl(dates[d])))
  if(raw == "{\"observations\":[]}") next
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
  #if (daily == FALSE & d == length(dates)) {
  #  write.csv(final, file = paste0(directory, date_nickname, ".csv"))
  #} else if (daily == TRUE) {
  #  write.csv(dataset, file = paste0(directory, dates[d],".csv"))
  #}
}

# turn warnings back on -----------------------------------------------------------------
options(warn = 0)

# unselect unnecessary variables --------------------------------------------------------
temp <- final %>%
  select(-stationID, -tz, -epoch, -lat, -lon, -solarRadiationHigh, -uvHigh, 
         -metric.windchillHigh, -metric.windchillLow, -metric.windchillAvg, 
         -metric.heatindexHigh, -metric.heatindexLow, -metric.heatindexAvg) %>% 
  as_tibble()

# determine whether there are new data --------------------------------------------------
end_datetime2 <- as_datetime(tail(temp$obsTimeLocal, n = 1))
if (end_datetime2 > end_datetime) {
  
  # select only new data ----------------------------------------------------------------
  additional_data <- temp %>% filter(obsTimeUtc > end_datetime) %>%
    select(-obsTimeUtc) %>%
    rename(datetime = obsTimeLocal)
  
  hist_data <- rbind(hist_data, additional_data)
}

# save copy of updated data -------------------------------------------------------------
write_csv(hist_data, file = "./data/KMALINCO3_hist_2014-04-14.csv")

# add variable for different aggregation periods to airt (i.e. day, week, month, year)
#----------------------------------------------------------------------------------------
airt <- hist_data %>% 
  select(datetime, metric.tempAvg) %>% 
  rename(airt = metric.tempAvg) %>%
  mutate (day = as_date(datetime, '%Y-%m-%d'))
# the ISO 8601 week started on day of data, but this was already week 14
#airt <- add_column(airt, 
#                    week  = floor ((airt$datetime - 
#                                    (min(airt$datetime, na.rm = T))) / dweeks (1))) 
airt <- add_column(airt, month = floor_date(airt$datetime, 'month'))
airt <- add_column(airt, year  = floor_date(airt$datetime, 'year'))

# create mean airt over varying aggregation periods (i.e. day, week, month, year)
#----------------------------------------------------------------------------------------
dailyAirt <- airt %>% group_by(day) %>% 
  dplyr::summarise(airt = mean(airt, na.rm = T)) %>% filter(!is.na(day))
dailyMaxAirt <- suppressWarnings(
  airt %>% group_by(day) %>% dplyr::summarise(airt = max(airt, na.rm = T))) %>% 
  filter(!is.na(day))
#weeklyAirt <- airt %>% group_by (week) %>% 
#  dplyr::summarise (airt = mean (airt, na.rm = T)) %>% filter (!is.na (week) & week != 0) # remove week 0 as it did not contain 7 days of data
monthlyAirt <- airt %>% group_by(month) %>% 
  dplyr::summarise(airt = mean(airt, na.rm = T)) %>% filter(!is.na(month))
yearlyAirt <- airt %>% group_by(year) %>% 
  dplyr::summarise (airt = mean(airt, na.rm = T)) %>% filter(!is.na(year))

# rank intervals from highest to lowest
#----------------------------------------------------------------------------------------
airt         <- add_column(airt,         rank = rank(-airt$airt))
dailyAirt    <- add_column(dailyAirt,    rank = rank(-dailyAirt$airt))
dailyMaxAirt <- add_column(dailyMaxAirt, rank = rank(-dailyMaxAirt$airt))
#weeklyAirt   <- add_column(weeklyAirt,   rank = rank(-weeklyAirt   [['airt']]))
monthlyAirt  <- add_column(monthlyAirt,  rank = rank(-monthlyAirt$airt))
yearlyAirt   <- add_column(yearlyAirt,   rank = rank(-yearlyAirt$airt))

# add variable for different aggregation period to prec (i.e. day, week, month, year)
#----------------------------------------------------------------------------------------
# TR - Need to figure out what the units are
# prec <- hist_data %>% 
#   select(datetime, metric.precipRate) %>% 
#   rename(prec = metric.precipRate) %>%
#   mutate (day = as_date(datetime, '%Y-%m-%d'))
# Week determination needs to be debugged
#prec <- add_column (prec, 
#                    week  = floor ((airt [['TIMESTAMP']] - 
#                                      (min (airt [['TIMESTAMP']], na.rm = T) -
#                                         2 * 60.0 * 60.0 * 24.0)) / dweeks (1))) 
# prec <- add_column (prec, month = floor_date (prec$datetime, 'month'))
# prec <- add_column (prec, year  = floor_date (prec$datetime, 'year'))


# create total prec over varying aggregation periods (i.e. day, week, month, year)
#----------------------------------------------------------------------------------------
# dailyPrec <- prec %>% group_by(day) %>% 
#   dplyr::summarise(prec = sum(prec, na.rm = T)) %>% filter(!is.na(day))
#weeklyPrec  <- prec %>% group_by(week) %>% 
#  dplyr::summarise(prec = sum(prec, na.rm = T)) %>% filter(!is.na(week) & week != 0) # remove week 0 as it did not contain 7 days of data
# monthlyPrec <- prec %>% group_by(month) %>% 
#   dplyr::summarise(prec = sum(prec, na.rm = T)) %>% filter(!is.na(month))
# yearlyPrec <- prec %>% group_by(year) %>% 
#   dplyr::summarise(prec = sum(prec, na.rm = T)) %>% filter(!is.na(year))

# rank intervals from highest to lowest
#----------------------------------------------------------------------------------------
# prec        <- add_column(prec,         rank = rank(-prec$prec))
# dailyPrec   <- add_column(dailyPrec,    rank = rank(-dailyPrec$prec))
#weeklyPrec  <- add_column(weeklyPrec,   rank = rank(-weeklyPrec$prec))
# monthlyPrec <- add_column(monthlyPrec,  rank = rank(-monthlyPrec$prec))
# yearlyPrec  <- add_column(yearlyPrec,   rank = rank(-yearlyPrec$prec))

# add variable for different aggregation periods to wind and gust (i.e. day, week, month, year)
#----------------------------------------------------------------------------------------
wind <-  hist_data %>% 
  select(datetime, metric.windspeedAvg) %>% 
  rename(wind = metric.windspeedAvg) %>%
  mutate (day = as_date(datetime, '%Y-%m-%d'))
gust <-  hist_data %>% 
  select(datetime, metric.windgustAvg) %>% 
  rename(gust = metric.windgustAvg) %>%
  mutate (day = as_date(datetime, '%Y-%m-%d'))

# create daily max wind speed over
#----------------------------------------------------------------------------------------
dailyWind <- gust %>% group_by(day) %>% 
  dplyr::summarise(gust = max(gust, na.rm = T)) %>% filter(!is.na (day))

# add variable for day to rehu to get mean daily relative humidity
#----------------------------------------------------------------------------------------
rehu <-  hist_data %>% 
  select(datetime, humidityAvg) %>% 
  rename(rehu = humidityAvg) %>%
  mutate (day = as_date(datetime, '%Y-%m-%d'))
dailyReHu <- rehu %>% group_by(day) %>% 
  dplyr::summarise(rehu = mean(rehu, na.rm = T)) %>% 
  filter(!is.na(day))

# calculate daily vapour pressure deficit
#----------------------------------------------------------------------------------------
dailyVPD <- tibble (day = dailyReHu$day,
                    VPD = RHtoVPD(RH = dailyReHu$rehu,
                                  TdegC = dailyAirt$airt,
                                  Pa = 101)) # Should make pressure a variable as well.

# write csv files of the main variables -------------------------------------------------
readr::write_csv (x = airt, file = sprintf ("%sdata/airt.csv", path))
readr::write_csv (x = gust, file = sprintf ("%sdata/gust.csv", path))
readr::write_csv (x = prec, file = sprintf ("%sdata/prec.csv", path))

readr::write_csv (x = dailyAirt, file = sprintf ("%sdata/dailyAirt.csv", path))
readr::write_csv (x = dailyMaxAirt, file = sprintf ("%sdata/dailyMaxAirt.csv", path))
readr::write_csv (x = dailyPrec, file = sprintf ("%sdata/dailyPrec.csv", path))
readr::write_csv (x = dailyReHu, file = sprintf ("%sdata/dailyReHu.csv", path))
readr::write_csv (x = dailyVPD, file = sprintf ("%sdata/dailyVPD.csv", path))
readr::write_csv (x = dailyWind, file = sprintf ("%sdata/dailyWind.csv", path))

#readr::write_csv (x = weeklyAirt, file = sprintf ("%sdata/weeklyAirt.csv", path))
#readr::write_csv (x = weeklyPrec, file = sprintf ("%sdata/weeklyPrec.csv", path))
#readr::write_csv (x = weeklySnow, file = sprintf ("%sdata/weeklySnow.csv", path))

readr::write_csv (x = monthlyAirt, file = sprintf ("%sdata/monthlyAirt.csv", path))
readr::write_csv (x = monthlyPrec, file = sprintf ("%sdata/monthlyPrec.csv", path))

readr::write_csv (x = yearlyAirt, file = sprintf ("%sdata/yearlyAirt.csv", path))
readr::write_csv (x = yearlyPrec, file = sprintf ("%sdata/yearlyPrec.csv", path))

# delete temporary variables ------------------------------------------------------------
rm (additional_data, airt, dailyAirt, dailyMaxAirt, dailyPrec, dailyReHu, dailyVPD, 
    dailyWind, dataset, day, final, gust, hist_data, monthlyAirt, monthlyPrec, 
    newcol, prec, rehu, temp, wind, yearlyAirt, yearlyPrec, d, date_nickname, dates, 
    end_date, end_datetime, end_datetime2, path, raw, station, WU_API_key)
#========================================================================================