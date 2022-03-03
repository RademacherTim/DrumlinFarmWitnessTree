#=======================================================================================
# This script updates the local copy of the data needed to run each witness tree, such 
# as meteorological data from the specific met station. For example, we download 
# meteorological data from the Fisher meteorological station for the the Witness Tree at 
# Harvard Forest.
# 
# Weather datais documented here: 
# https://harvardforest1.fas.harvard.edu/exist/apps/datasets/showData.html?id=HF001
#
# Snow pillow data is documented here:
# https://harvardforest1.fas.harvard.edu/exist/apps/datasets/showData.html?id=HF155
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

# load module specific dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ("esat"))      suppressPackageStartupMessages (library ("plantecophys")) # for calculation of vapour pressure deficit
if (!existsFunction ("as_date"))   suppressPackageStartupMessages (library ("lubridate"))
if (!existsFunction ("cols"))      suppressPackageStartupMessages (library ("readr"))
if (!existsFunction ("tibble"))    suppressPackageStartupMessages (library ("tibble"))
if (!existsFunction ("summarise")) suppressPackageStartupMessages (library ("dplyr"))

# read climate data from the appropriate weather station
#----------------------------------------------------------------------------------------
suppressWarnings (met_HF_shaler <- 
  readr::read_csv (file = url ("http://harvardforest.fas.harvard.edu/data/p00/hf000/hf000-01-daily-m.csv"),
                   col_types = cols ()))
suppressWarnings (met_HF_gap <- 
  readr::read_csv (file = url ("http://harvardforest.fas.harvard.edu/data/p00/hf001/hf001-08-hourly-m.csv"),
                   col_types = cols ()))
suppressWarnings (met_HF_old <- 
  readr::read_csv (file = url ("http://harvardforest.fas.harvard.edu/data/p00/hf001/hf001-10-15min-m.csv"),
                   col_types = cols ()))
suppressWarnings (met_HF_current <- 
  readr::read_csv (file = url ("http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/weather/qfm.csv"),
                   col_types = cols ()))
suppressWarnings (snow_HF_past <- 
  readr::read_csv (file = url ("https://harvardforest.fas.harvard.edu/data/p15/hf155/hf155-02-15min.csv"), 
                   col_types = cols ()))
suppressWarnings (snow_HF_current <- 
  readr::read_csv (file = url ("http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/weather/sqf.csv"),
                   col_types = cols ()))

# create timestamp for each file
#----------------------------------------------------------------------------------------
met_HF_gap$TIMESTAMP <- as.POSIXct (met_HF_gap$datetime, 
                                    format = '%Y-%m-%d %H:%M:%S',
                                    tz = 'EST') 
met_HF_shaler$TIMESTAMP <- as.POSIXct (met_HF_shaler$date, 
                                       format = '%Y-%m-%d',
                                       tz = 'EST') 
met_HF_old$TIMESTAMP <- as.POSIXct (met_HF_old$datetime, 
                                    format = '%Y-%m-%d %H:%M:%S',
                                    tz = 'EST') 
met_HF_current$TIMESTAMP <- as.POSIXct (met_HF_current$datetime, 
                                         format = '%Y-%m-%d %H:%M:%S',
                                         tz = 'EST') 

# extract variables of interest
#----------------------------------------------------------------------------------------
dates  <- c (met_HF_shaler$TIMESTAMP, met_HF_gap$TIMESTAMP, met_HF_old$TIMESTAMP, 
             met_HF_current$TIMESTAMP)
dates2 <- c (met_HF_gap$TIMESTAMP, met_HF_old$TIMESTAMP, met_HF_current$TIMESTAMP)
dates3 <- 
airt <- tibble (TIMESTAMP = dates, airt = c (as.numeric (met_HF_shaler$airt), met_HF_gap$airt, met_HF_old$airt, met_HF_current$airt))
prec <- tibble (TIMESTAMP = dates, prec = c (as.numeric (met_HF_shaler$prec), met_HF_gap$prec, met_HF_old$prec, met_HF_current$prec))
wind <- tibble (TIMESTAMP = dates2, wind = c (met_HF_gap$wspd, met_HF_old$wspd, met_HF_current$wspd))
gust <- tibble (TIMESTAMP = dates2, gust = c (met_HF_gap$gspd, met_HF_old$gspd, met_HF_current$gspd))
rehu <- tibble (TIMESTAMP = dates2, relativeHumidity = c (met_HF_gap$rh, met_HF_old$rh, met_HF_current$rh))
snow <- tibble (TIMESTAMP = c (snow_HF_past$datetime, snow_HF_current$datetime), 
                snow = c (snow_HF_past$swe, snow_HF_current$swe))

# add variable for different aggregation periods to airt (i.e. day, week, month, year)
#----------------------------------------------------------------------------------------
airt <- airt [-1, ] # delete first row, which was pre-1964
airt <- add_column (airt, day   = format (airt [['TIMESTAMP']], '%Y-%m-%d'))
# the ISO 8601 week started two days before the data, therefore we need the two day 
# offset (2 * 60.0 * 60.0 * 24.0)
airt <- add_column (airt, 
                    week  = floor ((airt [['TIMESTAMP']] - 
                                    (min (airt [['TIMESTAMP']], na.rm = T) -
                                    2 * 60.0 * 60.0 * 24.0)) / dweeks (1))) 
airt <- add_column (airt, month = floor_date (airt [['TIMESTAMP']], 'month'))
airt <- add_column (airt, year  = floor_date (airt [['TIMESTAMP']], 'year'))

# create mean airt over varying aggregation periods (i.e. day, week, month, year)
#----------------------------------------------------------------------------------------
dailyAirt    <- airt %>% group_by (day) %>% 
  dplyr::summarise (airt = mean (airt, na.rm = T)) %>% filter (!is.na (day))
dailyMaxAirt <- suppressWarnings (
  airt %>% group_by (day) %>% dplyr::summarise (airt = max  (airt, na.rm = T))) %>% 
  filter (!is.na (day))
weeklyAirt   <- airt %>% group_by (week) %>% 
  dplyr::summarise (airt = mean (airt, na.rm = T)) %>% filter (!is.na (week) & week != 0) # remove week 0 as it did not contain 7 days of data
monthlyAirt  <- airt %>% group_by (month) %>% 
  dplyr::summarise (airt = mean (airt, na.rm = T)) %>% filter (!is.na (month))
yearlyAirt   <- airt %>% group_by (year) %>% 
  dplyr::summarise (airt = mean (airt, na.rm = T)) %>% filter (!is.na (year))

# rank intervals from highest to lowest
#----------------------------------------------------------------------------------------
airt         <- add_column (airt,         rank = rank (-airt         [['airt']]))
dailyAirt    <- add_column (dailyAirt,    rank = rank (-dailyAirt    [['airt']]))
dailyMaxAirt <- add_column (dailyMaxAirt, rank = rank (-dailyMaxAirt [['airt']]))
weeklyAirt   <- add_column (weeklyAirt,   rank = rank (-weeklyAirt   [['airt']]))
monthlyAirt  <- add_column (monthlyAirt,  rank = rank (-monthlyAirt  [['airt']]))
yearlyAirt   <- add_column (yearlyAirt,   rank = rank (-yearlyAirt   [['airt']]))

# add variable for different aggregation period to prec (i.e. day, week, month, year)
#----------------------------------------------------------------------------------------
prec <- prec [-1, ] # delete first row, which was pre-1964
prec <- add_column (prec, day   = format (prec [['TIMESTAMP']], '%Y-%m-%d'))
# the ISO 8601 week started two days before the data, therefore we need the two day 
# offset (2 * 60.0 * 60.0 * 24.0)
prec <- add_column (prec, 
                    week  = floor ((airt [['TIMESTAMP']] - 
                                      (min (airt [['TIMESTAMP']], na.rm = T) -
                                         2 * 60.0 * 60.0 * 24.0)) / dweeks (1))) 
prec <- add_column (prec, month = floor_date (prec [['TIMESTAMP']], 'month'))
prec <- add_column (prec, year  = floor_date (prec [['TIMESTAMP']], 'year'))

# create total prec over varying aggregation periods (i.e. day, week, month, year)
#----------------------------------------------------------------------------------------
dailyPrec   <- prec %>% group_by (day) %>% 
  dplyr::summarise (prec = sum (prec, na.rm = T)) %>% filter (!is.na (day))
weeklyPrec  <- prec %>% group_by (week) %>% 
  dplyr::summarise (prec = sum (prec, na.rm = T)) %>% filter (!is.na (week) & week != 0) # remove week 0 as it did not contain 7 days of data
monthlyPrec <- prec %>% group_by (month) %>% 
  dplyr::summarise (prec = sum (prec, na.rm = T)) %>% filter (!is.na (month))
yearlyPrec  <- prec %>% group_by (year) %>% 
  dplyr::summarise (prec = sum (prec, na.rm = T)) %>% filter (!is.na (year))

# rank intervals from highest to lowest
#----------------------------------------------------------------------------------------
prec         <- add_column (prec,         rank = rank (-prec         [['prec']]))
dailyPrec    <- add_column (dailyPrec,    rank = rank (-dailyPrec    [['prec']]))
weeklyPrec   <- add_column (weeklyPrec,   rank = rank (-weeklyPrec   [['prec']]))
monthlyPrec  <- add_column (monthlyPrec,  rank = rank (-monthlyPrec  [['prec']]))
yearlyPrec   <- add_column (yearlyPrec,   rank = rank (-yearlyPrec   [['prec']]))

# add variable for different aggregation period to snow (i.e. day, week, month)
#----------------------------------------------------------------------------------------
snow <- add_column (snow, day   = format (snow [['TIMESTAMP']], '%Y-%m-%d'))
# the ISO 8601 week started two days before the data, therefore we need the two day 
# offset (2 * 60.0 * 60.0 * 24.0)
snow <- add_column (snow, 
                    week  = floor ((snow [['TIMESTAMP']] - 
                                    min (snow [['TIMESTAMP']], na.rm = T)) / 
                                      dweeks (1)) + 1) # first week is complete, so "+1" assures that it is used
snow <- add_column (snow, month = floor_date (snow [['TIMESTAMP']], 'month'))

# create mean snow over varying aggregation periods (i.e. day, week, month)
#----------------------------------------------------------------------------------------
dailySnow   <- snow %>% group_by (day) %>% 
  dplyr::summarise (snow = mean (snow, na.rm = T)) %>% filter (!is.na (day))
weeklySnow  <- snow %>% group_by (week) %>% 
  dplyr::summarise (snow = mean (snow, na.rm = T)) %>% filter (!is.na (week))
monthlySnow <- snow %>% group_by (month) %>% 
  dplyr::summarise (snow = mean (snow, na.rm = T))  %>% filter (!is.na (month))

# add variable for different aggregation periods to wind and gust (i.e. day, week, month, year)
#----------------------------------------------------------------------------------------
wind <- add_column (wind, day = format (wind [['TIMESTAMP']], '%Y-%m-%d'))
gust <- add_column (gust, day = format (gust [['TIMESTAMP']], '%Y-%m-%d'))

# create daily max wind speed over
#----------------------------------------------------------------------------------------
dailyWind <- gust %>% group_by (day) %>% 
  dplyr::summarise (gust = max (gust, na.rm = T)) %>% filter (!is.na (day))

# add variable for day to rehu to get mean daily relative humidity
#----------------------------------------------------------------------------------------
rehu <- add_column (rehu, day = format (rehu [["TIMESTAMP"]], "%Y-%m-%d"))
dailyReHu <- rehu %>% group_by (day) %>% 
  dplyr::summarise (relativeHumidity = mean (relativeHumidity, na.rm = T)) %>% filter (!is.na (day))

# calculate daily vapour pressure deficit
#----------------------------------------------------------------------------------------
dailyVPD <- tibble (day = dailyReHu [["day"]],
                    VPS = RHtoVPD (RH = dailyReHu [["relativeHumidity"]],
                                   TdegC = dailyAirt [["airt"]] [dailyAirt [["day"]] >= dailyReHu [["day"]] [1]],
                                   Pa = 101)) # Should make pressure a variable as well.

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
rm (met_HF_current, met_HF_gap, met_HF_old, met_HF_shaler, snow_HF_past, snow_HF_current,
    rehu, wind, dates, dates2, path)
#========================================================================================