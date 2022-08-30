#=======================================================================================
# This script updates the local copy of the data needed to run each witness tree, such 
# as meteorological data from the specific met station. For example, we download 
# meteorological data from the Fisher meteorological station for the the Witness Tree at 
# Harvard Forest.
# 
# Weather data is documented here: 
#
# Snow pillow data is documented here:
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
if (!existsFunction("cols"))      suppressPackageStartupMessages(library("readr"))
if (!existsFunction("tibble"))    suppressPackageStartupMessages(library("tibble"))
if (!existsFunction("summarise")) suppressPackageStartupMessages(library("dplyr"))

# read climate data from the appropriate weather station --------------------------------


# create timestamp for each file --------------------------------------------------------


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