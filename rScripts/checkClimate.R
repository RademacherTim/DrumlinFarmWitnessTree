#========================================================================================
# Functions to generate posts depending on the weather and climate at the tree (i.e., 
# based on meteorological data from nearby meteorological station).
#
#       Event                                   Date                 
#----------------------------------------------------------------------------------------
#   1)  Hottest temperature experienced         variable
#   2)  Coldest temperature experienced         variable
#   3)  Wettest day experienced                 variable Shawna will program this!
#   4)  Hottest day experienced                 variable
#   5)  Coldest day experienced                 variable
#   6)  Wettest week experienced                variable
#   7)  Hottest week experienced                variable
#   8)  Coldest week experienced                variable
#   9)  Wettest month experienced               variable
#  10)  Hottest month experienced               variable
#  11)  Coldest month experienced               variable
#  12)  Wettest year experienced                variable
#  13)  Hottest year experienced                variable
#  14)  Coldest year experienced                variable
#  15)  Latest frost event                      variable
#  16)  Lastest snow                            variable 
#  17)  Monthly summary of climate              beginning of following month
#  18)  Annual summary of climate               beginning of following year
#  19)  Growing season summary of climate       at the beginning of November?
#  18)  Frost event                             variable TR - Not made yet
#  xx)  Heat wave                               variable TR - Not made yet
#  xx)  Drought                                 variable TR - Not made yet
#  xx)  Heavy rain                              variable TR - Not made yet
#  xx)  Still standing after heavy wind/storm   variable TR - Not made yet "I am still standing"
#  XX)  Maple sirup season                      variable TR - Not made yet "My bro's juices are flowing"
#----------------------------------------------------------------------------------------

# to-do list:
#----------------------------------------------------------------------------------------
# TR - I might also want to delay messages for December and last year, so that they do not come out straight after new year's resolutions.
# TR - I need to introduce a memory or something to make sure that I am not repeating the same messages for hottest/coldest temperature over and over again.

# read in instantaneous weather data
#----------------------------------------------------------------------------------------
airt <- read_csv (file = paste0 (path,'data/airt.csv'), col_types = cols ())
gust <- read_csv (file = paste0 (path,'data/gust.csv'), col_types = cols ())
prec <- read_csv (file = paste0 (path,'data/prec.csv'), col_types = cols ())
# daily
dailyAirt <- read_csv (file = paste0 (path,'data/dailyAirt.csv'), col_types = cols ())
dailyMaxAirt <- read_csv (file = paste0 (path,'data/dailyMaxAirt.csv'), col_types = cols ())
dailyPrec <- read_csv (file = paste0 (path,'data/dailyPrec.csv'), col_types = cols ())
dailyReHu <- read_csv (file = paste0 (path,'data/dailyReHu.csv'), col_types = cols ())
dailyVPD <- read_csv (file = paste0 (path,'data/dailyVPD.csv'), col_types = cols ())
dailyWind <- read_csv (file = paste0 (path,'data/dailyWind.csv'), col_types = cols ())
# weekly
weeklyAirt <- read_csv (file = paste0 (path,'data/weeklyAirt.csv'), col_types = cols ())
weeklyPrec <- read_csv (file = paste0 (path,'data/weeklyPrec.csv'), col_types = cols ())
# monthly
monthlyAirt <- read_csv (file = paste0 (path,'data/monthlyAirt.csv'), col_types = cols ())
monthlyPrec <- read_csv (file = paste0 (path,'data/monthlyPrec.csv'), col_types = cols ())
# yearly
yearlyAirt <- read_csv (file = paste0 (path,'data/yearlyAirt.csv'), col_types = cols ())
yearlyPrec <- read_csv (file = paste0 (path,'data/yearlyPrec.csv'), col_types = cols ())

# convert degrees Celsius to Fahrenheit
#----------------------------------------------------------------------------------------
CtoF <- function (degC, difference = F) {
  if (!difference) {
    degF <- (degC * 9.0 / 5.0) + 32.0
  } else {
    degF <- degC * 9.0 / 5.0
  }
  return (degF)
}

# convert millimeters to inches
#----------------------------------------------------------------------------------------
mmtoInches <- function (mm) {
  inches <- mm / 25.4
  return (inches)
}

# hottest or coldest temperature on record (in memory)
#----------------------------------------------------------------------------------------
checkExtremeTemperatures <- function (ptable, TEST = 0) {
  
  # load depdendencies
  #--------------------------------------------------------------------------------------
  if (!existsFunction ('month')) library ('lubridate') 

  # check whether the current temperature is the hottest temperature on record 
  #--------------------------------------------------------------------------------------
  if (max (airt [['airt']], na.rm = T) <= tail (airt [['airt']], n = 1) | TEST == 1) {
    HOTTEST <- TRUE
    HOT     <- FALSE
  } else if (tail (airt [['rank']], n = 1) <=  100 | TEST == 2) {
    HOTTEST <- FALSE
    HOT     <- TRUE
  } else {
    HOTTEST <- FALSE
    HOT     <- FALSE
  }
  
  # check whether the yesterday was the warmest day on record
  #--------------------------------------------------------------------------------------
  if (max (dailyAirt [['airt']], na.rm = T) <= 
      head (tail (dailyAirt [['airt']], n = 2), n = 1) | TEST == 3) {
    HOTTESTDAY <- TRUE
    HOTDAY     <- FALSE
  } else if (head (tail (dailyAirt [['rank']], n = 2), n = 1) <= 50 | TEST == 4) {
    HOTTESTDAY <- FALSE
    HOTDAY     <- TRUE
  } else {
    HOTTESTDAY <- FALSE
    HOTDAY     <- FALSE
  }
  
  # Check whether the last week was the warmest week on record
  #--------------------------------------------------------------------------------------
  if (max (weeklyAirt [['airt']], na.rm = T) <= 
      head (tail (weeklyAirt [['airt']], n = 2), n = 1) | TEST == 5) { 
    HOTTESTWEEK <- TRUE
    HOTWEEK     <- FALSE
  } else if (head (tail (weeklyAirt [['rank']], n = 2), n = 1) <= 30 | TEST == 6) {
    HOTTESTWEEK <- FALSE
    HOTWEEK     <- TRUE
  } else {
    HOTTESTWEEK <- FALSE
    HOTWEEK     <- FALSE
  }
  
  # Check whether the last month was the warmest month on record
  #--------------------------------------------------------------------------------------
  if (max (monthlyAirt [['airt']], na.rm = T) <= 
      head (tail (monthlyAirt [['airt']], n = 2), n = 1) | TEST == 7) { 
    HOTTESTMONTH <- TRUE
    HOTMONTH     <- FALSE
  } else if (head (tail (monthlyAirt [['rank']], n = 2), n = 1) <= 20 | TEST == 8) {
    HOTTESTMONTH <- FALSE
    HOTMONTH     <- TRUE
  } else {
    HOTTESTMONTH <- FALSE
    HOTMONTH     <- FALSE
  }
  
  # Check whether the last year was the warmest year on record
  #--------------------------------------------------------------------------------------
  if (max (yearlyAirt [['airt']], na.rm = T) <= 
      head (tail (yearlyAirt [['airt']], n = 2), n = 1) | TEST == 9) { 
    HOTTESTYEAR <- TRUE
    HOTYEAR     <- FALSE
  } else if (head (tail (yearlyAirt [['rank']], n = 2), n = 1) <= 10 | TEST == 10) {
    HOTTESTYEAR <- FALSE
    HOTYEAR     <- TRUE
  }  else {
    HOTTESTYEAR <- FALSE
    HOTYEAR     <- FALSE
  }
  
  # Check whether the current temperature is the coldest temperature on record
  #--------------------------------------------------------------------------------------
  tempRank <- rank (airt [['airt']])
  if (min (airt [['airt']], na.rm = T) >= 
      tail (airt [['airt']], n = 1) | TEST == 11) {
    COLDEST <- TRUE
    COLD    <- FALSE
  } else if (tail (tempRank, n = 1) <= 100 | TEST == 12) {
    COLDEST <- FALSE
    COLD    <- TRUE
  } else {
    COLDEST <- FALSE
    COLD    <- FALSE
  }
  temperatureC <- tail (airt [['airt']], n = 1)
  temperatureF <- CtoF (temperatureC)
  
  # Check whether the yesterday was the coldest day on record
  #--------------------------------------------------------------------------------------
  tempRank <- rank (dailyAirt [['airt']]) 
  if (min (dailyAirt [['airt']], na.rm = T) >= 
      head (tail (dailyAirt [['airt']], n = 2), n = 1) | TEST == 13) {
    COLDESTDAY <- TRUE
    COLDDAY    <- FALSE
  } else if (head (tail (tempRank, n = 2), n = 1) <= 50 | TEST == 14) {
    COLDESTDAY <- FALSE
    COLDDAY    <- TRUE
  } else {
    COLDESTDAY <- FALSE
    COLDDAY    <- FALSE
  }
  dailyTemperatureC <- head (tail (dailyAirt [['airt']], n = 2), n = 1)
  dailyTemperatureF <- CtoF (dailyTemperatureC)
  
  # Check whether the last week was the coldest week on record
  #--------------------------------------------------------------------------------------
  tempRank <- rank (weeklyAirt [['airt']]) 
  if (min (weeklyAirt [['airt']], na.rm = T) >= 
      head (tail (weeklyAirt [['airt']], n = 2), n = 1) | TEST == 15) { 
    COLDESTWEEK <- TRUE
    COLDWEEK    <- FALSE
  } else if (head (tail (tempRank, n = 2), n = 1) <= 30 | TEST == 16) {
    COLDESTWEEK <- FALSE
    COLDWEEK    <- TRUE
  } else {
    COLDESTWEEK <- FALSE
    COLDWEEK    <- FALSE
  }
  weeklyTemperatureC <- head (tail (weeklyAirt [['airt']], n = 2), n = 1)
  weeklyTemperatureF <- CtoF (weeklyTemperatureC)
  
  # Check whether the last month was the coldest month on record
  #--------------------------------------------------------------------------------------
  tempRank <- rank (monthlyAirt [['airt']]) 
  if (min (monthlyAirt [['airt']], na.rm = T) >= 
      head (tail (monthlyAirt [['airt']], n = 2), n = 1) | TEST == 17) { 
    COLDESTMONTH <- TRUE
    COLDMONTH    <- FALSE
  } else if (head (tail (tempRank, n = 2), n = 1) <= 20 | TEST == 18) {
    COLDESTMONTH <- FALSE
    COLDMONTH    <- TRUE
  }  else {
    COLDESTMONTH <- FALSE
    COLDMONTH    <- FALSE
  }
  monthlyTemperatureC <- head (tail (monthlyAirt [['airt']], n = 2), n = 1)
  monthlyTemperatureF <- CtoF (monthlyTemperatureC)
  
  # Check whether the last year was the warmest year on record
  #--------------------------------------------------------------------------------------
  tempRank <- rank (yearlyAirt [['airt']]) 
  if (min (yearlyAirt [['airt']], na.rm = T) >= 
      head (tail (yearlyAirt [['airt']], n = 2), n = 1) | TEST == 19) { 
    COLDESTYEAR <- TRUE
    COLDYEAR    <- FALSE
  } else if (head (tail (tempRank, n = 2), n = 1) <= 10 | TEST == 20) {
    COLDESTYEAR <- FALSE
    COLDYEAR    <- TRUE
  } else {
    COLDESTYEAR <- FALSE
    COLDYEAR    <- FALSE
  }
  yearlyTemperatureC <- head (tail (yearlyAirt [['airt']], n = 2), n = 1)
  yearlyTemperatureF <- CtoF (yearlyTemperatureC) 
  
  # post message if it is the hottest or coldest temperature
  #--------------------------------------------------------------------------------------
  if (HOT | HOTTEST | COLD | COLDEST | HOTDAY | HOTTESTDAY | COLDDAY | COLDESTDAY | 
      HOTWEEK | HOTTESTWEEK | COLDWEEK | COLDESTWEEK | HOTTESTMONTH | COLDESTMONTH | 
      HOTTESTYEAR | COLDESTYEAR | TEST >= 1) {
    priority  <- 9
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date (), 
                                                    format = '%Y-%m-%d'), treeTimeZone)

    # get posts details, if they are overwritten use order of priority below
    #     1) cold events is prioritised over hot events
    #     2) short-term events are prioritised over longer-term events (daily > annual)
    #------------------------------------------------------------------------------------
    if (HOTYEAR) {
      postDetails <- getPostDetails ('hotYear')
      rank <- head (tail (yearlyAirt [['rank']], n = 2), n = 1)
      message   <- sprintf (postDetails [['MessageText']], rank, findOrdinalSuffix (rank), 
                            round (yearlyTemperatureC, 1), 
                            round (yearlyTemperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
    }
    if (COLDYEAR) {
      postDetails <- getPostDetails ('coldYear')
      rank <- head (tail (rank (yearlyAirt [['airt']]), n = 2), n = 1)
      message <- sprintf (postDetails [['MessageText']], rank, findOrdinalSuffix (rank), 
                          round (yearlyTemperatureC, 1), 
                          round (yearlyTemperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
    }
    if (HOTTESTYEAR) {
      postDetails <- getPostDetails ('hottestYear')
      message   <- sprintf (postDetails [['MessageText']], year (Sys.Date ()) - 1,
                            round (yearlyTemperatureC, 1), 
                            round (yearlyTemperatureF, 1),
                            round (mean (yearlyAirt [['airt']], na.rm = T), 1),
                            round (CtoF (mean (yearlyAirt [['airt']], na.rm = T)), 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
    }
    if (COLDESTYEAR) {
      postDetails <- getPostDetails ('coldestYear')
      message   <- sprintf (postDetails [['MessageText']], 
                            lubridate::year (Sys.Date ()) - 1,
                            round (yearlyTemperatureC, 1), 
                            round (yearlyTemperatureF, 1),
                            round (mean (yearlyAirt [['airt']], na.rm = T), 1),
                            round (CtoF (mean (yearlyAirt [['airt']], na.rm = T)), 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
    }
    if (HOTMONTH) {
      postDetails <- getPostDetails ('hotMonth')
      rank <- head (tail (monthlyAirt [['rank']], n = 2), n = 1)
      message   <- sprintf (postDetails [['MessageText']], treeLocationName, rank, 
                            findOrdinalSuffix (rank), round (monthlyTemperatureC, 1), 
                            round (monthlyTemperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
    } 
    if (COLDMONTH) {
      postDetails <- getPostDetails ('coldMonth')
      rank <- head (tail (rank (monthlyAirt [['airt']]), n = 2), n = 1)
      message   <- sprintf (postDetails [['MessageText']], treeLocationName, rank, 
                            findOrdinalSuffix (rank),
                            round (monthlyTemperatureC, 1), 
                            round (monthlyTemperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
    }
    if (HOTTESTMONTH) {
      postDetails <- getPostDetails ('hottestMonth')
      message   <- sprintf (postDetails [['MessageText']], round (monthlyTemperatureC, 1), 
                            round (monthlyTemperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
    }
    if (COLDESTMONTH) {
      postDetails <- getPostDetails ('coldestMonth')
      message   <- sprintf (postDetails [['MessageText']], round (monthlyTemperatureC, 1), 
                            round (monthlyTemperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
    }
    if (HOTWEEK) {
      postDetails <- getPostDetails ('hotWeek')
      message   <- sprintf (postDetails [['MessageText']], round (weeklyTemperatureC, 1), 
                            round (weeklyTemperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7)) * 60 * 60
    }    
    if (COLDWEEK) {
      postDetails <- getPostDetails ('coldWeek')
      message   <- sprintf (postDetails [['MessageText']], round (weeklyTemperatureC, 1), 
                            round (weeklyTemperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 7)) * 60 * 60
    }
    if (HOTTESTWEEK) {
      postDetails <- getPostDetails ('hottestWeek')
      message   <- sprintf (postDetails [['MessageText']], round (weeklyTemperatureC, 1), 
                            round (weeklyTemperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 7)) * 60 * 60
    }
    if (COLDESTWEEK) {
      postDetails <- getPostDetails ('coldestWeek')
      message   <- sprintf (postDetails [['MessageText']], round (weeklyTemperatureC, 1), 
                            round (weeklyTemperatureF, 1))
      
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 7)) * 60 * 60
    }
    if (HOTDAY) {
      postDetails <- getPostDetails ('hotDay')
      rank <- head (tail (dailyAirt [['rank']], n = 2), n = 1)
      message   <- sprintf (postDetails [['MessageText']], round (dailyTemperatureC, 1), 
                            round (dailyTemperatureF, 1), rank, findOrdinalSuffix (rank))
      
      # Expires at the end of the day
      delay <- 0
    }
    if (COLDDAY) {
      postDetails <- getPostDetails ('coldDay')
      rank <- head (tail (rank (dailyAirt [['airt']]), n = 2), n = 1)
      message <- sprintf (postDetails [['MessageText']], round (dailyTemperatureC, 1), 
                          round (dailyTemperatureF, 1), rank, findOrdinalSuffix (rank))
      
      # Expires at the end of the day
      delay <- 0
    }
    if (HOTTESTDAY) {
      postDetails <- getPostDetails ('hottestDay')
      message   <- sprintf (postDetails [['MessageText']], round (dailyTemperatureC, 1), 
                            round (dailyTemperatureF, 1))
      # Expires at the end of the day
      delay <- 0
    }
    if (COLDESTDAY) {
      pastTemperatureC  <- tail (head (sort (airt [['airt']]), n = 2), n = 1)
      pastTemperatureF  <- CtoF (pastTemperatureC)
      temp <- max (airt [['day']] [airt [['airt']] == pastTemperatureC], na.rm = T)
      dateOfLastColdest <- sprintf ('%s %s', lubridate::month (temp, label = T, abbr = F), 
                                    lubridate::year (temp)) 
      postDetails <- getPostDetails ('coldestDay')
      message   <- sprintf (postDetails [['MessageText']], dateOfLastColdest, 
                            round (pastTemperatureC, 1), round (pastTemperatureF, 1), 
                            round (dailyTemperatureC, 1), round (dailyTemperatureF, 1)) # TR still nedds work
      
      # Expires at the end of the day
      delay <- 0
    }  
    if (HOT){
      postDetails <- getPostDetails ('hot')
      rank <- tail (airt [['rank']], n = 1)
      message    <- sprintf (postDetails [['MessageText']], 
                             round (temperatureC, 1),
                             round (temperatureF, 1), 
                             rank, findOrdinalSuffix (rank))
      # Expires after delay (s)
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 7)) * 60 * 60
    }
    if (COLD){
      postDetails <- getPostDetails ('cold')
      rank <- tail (rank (airt [['airt']]), n = 1)
      message    <- sprintf (postDetails [['MessageText']], 
                             round (temperatureC, 1),
                             round (temperatureF, 1), 
                             rank, findOrdinalSuffix (rank))
      # Expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 7)) * 60 * 60
    }
    if (HOTTEST) {
      postDetails <- getPostDetails ('hottest')
      message    <- sprintf (postDetails [['MessageText']], round (temperatureC, 1), 
                             round (temperatureF, 1))
      
      # Determine delay (s)
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 7)) * 60 * 60
    }
    if (COLDEST) {
      postDetails <- getPostDetails ('coldest')
      message   <- sprintf (postDetails [['MessageText']], round (temperatureC, 1), 
                            round (temperatureF, 1))
      
      # expires after delay
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 7)) * 60 * 60
    }
    ptable <- add_row (ptable, 
                       priority    = postDetails [["Priority"]], 
                       figureName  = postDetails [["FigureName"]], 
                       fFigure     = postDetails [["fFigure"]],
                       message     = message, 
                       hashtags    = postDetails [["Hashtags"]], 
                       expires     = expiresIn (delay = delay))
  } 
  
  # return the table with posts
  #--------------------------------------------------------------------------------------
  return (ptable)
} 

# check for extreme Precipitation events, so either a lot of rain or very little
#--------------------------------------------------------------------------------------
checkExtremePrecipitation <- function (ptable, TEST = 0) {
  
  # check whether the yesterday was the wettest day or a top 50 wettest day on record
  #--------------------------------------------------------------------------------------
  if (max (dailyPrec [['prec']], na.rm = T) <= head (tail (dailyPrec [['prec']], n = 2), n = 1) | 
      TEST == 1) {
    WETTESTDAY <- TRUE
    WETDAY     <- FALSE
  } else if (head (tail (dailyPrec [['rank']], n = 2), n = 1) <= 50 | TEST == 2) {
    WETTESTDAY <- FALSE
    WETDAY     <- TRUE
  } else {
    WETTESTDAY <- FALSE
    WETDAY     <- FALSE
  }
  
  # check whether the last week was the wettest week or a top 30 wettest week on record
  #----------------------------------------------------------------------------------------
  if (max (weeklyPrec [['prec']], na.rm = T) <= head (tail (weeklyPrec [['prec']], n = 2), n = 1) | 
      TEST == 3) {
    WETTESTWEEK <- TRUE
    WETWEEK     <- FALSE
  } else if (head (tail (weeklyPrec [['rank']], n = 2),  n = 1) <= 30 | TEST == 4) {
    WETTESTWEEK <- FALSE
    WETWEEK     <- TRUE
  } else {
    WETTESTWEEK <- FALSE
    WETWEEK     <- FALSE
  }
  
  # check whether the last month was the wettest month or a top 20 wettest month on record
  #--------------------------------------------------------------------------------------
  if (max (monthlyPrec [['prec']], na.rm = T) <= head (tail (monthlyPrec [['prec']], n = 2), n = 1) | 
      TEST == 5) {
    WETTESTMONTH <- TRUE
    WETMONTH     <- FALSE
  } else if (head (tail (monthlyPrec [['rank']], n = 2), n = 1) <= 20 | TEST == 6) {
    WETTESTMONTH <- FALSE
    WETMONTH     <- TRUE
  } else {
    WETTESTMONTH <- FALSE
    WETMONTH     <- FALSE
  }
  
  # check whether the last year was the wettest year or a top 10 wettest year on record
  #--------------------------------------------------------------------------------------
  if (max (yearlyPrec [['prec']], na.rm = T) <= head (tail (yearlyPrec [['prec']], n = 2), n = 1) | 
      TEST == 7) {
    WETTESTYEAR <- TRUE
    WETYEAR     <- FALSE
  } else if (head (tail (yearlyPrec [['rank']], n = 2), n = 1) <= 10 | TEST == 8) {
    WETTESTYEAR <- FALSE
    WETYEAR     <- TRUE
  } else {
    WETTESTYEAR <- FALSE
    WETYEAR     <- FALSE
  }
  
  # check whether the last month was the driest month or a top 20 driest month on record
  #----------------------------------------------------------------------------------------
  tempRank <- rank (monthlyPrec [['prec']])
  if (min (monthlyPrec [['prec']], na.rm = T) >= head (tail (monthlyPrec [['prec']], n = 2), n = 1) | 
      TEST == 9) {
    DRIESTMONTH <- TRUE
    DRYMONTH    <- FALSE
  } else if (head (tail (tempRank, n = 2), n = 1) <= 20 | TEST == 10) {
    DRIESTMONTH <- FALSE
    DRYMONTH    <- TRUE
  } else {
    DRIESTMONTH <- FALSE
    DRYMONTH    <- FALSE
  }
  
  # check whether the last year was the driest year or a top 10 driest year on record
  #----------------------------------------------------------------------------------------
  tempRank <- rank (yearlyPrec [['prec']])
  if (min (yearlyPrec [['prec']], na.rm = T) >= head (tail (yearlyPrec [['prec']], n = 2), n = 1) | 
      TEST == 11) {
    DRIESTYEAR <- TRUE
    DRYYEAR    <- FALSE
  } else if (head (tail (tempRank, n = 2), n = 1) <= 10 | TEST == 12) {
    DRIESTYEAR <- FALSE
    DRYYEAR    <- TRUE
  } else {
    DRIESTYEAR <- FALSE
    DRYYEAR    <- FALSE
  }
  
  # get the appropriate message 
  # messages are overwritten in order of priority with dry conditions being prioritised 
  # over wet conditions and instantaneous events being prioritised over longer-term events
  # (e.g., day > year). There will be more opportunities to post about a longer-term events
  #----------------------------------------------------------------------------------------
  if (WETDAY | WETTESTDAY | WETWEEK | WETTESTWEEK | WETMONTH | WETTESTMONTH | WETYEAR | 
      WETTESTYEAR | DRYMONTH | DRIESTMONTH | DRYYEAR | DRIESTYEAR) {
    if (WETYEAR) {
      postDetails <- getPostDetails ('wetYear')
      rank <- head (tail (yearlyPrec [['rank']], n = 2), n = 1)
      message <- sprintf (postDetails [['MessageText']], year (Sys.Date())-1,
                          rank, findOrdinalSuffix (rank), treeLocationName)
      
      # Expires after 19 days
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
    }
    if (DRYYEAR) {
      postDetails <- getPostDetails ('dryYear')
      rank <- head (tail (rank (yearlyPrec [['prec']]), n = 2), n = 1)
      message <- sprintf (postDetails [['MessageText']], year (Sys.Date())-1, 
                          rank, findOrdinalSuffix (rank), 
                          round (mmtoInches (head (tail (yearlyPrec [['prec']], n = 2), n = 1)), 1),
                          round (head (tail (yearlyPrec [['prec']], n = 2), n = 1), 1),
                          year (Sys.Date ()))
      
      # Expires after 19 days
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
    }
    if (WETTESTYEAR) {
      postDetails <- getPostDetails ('wettestYear')
      message   <- sprintf (postDetails [['MessageText']], year (Sys.Date())-1,
                            round (mmtoInches (head (tail (yearlyPrec [['prec']], n = 2), n = 1)), 1),
                            round (head (tail (yearlyPrec [['prec']], n = 2), n = 1), 1))
      
      # Expires after 19 days
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
    }
    if (DRIESTYEAR) {
      postDetails <- getPostDetails ('driestYear')
      message <- sprintf (postDetails [['MessageText']], year (Sys.Date())-1, 
                          round (mmtoInches (head (tail (yearlyPrec [['prec']], n = 2), n = 1)), 1),
                          round (head (tail (yearlyPrec [['prec']], n = 2), n = 1), 1),
                          year (Sys.Date ()))
      
      # Expires after 19 days
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
    }
    if (WETMONTH) {
      postDetails <- getPostDetails ('wetMonth')
      rank <- head (tail (monthlyPrec [['rank']], n = 2), n = 1)
      message   <- sprintf (postDetails [['MessageText']], treeLocationName,
                            round (mmtoInches (head (tail (monthlyPrec [['prec']], n = 2), n = 1)), 1),
                            round (head (tail (monthlyPrec [['prec']], n = 2), n = 1), 1),
                            rank, findOrdinalSuffix (rank))
      
      # Expires after 19 days
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
    }
    if (DRYMONTH) {
      postDetails <- getPostDetails ('dryMonth')
      rank <- head (tail (rank (monthlyPrec [['prec']]), n = 2), n = 1)
      message <- sprintf (postDetails [['MessageText']], treeLocationName, 
                          rank, findOrdinalSuffix (rank), 
                          round (mmtoInches (head (tail (monthlyPrec [['prec']], n = 2), n = 1)), 2),
                          round (head (tail (monthlyPrec [['prec']], n = 2), n = 1), 2))
      
      # Expires after 19 days
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
    }
    if (WETTESTMONTH) {
      postDetails <- getPostDetails ('wettestMonth')
      message   <- sprintf (postDetails [['MessageText']], 
                            round (mmtoInches (head (tail (monthlyPrec [['prec']], n = 2), n = 1)), 1),
                            round (head (tail (monthlyPrec [['prec']], n = 2), n = 1), 1))
      
      # Expires after 19 days
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
    }
    if (DRIESTMONTH) {
      postDetails <- getPostDetails ('driestMonth')
      message <- sprintf (postDetails [['MessageText']], 
                          round (mmtoInches (head (tail (monthlyPrec [['prec']], n = 2), n = 1)), 2),
                          round (head (tail (monthlyPrec [['prec']], n = 2), n = 1), 2))
      
      # Expires after 19 days
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
    }
    if (WETWEEK) {
      postDetails <- getPostDetails ('wetWeek')
      rank <- head (tail (weeklyPrec [['rank']], n = 2), n = 1)
      message   <- sprintf (postDetails [['MessageText']], 
                            round (mmtoInches (mean (weeklyPrec [['prec']], na.rm = TRUE)), 3),
                            round (mean (weeklyPrec [['prec']], na.rm = TRUE), 2),
                            treeLocationName,
                            round (mmtoInches (head (tail (weeklyPrec [['prec']], n = 2), n = 1)), 3),
                            round (head (tail (weeklyPrec [['prec']], n = 2), n = 1), 2),
                            rank, findOrdinalSuffix (rank))
      
      # Expires after six days
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
    }
    if (WETTESTWEEK) {
      postDetails <- getPostDetails ('wettestWeek')
      message   <- sprintf (postDetails [['MessageText']], 
                            round (mmtoInches (head (tail (weeklyPrec [['prec']], n = 2), n = 1)), 3),
                            round (head (tail (weeklyPrec [['prec']], n = 2), n = 1), 2))
      
      # Expires after six days
      delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
    }
    if (WETDAY) {
      postDetails <- getPostDetails ('wetDay')
      rank <- head (tail (dailyPrec [['rank']], n = 2), n = 1)
      message   <- sprintf (postDetails [['MessageText']], 
                            rank, findOrdinalSuffix (rank),
                            round (mmtoInches (head (tail (dailyPrec [['prec']], n = 2), n = 1)), 3),
                            round (head (tail (dailyPrec [['prec']], n = 2), n = 1), 2))
      
      # Expires at the end of the day
      delay <- 0
    } 
    if (WETTESTDAY) {
      postDetails <- getPostDetails ('wettestDay')
      message   <- sprintf (postDetails [['MessageText']], 
                            round (mmtoInches (head (tail (dailyPrec [['prec']], n = 2), n = 1)), 3),
                            round (head (tail (dailyPrec [['prec']], n = 2), n = 1), 2))
      
      # Expires at the end of the day
      delay <- 0
    }
    
    # compile post details
    #------------------------------------------------------------------------------------
    ptable    <- add_row (ptable,
                          priority = postDetails [["Priority"]],
                          fFigure  = postDetails [["fFigure"]],
                          message  = message,
                          hashtags = postDetails [["Hashtags"]],
                          expires  = expiresIn (delay = delay))
  }
  
  # return the table with posts
  #--------------------------------------------------------------------------------------
  return (ptable)
}

# summarise and compare last month's climate at the beginning of the month
#----------------------------------------------------------------------------------------
monthlyClimateSummary <- function (ptable, TEST = 0) {
  
  # check whether it is the first day of the month
  #--------------------------------------------------------------------------------------
  if (substring (Sys.Date (), 9, 10) == '01' & substring (Sys.time (), 12, 13) == '13'| TEST >= 1) {
    
    # calculate mean and standard deviation for monthly temperature for all months such as the previous (i.e. May) 
    #--------------------------------------------------------------------------------------
    acMonthlyAirt <- head (tail (monthlyAirt [['airt']], n = 2), n = 1) 
    meMonthlyAirt <- mean (monthlyAirt [['airt']] [month (monthlyAirt [['month']]) == month (Sys.Date ()) - 1], na.rm = T)
    sdMonthlyAirt <- sd   (monthlyAirt [['airt']] [month (monthlyAirt [['month']]) == month (Sys.Date ()) - 1], na.rm = T)
    diMonthlyAirt <- acMonthlyAirt - meMonthlyAirt
    
    # calculate mean and standard deviation for monthly temperature for all months such as the previous (i.e. May) 
    #--------------------------------------------------------------------------------------
    acMonthlyPrec <- head (tail (monthlyPrec [['prec']], n = 2), n = 1) 
    meMonthlyPrec <- mean (monthlyPrec [['prec']] [month (monthlyPrec [['month']]) == month (Sys.Date ()) - 1], na.rm = T)
    sdMonthlyPrec <- sd   (monthlyPrec [['prec']] [month (monthlyPrec [['month']]) == month (Sys.Date ()) - 1], na.rm = T)
    diMonthlyPrec <- acMonthlyPrec - meMonthlyPrec
    
    # determine rainy days in previous month
    #--------------------------------------------------------------------------------------
    rainyDays <- sum (dailyPrec [['prec']] [month (dailyPrec [['day']]) == month (Sys.Date ()) - 1 &
                                              year (dailyPrec [['day']]) == year (Sys.Date ())] != 0, 
                      na.rm = T)
    
    # get the monthly max temperature
    #--------------------------------------------------------------------------------------
    maxAirT <-max (dailyMaxAirt [['airt']] [month (dailyMaxAirt [['day']]) == month (Sys.Date ()) - 1 &
                                              year (dailyMaxAirt [['day']]) == year (Sys.Date ())], 
                   na.rm = T)
    
    # get previous month
    #--------------------------------------------------------------------------------------
    prMonth <- lubridate::month (month (Sys.Date ()) - 1, label = T, abbr = F)
    
    # Choose what to talk about
    #--------------------------------------------------------------------------------------
    if (diMonthlyAirt < sdMonthlyAirt & diMonthlyPrec < sdMonthlyPrec | TEST == 1) { # Just an close-to-average month 
      postDetails <- getPostDetails ('monthlyClimateSummary - normal')
      message        <- sprintf (postDetails [['MessageText']], round (acMonthlyAirt, 1), 
                                 round (CtoF (acMonthlyAirt), 1), round (acMonthlyPrec, 1), 
                                 treeLocationName, prMonth)
    }
    if (abs (diMonthlyAirt) >= sdMonthlyAirt | TEST == 2 | TEST == 3) { # Temperature was anormal
      if (diMonthlyAirt < 0 | TEST == 2) { # Cold month
        postDetails <- getPostDetails ('monthlyClimateSummary - cold')
        message        <- sprintf (postDetails [['MessageText']], round (meMonthlyAirt, 1), 
                                   round (CtoF (meMonthlyAirt), 1), round (-diMonthlyAirt, 1), 
                                   treeLocationName, prMonth)
      } else if (diMonthlyAirt > 0 | TEST == 3) { # Warm month
        postDetails <- getPostDetails ('monthlyClimateSummary - warm')
        message        <- sprintf (postDetails [['MessageText']], round (meMonthlyAirt, 1),
                                   round (CtoF (meMonthlyAirt), 1), prMonth, 
                                   round (diMonthlyAirt, 1), 
                                   round (CtoF (diMonthlyAirt, difference  = T), 1), 
                                   treeLocationName)
      }
    }
    if (abs (diMonthlyPrec) >= sdMonthlyPrec | TEST == 4  | TEST == 5) { # Precip was anormal
      if (diMonthlyPrec < 0  | TEST == 4) { # Dry month
        postDetails <- getPostDetails ('monthlyClimateSummary - dry')
        message        <- sprintf (postDetails [['MessageText']], 
                                   round (maxAirT, 1),
                                   round (CtoF (maxAirT), 1), 
                                   round (meMonthlyPrec, 1), 
                                   treeLocationName)
      } else if (diMonthlyAirt > 0 | TEST == 5) { # Wet month
        postDetails <- getPostDetails ('monthlyClimateSummary - wet')
        message        <- sprintf (postDetails [['MessageText']], 
                                   format (Sys.Date () - 1, '%d %B'), 
                                   '23:59h',
                                   round (acMonthlyPrec, 1), 
                                   round (mmtoInches (acMonthlyPrec)), 
                                   rainyDays,
                                   days_in_month (month (Sys.Date () - 1)),
                                   prMonth)
      }
    }
    delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7))
    expirDate <- sprintf ("%s 23:59:59 %s", 
                          format (Sys.Date () + delay, format = '%Y-%m-%d'), 
                          treeTimeZone) %>% lubridate::as_datetime (tz = treeTimeZone) 
    ptable    <- add_row (ptable, 
                          priority   = postDetails [["Priority"]], 
                          fFigure    = postDetails [["fFigure"]],
                          figureName = postDetails [["FigureName"]], 
                          message    = message, 
                          hashtags   = postDetails [["Hashtags"]], 
                          expires    = expirDate)
  }
  
  # return the table with posts
  #--------------------------------------------------------------------------------------
  return (ptable)
} 

# Summarise and compare last month's climate at the beginning of the month
#----------------------------------------------------------------------------------------
# monthlyClimateSummaryFollowUp <- function (ptable, TEST = 0) {
#   
#   # Check whether it is the seventh day of the month
#   #--------------------------------------------------------------------------------------
#   if (substring (Sys.Date (), 9, 10) == '07' & substring (Sys.time (), 12, 13) == '13'| TEST >= 1) {
#     
#     # Calculate mean and standard deviation for monthly temperature for all months such as 
#     # the previous (i.e. May) 
#     #--------------------------------------------------------------------------------------
#     acMonthlyAirt <- head (tail (monthlyAirt [['airt']], n = 2), n = 1) 
#     meMonthlyAirt <- mean (monthlyAirt [['airt']] [month (monthlyAirt [['month']]) == month (Sys.Date ()) - 1])
#     diMonthlyAirt <- acMonthlyAirt - meMonthlyAirt
#     
#     # determine rainy days in previous month
#     #--------------------------------------------------------------------------------------
#     rainyDays <- sum (dailyPrec [['prec']] [month (dailyPrec [['day']]) == month (Sys.Date ()) - 1 &
#                                               year (dailyPrec [['day']]) == year (Sys.Date ())] != 0, 
#                       na.rm = T)
# 
#     # Calculate mean and standard deviation for monthly temperature for all months such as 
#     # the previous (i.e. May) 
#     #--------------------------------------------------------------------------------------
#     baseline <- month (monthlyAirt [['month']]) == month (Sys.Date ()) - 1 &
#                 year  (monthlyAirt [['month']]) >= 1964                    &
#                 year  (monthlyAirt [['month']]) <= 1993
#     meMonthlyAirtBase <- mean (monthlyAirt [['airt']] [baseline])
#     diMonthlyAirtBase <- acMonthlyAirt - meMonthlyAirtBase
#     
#     # Get previous month
#     #--------------------------------------------------------------------------------------
#     prMonth <- month (month (Sys.Date ()) - 1, label = T, abbr = F)
#     
#     # Choose what to talk about
#     #--------------------------------------------------------------------------------------
#     if (diMonthlyAirt < diMonthlyAirtBase | TEST == 1) { 
#       postDetails <- getPostDetails ('monthlyClimateSummaryFollowUp - warmer')
#       message        <- sprintf (postDetails [['MessageText']], round (acMonthlyAirt, 1), 
#                                  round (CtoF (acMonthlyAirt), 1), round (acMonthlyPrec, 1), 
#                                  treeLocationName, prMonth)
#     } else if (abs (diMonthlyAirt) >= sdMonthlyAirt | TEST == 2 | TEST == 3) { # Temperature was anormal
#       if (diMonthlyAirt < 0 | TEST == 2) { # Cold month
#         postDetails <- getPostDetails ('monthlyClimateSummary - cold')
#         message        <- sprintf (postDetails [['MessageText']], round (meMonthlyAirt, 1), 
#                                    round (CtoF (meMonthlyAirt), 1), round (-diMonthlyAirt, 1), 
#                                    treeLocationName, prMonth)
#       } else if (diMonthlyAirt > 0 | TEST == 3) { # Warm month
#         postDetails <- getPostDetails ('monthlyClimateSummary - warm')
#         message        <- sprintf (postDetails [['MessageText']], round (meMonthlyAirt, 1),
#                                    round (CtoF (meMonthlyAirt), 1), prMonth, 
#                                    round (diMonthlyAirt, 1), 
#                                    round (CtoF (diMonthlyAirt, difference  = T), 1), 
#                                    treeLocationName)
#       }
#     } else if (abs (diMonthlyPrec) >= sdMonthlyPrec | TEST == 4  | TEST == 5) { # Precip was anormal
#       if (diMonthlyPrec < 0  | TEST == 4) { # Dry month
#         postDetails <- getPostDetails ('monthlyClimateSummary - dry')
#         message        <- sprintf (postDetails [['MessageText']], 
#                                    round (maxAirT, 1),
#                                    round (CtoF (maxAirT), 1), 
#                                    round (meMonthlyPrec, 1), 
#                                    treeLocationName)
#       } else if (diMonthlyAirt > 0 | TEST == 5) { # Wet month
#         postDetails <- getPostDetails ('monthlyClimateSummary - wet')
#         message        <- sprintf (postDetails [['MessageText']], 
#                                    format (Sys.Date () - 1, '%d %B'), 
#                                    '23:59h',
#                                    round (acMonthlyPrec, 1), 
#                                    round (mmtoInches (acMonthlyPrec)), 
#                                    rainyDays,
#                                    days_in_month (month (Sys.Date () - 1)),
#                                    prMonth)
#       }
#     }
#     delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7))
#     expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date () + delay, format = '%Y-%m-%d'), treeTimeZone) 
#     ptable    <- add_row (ptable, 
#                           priority   = postDetails [["Priority"]], 
#                           fFigure    = postDetails [["fFigure"]],
#                           figureName = postDetails [["FigureName"]], 
#                           message    = message, 
#                           hashtags   = postDetails [["Hashtags"]], 
#                           expires    = expirDate)
#   }
#   # return the table with posts
#   #--------------------------------------------------------------------------------------
#   return (ptable)
# } 


# Summarise and compare last year's climate at the beginning of the year
#----------------------------------------------------------------------------------------
annualClimateSummary <- function (ptable, TEST = 0) {
  
  # Check whether it is the first day of the year
  #---------------------------------------------------------------------------------------#
  if (substring (Sys.Date (), 6, 10) == '01-01' & substring (Sys.time (), 12, 15) == '12:0'| 
      TEST >= 1) {
    
    # Calculate mean and standard deviation for monthly temperature for all months such as the previous (i.e. May) 
    acYearlyAirt <- head (tail (yearlyAirt [['airt']], n = 2), n = 1) 
    meYearlyAirt <- mean (yearlyAirt [['airt']] [2:31]) # Compare to 1964-1993 mean
    sdYearlyAirt <- sd   (yearlyAirt [['airt']] [2:31])
    diYearlyAirt <- acYearlyAirt - meYearlyAirt
    
    # Calculate mean and standard deviation for monthly temperature for all months such as the previous (i.e. May) 
    acYearlyPrec <- head (tail (yearlyPrec [['prec']], n = 2), n = 1) 
    meYearlyPrec <- mean (yearlyPrec [['prec']] [2:31])
    sdYearlyPrec <- sd   (yearlyPrec [['prec']] [2:31])
    diYearlyPrec <- acYearlyPrec - meYearlyPrec
    
    # Choose what to talk about
    if (diYearlyAirt < sdYearlyAirt & diYearlyPrec < sdYearlyPrec | TEST == 1) { # Just an close-to-average Year 
      postDetails <- getPostDetails ('annualClimateSummary - normal')
      message        <- sprintf (postDetails [['MessageText']], round (acYearlyAirt, 1), round (acYearlyPrec, 1), year (Sys.Date ()) - 1)
    } else if (abs (diYearlyAirt) >= sdYearlyAirt | TEST == 2 | TEST == 3) { # Temperature was anormal
      if (diYearlyAirt < 0 | TEST == 2) { # Cold Year
        postDetails <- getPostDetails ('annualClimateSummary - cold')
        message        <- sprintf (postDetails [['MessageText']], round (meYearlyAirt, 1), round (-diYearlyAirt, 1), prYear)
      } else if (diYearlyAirt > 0 | TEST == 3) { # Warm Year
        postDetails <- getPostDetails ('annualClimateSummary - warm')
        message        <- sprintf (postDetails [['MessageText']], round (meYearlyAirt, 1), round (-diYearlyAirt, 1), prYear)
      }
    } else if (abs (diYearlyPrec) >= sdYearlyPrec | TEST == 4  | TEST == 5) { # Precip was anormal
      if (diYearlyPrec < 0  | TEST == 4) { # Dry Year
        postDetails <- getPostDetails ('annualClimateSummary - dry')
        message        <- sprintf (postDetails [['MessageText']], round (meYearlyPrec, 1), round (-diYearlyPrec, 1), prYear)
      } else if (diYearlyAirt > 0 | TEST == 5) { # Wet Year
        postDetails <- getPostDetails ('annualClimateSummary - wet')
        message        <- sprintf (postDetails [['MessageText']], round (meYearlyPrec, 1), round (-diYearlyPrec, 1), prYear)
      }
    }
    delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,8))
    expirDate <- sprintf ("%s 23:59:59 %s", format (Sys.Date () + delay, format = '%Y-%m-%d'), treeTimeZone) 
    ptable    <- add_row (ptable, 
                          priority   = postDetails [["Priority"]], 
                          fFigure    = postDetails [["fFigure"]],
                          figureName = postDetails [["FigureName"]], 
                          message    = message, 
                          hashtags   = postDetails [["Hashtags"]], 
                          expires    = expirDate)
  }
  
  return (ptable)
}

# check for first frost event of the year and late frosts during the growing season
#----------------------------------------------------------------------------------------
checkFrost <- function (ptable, TEST = 0) {
  
  # was there a frost today (calendar day)?
  #--------------------------------------------------------------------------------------
  if (min (airt [['airt']] [airt [['day']] == Sys.Date ()], na.rm = TRUE) < 0.0) {
  
    # by default there is no frost-related message
    #------------------------------------------------------------------------------------
    FROST <- FALSE
    
    # was this the first frost of the autumn (after July)
    #------------------------------------------------------------------------------------
    memory <- read_csv (paste0 (path, 'code/memory.csv'), col_types = cols ())
    if ((substring (Sys.Date (), 6, 10) >= '07-31' & memory [['autumnFrosts']] < 2) | 
        TEST == 1) {
      postDetails <- getPostDetails ('checkFrost - first')
      FROST <- TRUE
    } else if (substring (Sys.Date (), 6, 10) >= '07-31') {
      memory [['autumnFrosts']] <- memory [['autumnFrosts']] + 1
      write_csv (memory, file = paste0 (path, 'code/memory.csv'))
    }
    
    # reset the autumnal frost counter on the 1st of January each year 
    #------------------------------------------------------------------------------------
    if (lubridate::month (Sys.Date ()) == 1 & lubridate::day (Sys.Date ()) == 1) {
      memory [['autumnFrost']] <- 0
    } 
    
    # check for late frosts (after April and with at least three preceding frost-free days)
    #------------------------------------------------------------------------------------
    if ((substring (Sys.Date (), 6, 10) >= '05-01' & # after April
         substring (Sys.Date (), 6, 10) <= '08-01' & # before August
         # below checks whether there was no frost in preceding three days
         sum (airt [['airt']] [airt [['day']] >= format (Sys.Date () - 3, '%Y-%m-%d') & 
                               airt [['day']] <  format (Sys.Date (),     '%Y-%m-%d') & 
                               !is.na  (airt [['airt']])                              &
                               !is.nan (airt [['airt']])] <= 0.0, na.rm = T) < 1) | 
        TEST == 2) {                                        # or we are testing
      postDetails <- getPostDetails ('checkFrost - late')
      FROST <- TRUE
    }
    
    # remainder only needs running if we have a frost-related message
    #------------------------------------------------------------------------------------
    if (FROST) {
    # determine number of preceding frost-free days
    #------------------------------------------------------------------------------------
      frostFreeDays <- 0 
      NOFROST <- TRUE
      while (NOFROST) { # go back in time
        # select only temperatures during the day prior to the last checked day
        temps <- airt [['airt']] [airt [['day']] >= format (Sys.Date () - (frostFreeDays), '%Y-%m-%d')]
        if (sum (temps < 0.0, na.rm = T) > 0.0) {
          NOFROST = FALSE
        } else {
          frostFreeDays <- frostFreeDays + 1
        }
      } 
      
      # compose post details
      #----------------------------------------------------------------------------------
      message   <- sprintf (postDetails [['MessageText']],  frostFreeDays) 
      delay     <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7))
      expirDate <- sprintf ("%s 23:59:59 %s", 
                            format (Sys.Date () + delay, format = '%Y-%m-%d'), 
                            treeTimeZone) %>% lubridate::as_datetime (tz = treeTimeZone)
      ptable    <- add_row (ptable, 
                            priority    = postDetails [["Priority"]], 
                            fFigure     = postDetails [['fFigure']],
                            figureName  = postDetails [["FigureName"]], 
                            message     = message, 
                            hashtags    = postDetails [["Hashtags"]], 
                            expires     = expirDate)
    }
  }
  
  # return the appropriate posts details
  #--------------------------------------------------------------------------------------
  return (ptable)
}

# check for a heatwave
#----------------------------------------------------------------------------------------
checkHeatWave <- function (ptable, TEST = 0) {
  
  # define heatwave threshold
  #-------------------------------------------------------------------------------------#
  # here a heatwave is define as more than two days that have daily maximum temperatures
  # exceeding the 90th percentile of the Fisher Meterological Station record (Shaler only 
  # had daily temperatures, thus the mean and max would be identical) for the current month.
  #-------------------------------------------------------------------------------------#
  percentile90th <- quantile (dailyMaxAirt [['airt']] [as.numeric (substring (dailyMaxAirt [['day']], 1, 4)) > 2002 &
                                                         month (dailyMaxAirt [['day']]) == month (Sys.Date ())],
                              probs = 0.9, na.rm = T)
  
  # check for a heatwave 
  #-------------------------------------------------------------------------------------#
  if ((tail (dailyMaxAirt [['airt']], n = 1)               > percentile90th & 
       head (tail (dailyMaxAirt [['airt']], n = 2), n = 1) > percentile90th) | TEST == 1) {
    
    # count days since the start of the heatwave
    #-------------------------------------------------------------------------------------#
    heatWaveDays <- 0
    HEATWAVE <- T
    while (HEATWAVE) {
      temp <- head (tail (dailyMaxAirt [['airt']], n = heatWaveDays+1), n = 1)  
      if (temp > percentile90th) {
        heatWaveDays <- heatWaveDays + 1
      } else {
        HEATWAVE <- F
      }
    }
    
    # parse message and expiration date
    #-------------------------------------------------------------------------------------#
    postDetails <- getPostDetails ('checkHeatWave')
    message   <- sprintf (postDetails [['MessageText']], heatWaveDays, lubridate::month (Sys.Date (), label = T, abbr = F)) 
    delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7))
    expirDate <- sprintf ("%s 23:59:59 %s", 
                          format (Sys.Date () + delay, format = '%Y-%m-%d'), 
                          treeTimeZone) %>% lubridate::as_datetime (tz = treeTimeZone)
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]], 
                          fFigure     = postDetails [["fFigure"]],
                          figureName  = postDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expirDate)
  }
  
  # return the appropriate post details
  return (ptable)
}

# Check for a windy day/storm (i.e. day with max windspeed above 15 m/s)
#----------------------------------------------------------------------------------------
checkStorm <- function (ptable, TEST = 0){
  
  # Set Storm boolean to FALSE by default
  #--------------------------------------------------------------------------------------
  STORM <- FALSE
  
  # Check whether the max wind speed for the day was above 15 m/s 
  #--------------------------------------------------------------------------------------
  if (tail (gust [['gust']], n = 1) > 15.0 | TEST == 1) {
    
    # Parse message and expiration date
    #------------------------------------------------------------------------------------
    postDetails <- getPostDetails ('checkStorm - windy')
    message   <- sprintf (postDetails [['MessageText']],  round (tail (gust [['gust']], n = 1), 1), 
                          round (tail (gust [['gust']], n = 1)*2.23694, 1), treeLocationName) 
    delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7))
    STORM <-TRUE
  }
  
  # Check whether rainfall during the storm was above 50 mm
  #--------------------------------------------------------------------------------------
  precIn24Hours <- sum (tail (prec [['prec']], n = 24 * 4), na.rm = T) 
  if (precIn24Hours > 50.0 | TEST == 2) {
    
    # Parse message and expiration date
    #------------------------------------------------------------------------------------
    postDetails <- getPostDetails ('checkStorm - wet')
    message   <- sprintf (postDetails [['MessageText']],  round (precIn24Hours / 10.0, 1), 
                          round (mmtoInches (precIn24Hours), 1)) 
    delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7))
    STORM <- TRUE
  }
  
  if (STORM) {
    expirDate <- sprintf ("%s 23:59:59 %s", 
                          format (Sys.Date () + delay, format = '%Y-%m-%d'), 
                          treeTimeZone) %>% lubridate::as_datetime (tz = treeTimeZone)
    ptable    <- add_row (ptable, 
                          priority    = postDetails [["Priority"]], 
                          fFigure     = postDetails [["fFigure"]],
                          figureName  = postDetails [["FigureName"]], 
                          message     = message, 
                          hashtags    = postDetails [["Hashtags"]], 
                          expires     = expirDate)
  }
  
  # Return post details to main script
  #--------------------------------------------------------------------------------------
  return (ptable)
}

# Check for rainfall in the last hour exceeding 3.0mm
#----------------------------------------------------------------------------------------
checkHourlyRainfall <- function (ptable, TEST = 0) {
  
  # Calculate rainfall in last hour
  #--------------------------------------------------------------------------------------
  lastHourPrec <- sum (tail (prec [['prec']], n = 4), na.rm = TRUE)
  
  # Check for pretty heavy rain (more than 1.5mm per fifteen minutes)
  #--------------------------------------------------------------------------------------
  if (lastHourPrec > 3.0 | TEST == 1) {
    
    # Get post details    
    #------------------------------------------------------------------------------------
    postDetails <- getPostDetails (fName = 'checkHourlyRainfall')
    if (substring (postDetails [['MessageText']], 1, 1) == 'I') {
      message <- sprintf (postDetails [['MessageText']], 
                          round (lastHourPrec, 2),
                          round (mmtoInches(lastHourPrec), 3))
    } else {
      message <- postDetails [['MessageText']]
    }
    delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7)) * 60.0 * 60.0
    expirDate <- sprintf ("%s %s", 
                          format (Sys.time () + delay, format = '%Y-%m-%d %H:%M:%S'), 
                          treeTimeZone) %>% lubridate::as_datetime (tz = treeTimeZone)
    ptable    <- add_row (ptable, 
                          priority    = postDetails [['Priority']], 
                          fFigure     = postDetails [['fFigure']],
                          figureName  = sprintf ('%s/tmp/dailyGrowth_%s.png',path,Sys.Date ()), 
                          message     = message, 
                          hashtags    = postDetails [['Hashtags']], 
                          expires     = expirDate)
  }
  
  # Return the post details
  #--------------------------------------------------------------------------------------
  return (ptable)
}

# Check for rainfall in the last day exceeding 20.0 mm
#----------------------------------------------------------------------------------------
checkDailyRainfall <- function (ptable, TEST = 0) {
  
  # Calculate rainfall in last hour
  #--------------------------------------------------------------------------------------
  lastDayPrec <- sum (tail (prec [['prec']], n = 4*24), na.rm = TRUE)
  
  # Check for pretty heavy rain (more than 20mm per day)
  #--------------------------------------------------------------------------------------
  if (lastDayPrec > 20.0 | TEST == 1) {
    
    # Load dependencies if necessary
    #------------------------------------------------------------------------------------
    if (!existsFunction ('cols')) library ('tidyverse')
    
    # Plot figure of growth and rainfall over the last two weeks
    #------------------------------------------------------------------------------------
    radGrowth <- calcRadialGrowth (temporalRes = 'daily', 
                                   pdm_calibration_path = dataPath, 
                                   PLOT = TRUE)
    
    # Get post details    
    #------------------------------------------------------------------------------------
    postDetails <- getPostDetails (fName = 'checkDailyRainfall')
    message <- sprintf (postDetails [['MessageText']], 
                        round (max (radGrowth [['dailyGrowth']], na.rm = TRUE), 2))
    delay <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7)) * 60.0 * 60.0 * 24.0
    expirDate <- sprintf ("%s %s", 
                          format (Sys.time () + delay, format = '%Y-%m-%d %H:%M:%S'), 
                          treeTimeZone) %>% lubridate::as_datetime (tz = treeTimeZone)
    ptable    <- add_row (ptable, 
                          priority    = postDetails [['Priority']], 
                          fFigure     = postDetails [['fFigure']],
                          figureName  = sprintf ('%s/tmp/dailyGrowth_%s.png',path,Sys.Date ()), 
                          message     = message, 
                          hashtags    = postDetails [['Hashtags']], 
                          expires     = expirDate)
  }
  
  # Return the post details
  #--------------------------------------------------------------------------------------
  return (ptable)
}
#=======================================================================================#