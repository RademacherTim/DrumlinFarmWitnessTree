#========================================================================================
# Script to generate responses for interactive functions              
#----------------------------------------------------------------------------------------
generateInteractiveResponses <- function(TEST = 0) {
  
  # create responses tibble -------------------------------------------------------------
  responses <- tibble(season = NA, topic = NA, reply = NA)
  
  # check air temperature and compare to the mean air temperature for a two hour window 
  # on this day of the year -------------------------------------------------------------
  airTemp <- tail(airt$airt, n = 1)
  meanAirTemp <- mean(airt$airt[yday(airt$day) == yday(Sys.Date()) & 
                                     hour(airt$datetime) > hour(Sys.time()) - 1 &
                                     hour(airt$datetime) < hour(Sys.time()) + 1],
                       na.rm = T)
  
  # check precipitation -----------------------------------------------------------------
  # precip <- sum(tail(dailyPrec$prec, n = 14), na.rm = T) # biweekly sum
  # from <- head(tail(dailyPrec$day, n = 14), n = 1)
  # for (iYr in 1964:year(Sys.Date ())) {
  #   sum14 <- sum(dailyPrec$prec'[year(dailyPrec$day) == iYr & 
  #                                yday(dailyPrec$day) >= yday(from) & 
  #                                    yday(dailyPrec$day) <= yday(Sys.Date())],
  #                na.rm = T)
  #   if (iYr == 1964) {
  #     meanPrecip <- tibble(year = iYr, mean = sum14)
  #   } else {
  #     meanPrecip <- add_row(meanPrecip, year = iYr, mean = sum14)
  #   }
  # }
  # meanPrecip <- mean(meanPrecip[['mean']], na.rm = T)
  
  # determine whether it is the growing season or not -----------------------------------
  if (Sys.Date() > as.POSIXct('15-03', format = '%d-%m') & 
      Sys.Date() < as.POSIXct('15-10', format = '%d-%m')) { # growing season
    
    # set season to growing season ------------------------------------------------------
    season <- 'growing season'
    
    # get responses based on temperature ------------------------------------------------
    if (round(airTemp) > round(meanAirTemp)) { # hot
      postDetails <- getPostDetails('generateInteractivity - growing season - hot')
    } else if (round(airTemp) == round(meanAirTemp)) { # average
      postDetails <- getPostDetails('generateInteractivity - growing season - average temperature')
    } else if (airTemp < meanAirTemp) { # cold
      postDetails <- getPostDetails('generateInteractivity - growing season - cold')
    }
    postDetails$MessageText <- sprintf(postDetails$MessageText, round(airTemp, 1),
                                       round (CtoF(airTemp), 1))
    responses <- add_row(responses, 
                         season = season, 
                         topic = 'temperature', 
                         reply = postDetails$MessageText)
    
    # get responses based on precipitation ----------------------------------------------
    # if (round(precip) > round(meanPrecip)) { # wet
    #   postDetails <- getPostDetails('generateInteractivity - growing season - wet')
    # } else if (round(precip) == round(meanPrecip)) { # average
    #   postDetails <- getPostDetails('generateInteractivity - growing season - average precipitation')
    # } else if (round(precip) < round(meanPrecip)) { # dry
    #   postDetails <- getPostDetails('generateInteractivity - growing season - dry')
    # }
    # responses <- add_row (responses, 
    #                       season = season, 
    #                       topic = 'rainfall', 
    #                       reply = postDetails[['MessageText']])
    
  } else { # off-season
    
    # set season to growing season
    #------------------------------------------------------------------------------------
    season <- 'off-season'
    
    # get responses based on temperature
    #------------------------------------------------------------------------------------
    if (round (airTemp) > round (meanAirTemp)) { # hot
      postDetails <- getPostDetails ('generateInteractivity - off season - hot')
    } else if (round (airTemp) == round (meanAirTemp)) { # average
      postDetails <- getPostDetails ('generateInteractivity - off season - average temperature')
    } else if (round (airTemp) < round (meanAirTemp)) { # cold
      postDetails <- getPostDetails ('generateInteractivity - off season - cold')
    }
    postDetails$MessageText <- sprintf(postDetails$MessageText, round(airTemp, 1),
                                              round (CtoF(airTemp), 1))
    responses <- add_row(responses, season = season, topic = 'temperature', 
                         reply = postDetails$MessageText)
    
    # get responses based on precipitation
    #------------------------------------------------------------------------------------
    # if (round (precip) > round (meanPrecip)) { # wet
    #   postDetails <- getPostDetails ('generateInteractivity - off season - wet')
    # } else if (round (precip) == round (meanPrecip)) { # average
    #   postDetails <- getPostDetails ('generateInteractivity - off season - average precipitation')
    # } else if (round (precip) < round (meanPrecip)) { # dry
    #   postDetails <- getPostDetails ('generateInteractivity - off season - dry')
    # }
    # responses <- add_row (responses, season = season, topic = 'rainfall', 
    #                       reply = postDetails [['MessageText']])
  }
  
  # delete first row in responses tibble
  #--------------------------------------------------------------------------------------
  responses <- responses[-1, ]
  
  # add response to hear a tree fall in the forest 
  #------------------------------------------------------------------------------------
  # postDetails <- getPostDetails('generateInteractivity - all-year - treeFall')
  # responses <-  add_row(responses, season = 'all-year', topic = 'can you hear a tree fall?',
  #                       reply = postDetails$MessageText, .before = 1)
  
  # add response to hottest and coldest day
  #------------------------------------------------------------------------------------
  postDetails <- getPostDetails('generateInteractivity - hottestDay')
  date <- dailyAirt$day [dailyAirt$rank == min (dailyAirt$rank, na.rm = TRUE)]
  # select one date if several days were equally warm
  if (length (date) > 1) {
    date <- sample (date, size = 1)
  }
  suffix <- findOrdinalSuffix (lubridate::day (date))
  dailyTemperatureC <- dailyAirt$airt [dailyAirt$day == date]
  message <- sprintf (postDetails$MessageText, 
                      lubridate::day (date), suffix, 
                      lubridate::month (date, label = TRUE, abbr = FALSE),
                      lubridate::year (date),  
                      round(dailyTemperatureC, 1),
                      round(CtoF (dailyTemperatureC), 1))
  responses <-  add_row(responses, season = 'all-year', topic = 'hottest day',
                        reply = message, .before = 1)
  postDetails <- getPostDetails('generateInteractivity - coldestDay')
  tempRank <- rank (dailyAirt$airt) 
  date <- dailyAirt$day [tempRank == min(tempRank, na.rm = TRUE)]
  # select one date if several days were equally cold
  if (length (date) > 1) {
    date <- sample (date, size = 1)
  }
  suffix <- findOrdinalSuffix (lubridate::day (date))
  dailyTemperatureC <- dailyAirt$airt [dailyAirt$day == date]
  message <- sprintf(postDetails$MessageText, 
                     lubridate::day (date), suffix, 
                     lubridate::month (date, label = TRUE, abbr = FALSE),
                     lubridate::year (date),  
                     round (dailyTemperatureC, 1),
                     round (CtoF (dailyTemperatureC), 1))
  responses <-  add_row(responses, season = 'all-year', topic = 'coldest day',
                        reply = message, .before = 1)
  
  # add response to "How old are you"
  #------------------------------------------------------------------------------------
  # postDetails <- getPostDetails('generateInteractivity - age')
  # message <-  sprintf(postDetails$MessageText, age, age + 1, coreImageLink)
  # responses <-  add_row(responses, season = 'all-year', topic = 'age',
  #                        reply = message, .before = 1)
  
  # add selfie response
  #------------------------------------------------------------------------------------
  # postDetails <- getPostDetails('generateInteractivity - selfie')
  # responses <- add_row(responses, season = 'all-year', topic = 'selfie', 
  #                      reply = postDetails$MessageText, .before = 1)
  
  # write messages to csv file
  #--------------------------------------------------------------------------------------
  write_csv(responses, file = paste0 (path, 'tmp/interactiveResponses.csv'))
  
  # return zero exit status to signal that it ran smoothly
  #--------------------------------------------------------------------------------------
  return (0)
}
#----------------------------------------------------------------------------------------
# - Should create better definition of the growing season than 15 March to 15 October
#========================================================================================