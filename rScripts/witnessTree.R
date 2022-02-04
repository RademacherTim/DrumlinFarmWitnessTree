#========================================================================================
# This is the main script running the witness tree bot. 
# See README.Rmd for more information.
#
# Version: 2.0.1
#
# Home repository: https://github.com/TTRademacher/HarvardForestWitnessTreeCode
#
# Project lead: Tim Rademacher (rademacher.tim@gmail.com)
#
# Acknowledgements: Thanks above all to Clarisse Hart and Taylor Jones! Thanks also to 
#                   David Basler, Clarisse Hart, Hannah Robbins, Kyle Wyche, 
#                   Shawna Greyeyes, Bijan Seyednasrollah for their invaluable 
#                   contributions.
#
# Last update: 2022-01-21
#
#----------------------------------------------------------------------------------------

# To-do list:
#----------------------------------------------------------------------------------------
# TR - Improve the weekly check-list
# TR - Pass facebook review and obtain pages access token to resume posting to facebook page
# TR - Add messages based on the snow pillow data from Harvard Forest
# TR - Install dendrometer and sapflow sensor
# TR - Reintegrate dendrometer and sapflow sensor into messaging
# TR      - It is needed for full functionality of checkDailyPrecipitation ()
# TR - Add stream gage data for Harvard Forest
#----------------------------------------------------------------------------------------

# get the absolute path to the directory including images and data 
#----------------------------------------------------------------------------------------
args = commandArgs (trailingOnly = TRUE)
if (length (args) == 0) {
  stop ("Error: At least one argument must be supplied (path to witnessTree directory).",
        call. = FALSE)
} else if (length (args) >= 1) {
  # default output file
  path       = args [1]
} else {
  stop ("Error: Too many command line arguments supplied to R.")
}

# output the paths at run-time to confirm that they were found
#----------------------------------------------------------------------------------------
print (paste0 (Sys.time (), '; rScritps: (1.1)  Path loaded: ',path,'.'))

# set the working directory and path to R scripts
#----------------------------------------------------------------------------------------
setwd (path)
rPath <- paste0 (path, 'code/rScripts/')

# load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('%>%'))     suppressPackageStartupMessages (library ('tidyverse'))
if (!existsFunction ('as_date')) suppressPackageStartupMessages (library ('lubridate'))

# source functions
#----------------------------------------------------------------------------------------
source  (paste0 (rPath, 'postHandling.R')) # TR - Sources fine but ought to check all functions
source  (paste0 (rPath, 'checkEvents.R'))   
source  (paste0 (rPath, 'checkClimate.R'))
#source  (paste0 (rPath, 'calcSapFlow.R'))    # TR - Needs sapflow sensor and data
#source  (paste0 (rPath, 'calcRadialGrowth.R')) # TR - Needs dendrometer
source  (paste0 (rPath, 'checkPhysiology.R')) 
source  (paste0 (rPath, 'checkPhenology.R'))
source  (paste0 (rPath, 'checkMorphology.R'))
source  (paste0 (rPath, 'checkCommunity.R'))
source  (paste0 (rPath, 'generateInteractivity.R'))
print (paste0 (Sys.time (), '; rScripts: (1.2)  Dependencies loaded.'))

# source basic data and stats for the trees
#----------------------------------------------------------------------------------------
source  (paste0 (rPath, 'treeStats.R'))
print (paste0 (Sys.time (), '; rScritps: (1.3)  Basic stats loaded.'))

# read in previously generated posts, if not first iteration
#----------------------------------------------------------------------------------------
if (file.exists (paste0 (path, 'posts/posts.csv'))) {
  posts <- read_csv (paste0 (path, 'posts/posts.csv'), 
                     col_names = T, col_types = c ('ilcccT'))
} else { # create a tibble for posts
  posts <- tibble (priority    = 0,  # priority of message to be posted (int; 
                   # between 0 for low and 10 for highest)
                   fFigure     = F,  # boolean whether it comes with a figure or not
                   figureName  = '', # text string with the figure name.  
                   message     = '', # the message itself (char) 
                   hashtags    = '', # hastags going with the message (char)
                   expires     = lubridate::as_datetime (Sys.Date ()) - 1e5) # expiration date of the message
  names (posts) <- c ('priority','fFigure','figureName','message','hashtags','expires')
}
print (paste0 (Sys.time (), '; rScritps: (1.4)  Previous messages read.'))

# purge expired posts
#----------------------------------------------------------------------------------------
posts <- checkExpirationDatesOf (posts)
print (paste0 (Sys.time (), '; rScritps: (1.5)  Expiration dates have been checked.'))

# re-evaluate priority of posts
#----------------------------------------------------------------------------------------
posts <- reEvaluatePriorityOf (posts)
print (paste0 (Sys.time (), '; rScritps: (1.6)  Priorities have been re-evaluated.'))

# generate new posts concerning regularly recurrent events
#----------------------------------------------------------------------------------------
posts <- helloWorld                     (posts) # on the launch date (2019-07-17) only
posts <- checkNewYears                  (posts) #  1st  of January
posts <- checkNationalWildLifeDay       (posts) #  4th  of March
posts <- checkPiDay                     (posts) #  14th of March
posts <- checkInternationalDayOfForests (posts) #  21st of March
posts <- checkWorldWaterDay             (posts) #  22nd of March
posts <- checkBirthday                  (posts) #  12th of April
posts <- checkArborDay                  (posts) #  Last Friday in April
posts <- checkMothersDay                (posts) #  Second Sunday in May
posts <- checkEarthDay                  (posts) #  22nd of April
posts <- checkSpringEquinox             (posts) # ~20th of March 
posts <- checkAutumnEquinox             (posts) # ~22nd of September
posts <- checkSummerSolstice            (posts) #  21st of June
posts <- checkWinterSolstice            (posts) #  21st of December
posts <- checkHalloween                 (posts) #  31st of October
posts <- monthlyEngagementReminder      (posts) #  2nd week of each month
print (paste0 (Sys.time (), '; rScritps: (1.7)  Events have been checked.'))

# generate new posts concerning leaf phenology
#----------------------------------------------------------------------------------------
#posts <- startOfGrowingSeason (posts)
#posts <- endOfGrowingSeason   (posts)

# generate new posts concerning meteorological & climatic events
#----------------------------------------------------------------------------------------
posts <- checkExtremeTemperatures  (posts) # Test whether it is particularly hot or cold
posts <- checkExtremePrecipitation (posts) # Test whether it is particularly wet or dry
posts <- monthlyClimateSummary     (posts) # Summarise and compare last month's climate 
                                           # to the long term average.
posts <- checkFrost (posts) # Check for first frost of autumn and late frost in the 
                            # early growing season.
posts <- checkHeatWave            (posts) # Check for a heat wave.
posts <- checkStorm               (posts) # Check for storm or rather a windy day.
posts <- checkHourlyPrecipitation (posts) # Check for hourly rainfall above 3.0mm.
# checkDailyPrecipitation relies on dendrometer, so I reduced its functionality for now.
posts <- checkDailyPrecipitation  (posts) # Check for daily rainfall above 20.0mm.
print (paste0 (Sys.time (), '; rScritps: (1.8)  Climatic conditions have been checked.'))

# generate new posts concerning the morphology of the tree
#----------------------------------------------------------------------------------------
posts <- explainDimensions (posts)

# generate new posts concerning the community surrounding the tree
#----------------------------------------------------------------------------------------
posts <- explainSeedDispersal      (posts) # give background on seed dispersal between 
                                           # 1st of September and end of November
posts <- explainGypsyMothHerbivory (posts) # give background on gypsy moths between 15th 
                                           # of May and end of August
posts <- explainGallWasps          (posts) # give background about galls
posts <- checkCommunityWildlife    (posts) # post about visitors from the wildlife camera  
print (paste0 (Sys.time (), '; rScritps: (1.9)  Community related messages have been checked.'))

# generate new posts concerning physiology
#----------------------------------------------------------------------------------------
#posts <- monthlyRadGrowthSummary (posts) # TR - Needs dendrometer data 
#posts <- checkWoodGrowthUpdate   (posts) # TR - Needs dendrometer data
posts <- checkWaxyCuticle        (posts)
print (paste0 (Sys.time (), '; rScritps: (1.10) Physiological conditions have been checked.'))

# generate new posts concerning phenology
#----------------------------------------------------------------------------------------
posts <- checkLeafColourChange (posts) # Verify leaf colour change from phenocam
print (paste0 (Sys.time (), '; rScritps: (1.11) Phenological conditions have been checked.'))

# generate interactive responses
#----------------------------------------------------------------------------------------
IOStatus <- generateInteractiveResponses ()
if (IOStatus != 0) {
  stop ('Error: Interactive responses were not generated properly!') 
} else {
  print (paste0 (Sys.time (), '; rScritps: (1.12) Interactive responses were generated.'))
}

# delete posts that have already been posted within the last two weeks
#----------------------------------------------------------------------------------------
posts <- deletePostedPostsAndRemoveDuplicates (posts) 

# selection of post, figure and images for the current iterations
#----------------------------------------------------------------------------------------
post <- selectPost (posts)
print (paste0 (Sys.time (), '; rScritps: (1.13) A post has been selected.'))

# check whether there is a post
#----------------------------------------------------------------------------------------
if (dim (post) [1] == 1) {
  
  # delete the selected post from the posts tibble 
  #--------------------------------------------------------------------------------------
  posts <- deletePost (posts, post)
  
  # check whether the bot has already posted four messages last week
  #--------------------------------------------------------------------------------------
  pastPostDates <- as.POSIXct (list.files (sprintf ('%s/posts/', path), 
                                           pattern = '.csv'),
                               format = "%Y-%m-%d_%H")
  numberOfPostsLastWeek <- length (pastPostDates [pastPostDates > Sys.Date () - 7 & 
                                                    !is.na  (pastPostDates)       &
                                                    !is.nan (pastPostDates)])
  lastPostDateTime <- tail (pastPostDates [!is.na (pastPostDates)], n = 1)
  
  # check whether there was a post in the past week at all
  #--------------------------------------------------------------------------------------
  if (length (lastPostDateTime) == 0) lastPostDateTime <- Sys.time () - 7 * 24 * 60 * 60
  
  # check whether the bot has already posted seven messages in the last week
  #--------------------------------------------------------------------------------------
  if (numberOfPostsLastWeek >= 7) { 

    # add post back to posts tibble, as it will not be posted right now
    #------------------------------------------------------------------------------------
    posts <- rbind (posts, post)
    print (paste0 (Sys.time (), '; Warning: We already had more than 7 posts in the last 7 days!'))
    
  # check whether the bot has posted in the last four hours
  #--------------------------------------------------------------------------------------
  } else if (lubridate::as.duration (Sys.time () - lastPostDateTime) / 
             lubridate::dhours (1) < 4.0 &
             post [['priority']] != 10) {
    # add post back to posts tibble, as it will not be posted right now
    #------------------------------------------------------------------------------------
    posts <- rbind (posts, post)
    print (paste0 (Sys.time (), '; Warning: The last post was less than four hours ago!'))
    
    # write post to posts/ folder named after date and time when it should be scheduled 
    #------------------------------------------------------------------------------------
  } else {
    
    # double check that there is a post
    #------------------------------------------------------------------------------------
    if (dim (post) [1] > 0) {
      write_csv (x    = post,
                 file = sprintf ('%sposts/%s.csv', path,
                                 format (Sys.time (), "%Y-%m-%d_%H")),
                 na   = "")
    }
  }
}

# save unused posts and figures in tmp/ folder for next iteration 
#----------------------------------------------------------------------------------------
if (dim (posts) [1] > 0) {
  write_csv (x    = posts,
             file = sprintf ('%sposts/posts.csv', path))
}

# write to log files
#----------------------------------------------------------------------------------------
write_csv (x         = as.data.frame (sprintf ('%s; R code ran smoothly.', 
                                               format (Sys.time (), "%Y-%m-%d %H:%M"))),
           file      = sprintf ('%slogs/logFileWitnessTreeRCode.txt', path),
           col_names = FALSE,
           append    = TRUE)

#========================================================================================