#========================================================================================
# Functions to generate posts depending on the physiological status of the tree.
#
#       Event                                   Date                 
#----------------------------------------------------------------------------------------
#   1)  Monthly sapflow summary                 second week of the month
#   x)  Start of sap flow                       variable
#   x)  Peak of sap flow                        variable
#   x)  End of sap flow                         variable
#   x)  Total growing season sap flow           variable
#   x)  Start of wood growth                    variable
#   x)  Peak of wood growth                     variable
#   x)  End of wood growth                      variable
#   x)  Total growing season wood growth        variable
#
#----------------------------------------------------------------------------------------


# Monthly sap flow summary
#----------------------------------------------------------------------------------------
# monthlySapFlowSummary <- function (ptable, TEST = 0) {
#   
#   # Check whether it is the second week of the month
#   if (ceiling (day (Sys.Date ()) / 7) == 2 | TEST == 1) {
#     # Read in sap flow data
#     
#     # Compare the sap flow to the previous month
#     #
#   } 
#   return (ptable)
# } 

# monthly radial growth (i.e. stem and branch diameter) summary -------------------------
# monthlyRadGrowthSummary <- function(ptable, TEST = 0) {
#   
#   # check whether it is the second week of the month
#   if (ceiling (lubridate::day(Sys.Date()) / 7) == 4 | TEST == 1) {
#     
#     # get radial growth for the last 30 days and the 30 days prior to that --------------
#     radGrowth <- calcRadialGrowth(pdm_calibration_path = dataPath, 
#                                   temporalRes = 'monthly',
#                                   PLOT = TRUE)
#     
#     # check whether radial growth is fast or slower than in the previous month ----------
#     if (radGrowth[1, 1] > radGrowth[1, 2]) { # current month grew more
#       postDetails <- getPostDetails("monthlyRadGrowthSummary - fast")
#       message     <- sprintf(postDetails[["MessageText"]], round(radGrowth[['monthlyGrowth']][1], 3),
#                              round(radGrowth[['monthlyGrowth']][2], 3))
#       delay   <- as.numeric(substring(postDetails[['ExpirationDate']], 7, 8)) * 60 * 60
#       ptable  <- add_row(ptable, 
#                          priority    = postDetails[["Priority"]],
#                          fFigure     = postDetails[['fFigure']],
#                          figureName  = sprintf('%s/tmp/monthlyGrowth_%s.png', path, Sys.Date()), 
#                          message     = message, 
#                          hashtags    = postDetails[["Hashtags"]], 
#                          expires     = expiresIn(delay))
#     } else if (radGrowth[1, 1] <= radGrowth[1, 2] & 
#                radGrowth[1, 1] > 0.05) { # last month grew as much or less -----
#       postDetails <- getPostDetails("monthlyRadGrowthSummary - slow")
#       message <- sprintf(postDetails[["MessageText"]], round(radGrowth[['monthlyGrowth']][1], 3),
#                              round (radGrowth[['monthlyGrowth']][2], 3))
#       delay <- as.numeric(substring(postDetails[['ExpirationDate']], 7 ,8)) * 60 * 60
#       ptable <- add_row (ptable, 
#                          priority    = postDetails[["Priority"]],
#                          fFigure     = postDetails[['fFigure']],
#                          figureName  = sprintf('%s/tmp/monthlyGrowth_%s.png', path, Sys.Date()), 
#                          message     = message, 
#                          hashtags    = postDetails[["Hashtags"]], 
#                          expires     = expires(delay))
#     } else if (radGrowth[1,1] <= 0.05) { # tree grew only very little and is dormant?
#       postDetails <- getPostDetails("monthlyRadGrowthSummary - dormant")
#       delay <- as.numeric(substring(postDetails[['ExpirationDate']], 7 ,8)) * 60 * 60
#       ptable <- add_row(ptable, 
#                         priority    = postDetails[["Priority"]],
#                         fFigure     = postDetails[['fFigure']],
#                         figureName  = sprintf('%s/tmp/monthlyGrowth_%s.png', path, Sys.Date()), 
#                         message     = postDetails[["MessageText"]], 
#                         hashtags    = postDetails[["Hashtags"]], 
#                         expires     = expiresIn(delay))
#     }
#   }
#   
#   # return the updated message table ----------------------------------------------------
#   return (ptable)
# } 

# wood growth update on the 24th of July ------------------------------------------------
# checkWoodGrowthUpdate <- function(ptable, TEST = 0) {
#   
#   # check whether the function to calculate wood growth exists --------------------------
#   if (!existsFunction('calcRadialGrowth')) {
#     source (sprintf('%s/rScripts/calcRadialGrowth.R', path))
#   }
#   
#   # check whether it is the 24th of July ------------------------------------------------
#   if (substring(Sys.time(), 6, 10) == '06-10' | TEST == 1) {
#     postDetails <- getPostDetails("checkWoodGrowthUpdate - june")
#     if (substring(postDetails [['MessageText']], 1, 1) == 'I'){
#       growth <- calcRadialGrowth(pdm_calibration_path = dataPath) # get monthly growth
#       message <- sprintf(postDetails [["MessageText"]], round (growth  [['monthlyGrowth']] [1], 2)) 
#     } else {
#       message <- sprintf(postDetails [["MessageText"]]) 
#     } 
#   } else if (substring(Sys.time(), 6, 10) == '07-24' | TEST == 2) {
#     postDetails <- getPostDetails("checkWoodGrowthUpdate - july")
#     if (substring(postDetails [['MessageText']], 1, 1) == 'T'){
#       growth <- calcRadialGrowth(pdm_calibration_path = dataPath, temporalRes = 'annual')
#       message <- sprintf(postDetails [["MessageText"]], round (growth [['annualGrowth']], 1), 
#                         round (growth  [['annualGrowth']] / 25.4, 2)) 
#     } else {
#       message <- sprintf(postDetails [["MessageText"]]) 
#     } 
#   } else {
#     return(ptable)
#   }
#   delay  <- as.numeric(substring (postDetails [['ExpirationDate']], 7 ,7)) * 60 * 60
#   ptable <- add_row (ptable, 
#                      priority    = postDetails [["Priority"]],
#                      fFigure     = postDetails [['fFigure']],
#                      figureName  = postDetails [["FigureName"]], 
#                      message     = message, 
#                      hashtags    = postDetails [["Hashtags"]], 
#                      expires     = expiresIn(delay))
#   
#   # return table with posts -------------------------------------------------------------
#   return (ptable)
# }

# near peak wood growth 
#----------------------------------------------------------------------------------------
# checkNearPeakWoodGrowth <- function (ptable, TEST = 0) {
#   
# }

# Start of sap flow
#----------------------------------------------------------------------------------------
# startSapFlow <- function (ptable, TEST = 0) {
#   # read in sap flow data
#   # check whether the sap flow has increased substantially ove rthe past week
#   # if so, send beginning of sap flow message
#   if ( | TEST == 1) {
#   } 
#   return (ptable)
# } 

# leaves completely elongated
#----------------------------------------------------------------------------------------
# checkLeafElongation <- function (ptable, TEST = 0) {
#   
# }

# leaves developed waxy cuticle ---------------------------------------------------------
checkWaxyCuticle <- function(ptable, TEST = 0) {
  if (substring(Sys.time(), 1, 13) == '2019-08-16 12' | TEST == 1) {
    postDetails <- getPostDetails("checkWaxyCuticle")
    delay <- as.numeric(substring(postDetails[['ExpirationDate']], 7, 8)) * 60 * 60
    ptable <- add_row(ptable, 
                      priority    = postDetails[["Priority"]],
                      fFigure     = postDetails[['fFigure']],
                      figureName  = postDetails[["FigureName"]], 
                      message     = postDetails[['MessageText']], 
                      hashtags    = postDetails[["Hashtags"]], 
                      expires     = expiresIn(delay))
  } 
  
  # return post table --------------------------------------------------------------------
  return (ptable)
}


#========================================================================================