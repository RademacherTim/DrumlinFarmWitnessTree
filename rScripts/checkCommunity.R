#========================================================================================
# Functions to generate postss about the community around the tree.
#
#       Event                                   Date                 
#----------------------------------------------------------------------------------------
#   1)  explainSeedDispersal                    Between 1st of Sep and 30th of Nov
#   2)  checkCommunityWildlife                  All year, when image is added to the 
#                                               directory
#   3)  explainSpongyMothHerbivory               Between 1st of Sep and 30th of Nov
#   4)  explainGallWasps                        Between 1st of Sep and 30th of Nov
#----------------------------------------------------------------------------------------

# Explain seed dispersal ----------------------------------------------------------------
explainSeedDispersal <- function(ptable, TEST = 0) {
  if (substring(Sys.Date(), 6, 10) > '09-01' & substring(Sys.Date(), 6, 10) < '11-15'| 
      TEST == 1) {
    postDetails <- getPostDetails("explainSeedDispersal")
    message   <- sprintf(postDetails [["MessageText"]])
    expiryDate <- sprintf("%s-11-30 23:59:59 %s", 
                          format(Sys.Date(), format = '%Y'),
                          treeTimeZone) %>% lubridate::as_datetime(tz = treeTimeZone)
    ptable <- add_row(ptable, 
                      priority    = postDetails [["Priority"]],
                      fFigure     = postDetails [['fFigure']],
                      figureName  = postDetails [["FigureName"]], 
                      message     = message, 
                      hashtags    = postDetails [["Hashtags"]], 
                      expires     = expiryDate)
  } 
  
  # Return table with posts -------------------------------------------------------------
  return(ptable)
} 

# Check for wildlife images (visitors) at the tree
#----------------------------------------------------------------------------------------
# 
# To post an image, it needs to be moved into the wildlifeCamera folder in the images
# directory and named according to the following naming convention: 
# 
# wildlifeCameraImageXXXX.jpg
# 
# , where XXXX has to be replaced by an increasing number with preceeding zeros. The 
#   first image would be named wildlifeCameraImage0001.jpg and so on. 
#----------------------------------------------------------------------------------------
checkCommunityWildlife <- function(ptable, TEST = 0) {
  
  # Check whether there is a new wildlife photo -----------------------------------------
  listOfVisitors <- list.files (path = sprintf('%simages/wildlifeCam/',path), 
                                pattern = '.jpg')  
  if (file.exists(paste0(path, 'code/memory.csv'))) {
    memory <- read_csv(file = paste0(path, 'code/memory.csv'), col_types = cols())
  } else {
    stop(paste0(Sys.time(), '; checkCommunity.R; Error: There is not memory file'))
  }
  
  
  # Check that there is at least one picture in the directory ---------------------------
  if (length(listOfVisitors) > 0 | TEST >= 1) {
    
    # Check whether there is a new picture in the directory -----------------------------
    if (as.numeric(substring(tail(listOfVisitors, n = 1), 20, 23)) > 
        memory [['numberOfPreviousVisitors']] | TEST >= 1) {
      
      # Get message depending on time of year -------------------------------------------
      if (substring(Sys.Date(), 6, 10) >  '03-21' & 
          substring(Sys.Date(), 6, 10) <= '06-21' | 
          TEST == 1) { # it is spring
        postDetails <- getPostDetails("checkCommunityWildlife - spring")
      } else if (substring(Sys.Date(), 6, 10) >  '06-21' & 
                 substring(Sys.Date(), 6, 10) <= '09-21' | 
                 TEST == 2) { # it is summer
        postDetails <- getPostDetails("checkCommunityWildlife - summer")
      } else if (substring(Sys.Date(), 6, 10) >  '09-21' & 
                 substring(Sys.Date(), 6, 10) <= '11-21' | 
                 TEST == 3) { # it is fall
        postDetails <- getPostDetails("checkCommunityWildlife - fall")
      } else if (substring(Sys.Date(), 6, 10) >  '11-21' & 
                 substring(Sys.Date(), 6, 10) <= '03-21' | 
                 TEST == 4) { # it is winter
        postDetails <- getPostDetails("checkCommunityWildlife - winter")
      }
      message <- sprintf(postDetails [["MessageText"]])
      delay   <- as.numeric(substring(postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
      ptable  <- add_row(ptable, 
                         priority   = postDetails [["Priority"]],
                         fFigure    = postDetails [['fFigure']],
                         figureName = paste0(path,'images/wildlifeCam/',
                                             tail(listOfVisitors, n = 1)),
                          message   = message, 
                          hashtags  = postDetails [["Hashtags"]], 
                          expires   = expiresIn(delay = delay))
      
      # increase the wildlife counter in the memory -------------------------------------
      memory [['numberOfPreviousVisitors']] <- memory [['numberOfPreviousVisitors']] + 1
      write_csv(memory, paste0(path,'code/memory.csv'))
    }
  }
  
  # Return table with posts -------------------------------------------------------------
  return(ptable)
} 

# Explain spongy moth herbivory ----------------------------------------------------------
explainSpongyMothHerbivory <- function(ptable, TEST = 0) {
  if (substring(Sys.Date(), 6, 10) > '05-15' & substring(Sys.Date(), 6, 10) < '08-31'| 
      TEST == 1) {
    postDetails <- getPostDetails("explainSpongyMothHerbivory")
    ptable    <- add_row(ptable, 
                         priority   = postDetails [["Priority"]],
                         fFigure    = postDetails [['fFigure']],
                         figureName = postDetails [["FigureName"]], 
                         message    = postDetails [["MessageText"]], 
                         hashtags   = postDetails [["Hashtags"]], 
                         expires    = expiresIn(0))
  } 
  
  # Return table with posts -------------------------------------------------------------
  return(ptable)
} 

# Explain gall wasps on oak trees -------------------------------------------------------
explainGallWasps <- function(ptable, TEST = 0) {
  if (substring(Sys.Date(), 6, 10) > '09-01' & substring(Sys.Date(), 6, 10) < '11-15'| 
      TEST == 1) {
    postDetails <- getPostDetails("explainGallWasps")
    message <- sprintf(postDetails [["MessageText"]])
    ptable  <- add_row(ptable, 
                       priority   = postDetails [["Priority"]],
                       fFigure    = postDetails [['fFigure']],
                       figureName = postDetails [["FigureName"]], 
                       message    = message, 
                       hashtags   = postDetails [["Hashtags"]], 
                       expires    = expiresIn(0))
  } 
  
  # Return table with posts -------------------------------------------------------------
  return(ptable)
} 
#========================================================================================