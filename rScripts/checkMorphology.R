#========================================================================================
# Functions to generate posts based on tree morphology.
#
#       Event                                   Date                 
#----------------------------------------------------------------------------------------
#   1)  Explain tree dimensions                 20th day of the month
#
#----------------------------------------------------------------------------------------

# explain dimensions (from LIDAR facts) 
#----------------------------------------------------------------------------------------
explainDimensions <- function (ptable, TEST = 0) {
  
  # load dependencies
  #--------------------------------------------------------------------------------------
  if (!existsFunction ('read_csv')) library ('readr')
  
  # make sure no "explain dimension"-post has been posted yet this month
  #--------------------------------------------------------------------------------------
  listOfVisitors <- list.files (path = sprintf ('%simages/wildlifeCam/',path), 
                                pattern = '.jpg')  
  if (file.exists (paste0 (path, 'code/memory.csv'))) {
    memory <- read_csv (paste0 (path, 'code/memory.csv'), col_types = cols ())
  } else { # if there is no memory.csv file create one
    memory <- tibble (numberOfPreviousVisitors = length (listOfVisitors),
                      lastResponse = format (Sys.time (), '%Y-%m-%d %H:%M'),
                      dimensionsPosted = FALSE,
                      growingSeason = TRUE)
  }
  
  # check whether it is the 20th
  #-------------------------------------------------------------------------------------- 
  if (substring (Sys.time (), 9, 13) == "20 12" & memory [['dimensionsPosted']] == FALSE | 
      TEST == 1) {
    postDetails <- getPostDetails ('explainDimensions')
    if (substring (postDetails [['MessageText']], 1, 3) == 'I a') {
      message <- sprintf (postDetails [["MessageText"]], totalSurfaceArea)
    } else if (substring (postDetails [['MessageText']], 1, 3) == 'Wow') {
      message <- sprintf (postDetails [["MessageText"]], round (branchLength / 1609.34, 1), 
                          round (branchLength / 1000.0,1))
    } else if (substring (postDetails [['MessageText']], 1, 3) == 'Sci') {
      message <- sprintf (postDetails [["MessageText"]], treeHeight, round (treeHeight * 3.28084, 0))
    } else {
      message <- postDetails [["MessageText"]]
    }
    delay  <- as.numeric (substring (postDetails [['ExpirationDate']], 7 ,7)) * 60 * 60 
    ptable <- add_row (ptable, 
                       priority    = postDetails [["Priority"]],
                       fFigure     = postDetails [['fFigure']],
                       figureName  = postDetails [["FigureName"]], 
                       message     = message, 
                       hashtags    = postDetails [["Hashtags"]], 
                       expires     = expiresIn (delay)) 
    
    # update memory 
    #------------------------------------------------------------------------------------
    memory [['dimensionsPosted']] <- TRUE
    memory [['lastResponse']] <- format (memory [['lastResponse']], '%Y-%m-%d %H:%M')
    write_csv (memory, paste0 (path,'code/memory.csv'))
    
    # Reset dimensionsPosted boolean on the 21st of the month
    #--------------------------------------------------------------------------------------
  } else if (substring (Sys.time (), 9, 13) == "21 12" & 
             memory [['dimensionsPosted']] == TRUE) {
    memory [['dimensionsPosted']] <- FALSE
    memory [['lastResponse']] <- format (memory [['lastResponse']], '%Y-%m-%d %H:%M')
    write_csv (memory, paste0 (path,'code/memory.csv'))
  } 
  
  
  # return updated post table
  #--------------------------------------------------------------------------------------
  return (ptable)
  
}
#========================================================================================