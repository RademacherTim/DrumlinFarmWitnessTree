#========================================================================================
# Collections of functions to handle potential posts including:
#    selectPost () - selecting a post from several options
#    deletePost () - deleting specific posts to avoid double posting
#    checkExpirationDate () - purging posts that are now irrelevant
#    reEvaluatePriorityOf () - updating the priority of posts
#    getPostDetails () - read message text, hastags and expiration date 
#    deletePostedPostsAndRemoveDuplicates () - deleting already posted posts and duplicates
#    expiresIn () - determining expiration dates
#    findOrdinalSuffix () - determining the ordinal suffix of a number
#----------------------------------------------------------------------------------------

# Function to select post of highest priority from list of possible posts
#----------------------------------------------------------------------------------------
selectPost <- function (ptable) { # tibble of posts with, inter alia, priorities 
  
  # delete all messages that are empty or saying "NEEDS MESSAGE"
  #--------------------------------------------------------------------------------------
  ptable <- ptable [ptable [['message']] != 'NEEDS MESSAGE', ]
  ptable <- ptable [ptable [['message']] != '', ]
  
  # arrange messages by descending priority
  #--------------------------------------------------------------------------------------
  mByPriority <- arrange (.data = ptable, desc (priority))
  
  # subset only highest priority
  #--------------------------------------------------------------------------------------
  highestPriority <- mByPriority [mByPriority [['priority']] == mByPriority [['priority']] [1] &
                                    !is.na (mByPriority [['priority']]), ]
  
  # Check whether there is more than one post of highest priority
  #--------------------------------------------------------------------------------------
  if (dim (highestPriority) [1] > 1) { # there are several posts of highest priority, select one at random
    post <- sample_n (highestPriority, 1)
  } else { # there is only one post of highest priority
    post <- highestPriority
  }
  
  # delete temporary variables
  #--------------------------------------------------------------------------------------
  rm (mByPriority, highestPriority)
  
  # return the selected message
  #--------------------------------------------------------------------------------------
  return (post)
}

# Comments:
#---------------------------------------------------------------------------------------
# TR - Not sure selecting messages at random is the best method when there are several 
#      messages of highest priority. TBD!

# Function to delete message from messages tibble to avoid it being used again.
#---------------------------------------------------------------------------------------
deletePost <- function (ptable, # tibble of posts
                        post) { # tibble of the selected post
  
  # Get line on which the message is 
  #-------------------------------------------------------------------------------------
  nRow <- which (ptable [['message']] == post [['message']])
  
  # Delete row with same message in posts tibble
  #-------------------------------------------------------------------------------------
  ptable <- ptable [-nRow, ]
  
  # Get lines which are duplicates
  #-------------------------------------------------------------------------------------
  temp <- ptable [, -1]  # Have to delete priority as that can vary over time.
  ptable <- ptable [!duplicated (temp), ]
  
  # Delete temporary variables
  #-------------------------------------------------------------------------------------
  rm (nRow, temp)
  
  # Return the selected message
  #-------------------------------------------------------------------------------------
  return (ptable)
}

# Function to check the expiration dates of posts
#---------------------------------------------------------------------------------------
checkExpirationDatesOf <- function (ptable) {
  
  # Loop over messages to check expiration date
  #---------------------------------------------------------------------------------------
  i = 1
  while (i <= dim (ptable) [1]) {
    
    # If message is expired, delete it.
    #------------------------------------------------------------------------------------
    if (ptable [['expires']] [i] <  Sys.time ()) {
      ptable <- ptable [-i, ]
    } else {
      i = i + 1
    }
  }
  
  # Return the remaining messages
  #---------------------------------------------------------------------------------------
  return (ptable)
} 

# Function to re-evaluate priority of preserved posts
#----------------------------------------------------------------------------------------
reEvaluatePriorityOf <- function (ptable) {
  
  # Increase priority of all messages by 0.01 (2.88 priority points per day), 
  # but have them max out at 10
  #--------------------------------------------------------------------------------------
  ptable [['priority']] <- apply (X      = cbind (as.numeric (ptable [['priority']]) + 0.01, 
                                                  rep (10, dim (ptable) [1])), 
                                  MARGIN = 1, 
                                  FUN    = min)
  
  # Return table pf posts with updated priorities
  #--------------------------------------------------------------------------------------
  return (ptable)
}

# Comments:
#---------------------------------------------------------------------------------------
# TR - maybe wee need to come up with a different way of increasing the priority of 
#      various messages instead on just increasing them by 1. 

# Function to read in message text, hastags and expiration date from post spreadsheet and 
# hand them to a specific function.
#----------------------------------------------------------------------------------------
getPostDetails <- function (fName) {
  
  # load dependencies -------------------------------------------------------------------
  if (!existsFunction("read_csv")) library("readr")
  if (!existsFunction("add_column")) library("tibble")
  
  # get posts spreadsheet ---------------------------------------------------------------
  input <- readr::read_csv (file = sprintf ('%stmp/postsDetails.csv', path),
                            col_types = c (
                              FunctionID         = col_character (),
                              Status             = col_character (),
                              Event              = col_character (),
                              Logic              = col_character (),
                              Date               = col_character (),
                              ExpirationDate     = col_character (),
                              Priority           = col_number (),
                              FigureName         = col_character (),
                              FigureDescription  = col_character (),
                              Hashtags           = col_character (),
                              MessageTreatment   = col_character (),
                              DominantTheme      = col_character (),
                              MessageText        = col_character (),
                              Variables          = col_character (),
                              VariablesExamples  = col_character (),
                              Links              = col_character (),
                              NumberOfCharacters = col_number ()
                            ))
  
  # find appropriate lines using the function name --------------------------------------
  temp <- input [input$FunctionID == fName & !is.na (input$FunctionID), ]
  
  # If there is more than one message for an event choose a message randomly
  # N.B.: To make sure that both treatments get choosen with the same probability, we 
  # need to have the same number of messages for both treatments (sober scientific data 
  # versus narrative environmental facts).
  #---------------------------------------------------------------------------------------
  if (dim (temp) [1] > 1) temp <- sample_n (temp, 1)
  
  # extract relevant post details --------------------------------------------------------
  postDetails <- temp %>% dplyr::select (-c (Status, Event, Logic, Variables, 
                                             VariablesExamples, Link, 
                                             NumberOfCharacters))
  
  # Check whether there is a figure accompanying the post
  #--------------------------------------------------------------------------------------
  postDetails <- add_column (postDetails,
                             fFigure = ifelse (length (postDetails [["FigureName"]]) == 0 |
                                                 is.na (postDetails [['FigureName']]), F, T))
  
  # Randomly decide whether we use the accompanying figure or not
  # N.B. Audience building posts are marked as such and are always posted with pictures
  #--------------------------------------------------------------------------------------
  if (!is.na (postDetails [['FigureName']])) {
    if (substring (fName, 1, 22) == 'checkCommunityWildlife' | 
        substring (fName, 1, 22) == 'monthlyEngagementRemin') {
      postDetails [['fFigure']] <- TRUE
    } else {
      postDetails [['fFigure']] <- sample (c (T, F), size = 1)
    } 
    
    # Add the image path to the figureName, so that the bot can actually find them
    # N.B. This has to be overwritten for monthlyEngagementReminders and monthlyRadGrowthSummary
    #------------------------------------------------------------------------------------
    postDetails [['FigureName']] <- paste0 (path,'images/', 
                                            postDetails [['FigureName']])
  } else {
    postDetails [['fFigure']] <- FALSE
  }
  
  # Return the post'd details
  #--------------------------------------------------------------------------------------
  return (postDetails)
}

# Function to delete already posted posts and remove duplicates from list of possible 
# posts
#---------------------------------------------------------------------------------------
deletePostedPostsAndRemoveDuplicates <- function (ptable) {
  
  # make list of posts during the last twenty days
  #----------------------------------------------------------------------------------------
  fileNames <- list.files (sprintf ('%s/posts/', path), pattern = '.csv')
  fileNames <- fileNames [fileNames != 'logfile.csv' & fileNames != 'posts.csv']
  fileNames <- fileNames [as.POSIXct (substring (fileNames, 1, 10), format = "%Y-%m-%d") >= 
                            Sys.Date () - 20]
  
  # replace all numbers with 'x' in the post table to avoid posting messages that only 
  # differ, for example, by a single digit  
  #--------------------------------------------------------------------------------------
  ptemp <- gsub ("([0-9]+)", x = ptable [['message']], replacement  = 'x') 
  
  # loop over posted messages and read messages
  #--------------------------------------------------------------------------------------
  if (length (fileNames) != 0) {
    for (p in 1:length (fileNames)) {
      temp <- read_csv (sprintf ('%s/posts/%s',path, fileNames [p]),
                        col_types = cols ())
      temp <- gsub ("([0-9]+)", x = temp [['message']], replacement = 'x')
      if (p == 1 | !exists ('nRow')) {
        nRow <- which (ptemp == temp)  
      } else {
        nRow <- c (nRow, which (ptemp == temp))
      }
    }
    
    # delete similar messages from the table of posts
    #------------------------------------------------------------------------------------
    if (length (nRow) != 0) {
      ptable <- ptable [-nRow, ]; rm (nRow, temp, ptemp)
    }
  }
  
  # Only continue further deleting duplicates, if there are any potential posts left
  #--------------------------------------------------------------------------------------
  if (dim (ptable) [1] >= 1) {
    # replace all numbers with 'x' in the post table to avoid posting messages that only 
    # differ, for example, by a single digit  
    #------------------------------------------------------------------------------------
    ptemp <- gsub ("([0-9]+)", x = ptable [['message']], replacement  = 'x') 
    
    # look for and delete duplicates by choosing unique messages
    #------------------------------------------------------------------------------------
    temp <- unique (ptemp)
    for (i in 1:length (temp)) {
      if (i == 1) {
        indicesToKeep <- min (which (gsub ("([0-9]+)", 
                                           x = ptable [['message']], 
                                           replacement = 'x') == temp [i]), na.rm = TRUE)
      } else {
        indicesToKeep <- c (indicesToKeep, 
                            min (which (gsub ("([0-9]+)", 
                                              x = ptable [['message']], 
                                              replacement = 'x') == temp [i]), na.rm = TRUE))
      }
    }
    ptable <- ptable [indicesToKeep, ]
  }  
  # return updated table with posts
  #--------------------------------------------------------------------------------------
  return (ptable)
}

# Function to determine the expiration date for posts
#----------------------------------------------------------------------------------------
expiresIn  <- function (delay = 0) { # delay in seconds
  as.POSIXct (sprintf ("%s 23:59:59", 
                       format (Sys.Date (), format = '%Y-%m-%d')),
              tz = treeTimeZone) + delay
}

# Function to find the ordinal suffix for a number
#----------------------------------------------------------------------------------------
findOrdinalSuffix <- function (string) {
  
  # Make sure the string is actually a character string
  #--------------------------------------------------------------------------------------
  string <- as.character (string)
  
  # Determine the number of digits of the number
  #--------------------------------------------------------------------------------------
  len <- nchar (string)
  
  # Find the ordinal suffix
  #--------------------------------------------------------------------------------------
  if (substring (string, len, len) == '1' &
      substring (string, len-1, len) != '11') {
    suffix <- 'st'
  } else if (substring (string, len, len) == '2' &
             substring (string, len-1, len) != '12') {
    suffix <- 'nd'  
  } else if (substring (string, len, len) == '3' &
             substring (string, len-1, len) != '13') {
    suffix <- 'rd'
  } else {
    suffix <- 'th'
  }
  
  # Return appropriate suffix
  #--------------------------------------------------------------------------------------
  return (suffix)
}
#=======================================================================================