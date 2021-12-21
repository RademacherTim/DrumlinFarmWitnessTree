#========================================================================================
# Functions to generate posts about the phenology of the tree itself and its 
# surrounding community. These functions rely heavily on work by Bijan Seyednasrollah and 
# his collaborators at the phenocam network.
#
#       Event                                   Date                 
#----------------------------------------------------------------------------------------
#   1)  checkLeafColourChange                   variable
#----------------------------------------------------------------------------------------

# function to post a phenocam image when colour change is ongoing
#----------------------------------------------------------------------------------------
checkLeafColourChange <- function (ptable, TEST = 0) {
  
  # don't bother checking this during summer and winter
  #--------------------------------------------------------------------------------------
  if ((substr (Sys.Date (), 6, 10) > "03-01" & substr (Sys.Date (), 6, 10) < "06-15") | 
      (substr (Sys.Date (), 6, 10) > "09-01" & substr (Sys.Date (), 6, 10) < "11-15") | 
      TEST >= 1) {
    
    # check season in memory
    #------------------------------------------------------------------------------------
    if (file.exists ('memory.csv')) {
      memory <- read_csv (file = paste0 (path, 'code/memory.csv'), col_types = cols ())
    } else {
      stop (paste0 (Sys.time (), '; checkPhenology.R; Error: There is not memory file.'))
    }
    
    # set site threshold (n.b. this needs to vary by sites)
    #------------------------------------------------------------------------------------
    siteGCCThreshold <- 0.35 # threshold is roughly fine for the DB ROI of the 
                             # harvardbarn and harvardbarn2 camera, but not for the 
                             # witness tree, where a reasonable value would be closer to 
                             # 0.365 
    
    # get phenocam data
    #------------------------------------------------------------------------------------
    gcc <- read_csv (paste0 (path,'data/gcc.csv'), col_types = cols ())
    
    # is it not the growing season and gcc indicates leaf unfolding has occurred?
    #------------------------------------------------------------------------------------
    if ((!memory [['growingSeason']] & gcc [['gcc_90']] [2] > siteGCCThreshold) | 
        TEST == 1) {
      postDetails <- getPostDetails ("checkLeafUnfolding")
      FigureName  <- 'witnesstree_PhenoCamImage' # TR - Need to change this to make it dependent on a variable 
      delay  <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
      ptable <- add_row (ptable,
                         priority    = postDetails [["Priority"]],
                         fFigure     = postDetails [['fFigure']],
                         figureName  = sprintf ('%s/tmp/%s.jpg', path, FigureName),
                         message     = postDetails [["MessageText"]],
                         hashtags    = postDetails [["Hashtags"]],
                         expires     = expiresIn (delay))
      
      # update growingSeason boolean to start the season
      #----------------------------------------------------------------------------------
      memory [['growingSeason']] <- TRUE
      write_csv (memory, paste0 (path, 'code/memory.csv'))
      
    # is it the growing season and gcc indicates leaf shedding has occurred?
    #------------------------------------------------------------------------------------
    } else if ((memory [['growingSeason']] & gcc [['gcc_90']] [2] < siteGCCThreshold) | 
               TEST == 2) {
      postDetails <- getPostDetails ("checkLeafColourChange - endOfSeason")
      FigureName  <- 'witnesstree_PhenoCamImage'
      delay  <- as.numeric (substring (postDetails [['ExpirationDate']], 7, 8)) * 60 * 60
      ptable <- add_row (ptable,
                         priority    = postDetails [["Priority"]],
                         fFigure     = postDetails [['fFigure']],
                         figureName  = sprintf ('%s/tmp/%s.jpg', path, FigureName),
                         message     = postDetails [['MessageText']],
                         hashtags    = postDetails [["Hashtags"]],
                         expires     = expiresIn (delay))
      
      # update growingSeason boolean to end the season
      #-----------------------------------------------------------------------------------
      memory [['growingSeason']] <- FALSE
      write_csv (memory, paste0 (path, 'code/memory.csv'))
      
    }
  }
  # return table with posts
  #------------------------------------------------------------------------------------
  return (ptable)
}

# # load dependencies
# #----------------------------------------------------------------------------------------
# library ('animation')
# library ('phenocamapi')
# library ('lubridate')
# 
# 
# # set parameters
# #----------------------------------------------------------------------------------------
# site  <- 'witnesstree'  # phenocam site name
# Years <- year (Sys.Date ()) # vector of years to make the animation
# vegType <- 'DB' # vegetation type DB = deciduous broadloeaf
# roiID <- 1000  # ROI ID 
# 
# # plot the image
# #----------------------------------------------------------------------------------------
# if(class(img)!='try-error'){
#   par(mar= c(0,0,0,0))
#   plot(0:1,0:1, type='n', axes= FALSE, xlab= '', ylab = '')
#   rasterImage(img, 0, 0, 1, 1)
# }
# 
# # create a new folder to download the midday images
# #----------------------------------------------------------------------------------------
# dir.create (site, showWarnings = FALSE)
# 
# # getting the timeseries from the phenocam server
# #----------------------------------------------------------------------------------------
# gcc_ts <- get_pheno_ts (site, 
#                         vegType = vegType, 
#                         roiID   = roiID, 
#                         type    = '1day')
# 
# # organizing columns
# #----------------------------------------------------------------------------------------
# gcc_ts [, month:=month (YYYYMMDD)] # extracting month from the date
# gcc_ts [, YYYYMMDD:=as.Date (YYYYMMDD)] # convert to the right format
# gcc_ts [, midday_url:=sprintf ('https://phenocam.sr.unh.edu/data/archive/%s/%04d/%02d/%s', 
#                                site, year, month, midday_filename)] # making the URL of midday images
# 
# # organizing the data into a new data.table including the URL, date and GCC90 values
# #----------------------------------------------------------------------------------------
# gcc_file_tbl <- gcc_ts[year%in%(Years),.(midday_url, YYYYMMDD, gcc_90)] 
# 
# # creating the destination filename to download each midday image
# #----------------------------------------------------------------------------------------
# gcc_file_tbl [, midday_dest:=paste0 (site, '/', basename (midday_url))] 
# gcc_file_tbl <- na.omit (gcc_file_tbl) # removing the NA values
# 
# 
# gcc_file_tbl <- gcc_file_tbl [month (YYYYMMDD) == 5]
# 
# # downloading midday files
# #----------------------------------------------------------------------------------------
# mapply (function (x) {
#           dest <- paste0 (site, '/', basename (x))
#           if (file.exists (dest)) {
#             message (dest, ' ', 'already exists!')
#             return ()
#           }
#           try (download.file (x, dest))
#         }, gcc_file_tbl$midday_url)
# 
# # a simple function to plot midday image given an index and corresponding gcc timeseries upto that date
# #----------------------------------------------------------------------------------------
# show_midday <- function (i) {
#   
#   par (fig = c (0, 1, 0.3, 1),  mar = c (0, 0, 0, 0), bg = '#000000')  
#   plot (0:1, 0:1, type = 'n', axes = FALSE, xlab = '', ylab = '')
#   
#   img <- readJPEG (gcc_file_tbl$midday_dest [i])
#   rasterImage (img, 0, 0, 1, 1)
#   mtext ('Greenup Seasonality at Harvard Forest', col = '#51fddc')
#   
#   par (fig = c (0, 1, 0, 0.3), new = T, mar = c (2, 2, 0, 0))  
#   plot (gcc_file_tbl$YYYYMMDD [1:i], 
#         gcc_file_tbl$gcc [1:i], 
#         bty ='n', 
#         type = 'l',
#         lwd = 2,
#         cex.axis = 1.5,
#         col = '#51fddc', 
#         col.axis = '#51fddc',
#         xlim = range (gcc_file_tbl$YYYYMMDD),
#         ylim = range (gcc_file_tbl$gcc, na.rm = TRUE))
#   mtext ('Canopy Greenness', side = 2, line = 0, col = '#51fddc', cex = 2, font = 2)
#   
#   points (gcc_file_tbl$YYYYMMDD [i], 
#           gcc_file_tbl$gcc [i], 
#           pch = 19,
#           col = '#ca5f63')
# }
# 
# # dummy
# #----------------------------------------------------------------------------------------
# gcc_file_tbl [, gcc := gcc_90]
# 
# # number of image
# #----------------------------------------------------------------------------------------
# n <- nrow (gcc_file_tbl)
# 
# # make the animation using the saveVideo animation file
# #----------------------------------------------------------------------------------------
# saveVideo (interval = 0.5, # animation interval in seconds
#            ani.width = 1000, # image width in pixels
#            ani.height = 900,# image height in pixels
#            ani.res = 75, # resolution, not important here
#            video.name = paste0 (site, '.mp4'),
#           
#            for (i in seq (1, n, by = 1)){
#              cat (i, '\n')
#              show_midday (i)
#            })
#========================================================================================