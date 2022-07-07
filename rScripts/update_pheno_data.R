#========================================================================================
# Script to download images and data from the phenocams for the Harvard Forest Witness 
# Tree, which are used to generate phenology based messages. These functions rely heavily 
# on work by Bijan Seyednasrollah for the phenocam network and the network itself.
#
#----------------------------------------------------------------------------------------

# Get arguments from command line (i.e., absolute path and Google API key) --------------
args = commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("Error: At least one argument must be supplied (path to witnessTree directory).",
       call. = FALSE)
} else if (length(args) >= 1) {
  path = args[1] # absolute path
} else {
  stop("Error: Too many command line arguments supplied to R.")
}

# load dependencies ---------------------------------------------------------------------
if (!existsFunction('cols'))              suppressPackageStartupMessages(library('readr'))
if (!existsFunction('download_phenocam')) suppressPackageStartupMessages(library('phenocamr'))
if (!existsFunction('get_midday_list'))   suppressPackageStartupMessages(library('phenocamapi')) 
# phenocamapi package needs to be installed from github

# get phenocam site names for the specific tree -----------------------------------------
siteNames = c('witnesstree', 'harvardbarn','harvardbarn2')

# loop over number of cameras -----------------------------------------------------------
for (s in siteNames) {
  
  # get list of midday image names for the three cameras --------------------------------
  assign(paste0('site_midday_',s), get_midday_list(s, direct = FALSE))
  
  # download only the last midday image for each camera ---------------------------------
  download.file(tail(get(paste0('site_midday_',s)), n = 1), 
                destfile = paste0(path,'images/',s,'_PhenoCamImage.jpg'), 
                mode = 'wb',
                quiet = TRUE)
  
  # download the very last available image for each camera ------------------------------
  if (s == 'witnesstree') {
    phenocamServer <- 'http://phenocam.sr.unh.edu/data/latest/'
    recentImgFilePath <- paste0(phenocamServer, s,'.jpg')
    download.file(recentImgFilePath,
                  destfile = paste0(path,'images/',s,'_PhenoCamImageRecent.jpg'),
                  mode = 'wb',
                  quiet = TRUE)
  }
  
  # getting the timeseries from the phenocam server -------------------------------------
  gcc_temp <- tail(get_pheno_ts(s, 
                                vegType = 'DB', 
                                roiID   = 1000, 
                                type    = '3day'), 
                   n = 1)
  if (s == siteNames[1]) {
    gcc <- gcc_temp 
  } else {
    gcc <- rbind(gcc, gcc_temp)
  }
}

# write a local file of the phenological data in the data directory ---------------------
write_csv(x = gcc, file = paste0(path, 'data/gcc.csv'))

# clean-up ------------------------------------------------------------------------------
rm(siteNames, s, gcc, gcc_temp, path, site_midday_harvardbarn, site_midday_harvardbarn2,
   site_midday_witnesstree)
#========================================================================================