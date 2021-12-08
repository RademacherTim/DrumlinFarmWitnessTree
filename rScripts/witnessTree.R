#========================================================================================
# This is the main script running the witness tree bot. 
# See README.Rmd for more information.
#
# Version: 2.0.1
#
# Home repository: https://github.com/TTRademacher/witnessTreeCode
#
# Project lead: Tim Rademacher (rademacher.tim@gmail.com)
#
# Acknowledgements: Thanks above all to Clarisse Hart and Taylor Jones! Thanks also to 
#                   David Basler, Clarisse Hart, Hannah Robbins, Kyle Wyche, 
#                   Shawna Greyeyes, Bijan Seyednasrollah for their invaluable 
#                   contributions.
#
# Last update: 2021-12-08
#
#----------------------------------------------------------------------------------------


# To-do list:
#----------------------------------------------------------------------------------------
# - Restart the account  
# - Move of data to the new rawData directory
#----------------------------------------------------------------------------------------

# Get the absolute path to the directory including images and data 
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

# Output the paths at run-time to confirm that they were found
#----------------------------------------------------------------------------------------
print (path)

# Set the working directory
#----------------------------------------------------------------------------------------
setwd (path)

# Load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('%>%'))     suppressPackageStartupMessages (library ('tidyverse'))
if (!existsFunction ('as_date')) suppressPackageStartupMessages (library ('lubridate'))

# Source functions
#----------------------------------------------------------------------------------------
source  (sprintf ('%scode/rScripts/postHandling.R',          path))
#source  (sprintf ('%scode/rScripts/checkEvents.R',           path))
#source  (sprintf ('%scode/rScripts/checkClimate.R',          path))
#source  (sprintf ('%scode/rScripts/calcSapFlow.R',           path))
#source  (sprintf ('%scode/rScripts/calcRadialGrowth.R',      path))
#source  (sprintf ('%scode/rScripts/checkPhysiology.R',       path))
#source  (sprintf ('%scode/rScripts/checkPhenology.R',        path))
#source  (sprintf ('%scode/rScripts/checkMorphology.R',       path))
#source  (sprintf ('%scode/rScripts/checkCommunity.R',        path))
#source  (sprintf ('%scode/rScripts/generateInteractivity.R', path))
print ('Dependencies loaded.')

# Source basic data and stats for the trees
#----------------------------------------------------------------------------------------
source  (sprintf ('%srScripts/treeStats.R', path))
print ('Basic stats loaded.')

# Download data to the data directory
#----------------------------------------------------------------------------------------
IOStatus <- updateData ()
#========================================================================================