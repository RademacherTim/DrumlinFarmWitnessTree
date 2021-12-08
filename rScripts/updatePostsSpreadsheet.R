#========================================================================================
# Script to download post spreadsheet and save it as postDetails.csv in tmp/ directory
#----------------------------------------------------------------------------------------

# get arguments from command line (i.e., absolute path and Google API key)
#----------------------------------------------------------------------------------------
args = commandArgs (trailingOnly = TRUE)
if (length (args) == 0) {
  stop ("Error: At least one argument must be supplied (path to witnessTree directory).",
        call. = FALSE)
} else if (length (args) >= 1) {
  path                 = args [1] # absolute path
  GoogleSheetsPostsKey = args [2] # Google API key
} else {
  stop ("Error: Too many command line arguments supplied to R.")
}
print (path)
print (GoogleSheetsPostsKey)

# load dependencies
#--------------------------------------------------------------------------------------
if (!existsFunction ('drive_rm'))  suppressPackageStartupMessages (library ('googledrive')) # for download of google sheet

# get posts spreadsheet
#--------------------------------------------------------------------------------------
IOStatus <- suppressMessages (
  googledrive::drive_download (file = as_id (GoogleSheetsPostsKey), 
                               path = sprintf ('%stmp/postsDetails.csv', path),
                               type = 'csv',
                               overwrite = TRUE)
)

# verify that download worked properly
#----------------------------------------------------------------------------------------
if (exists ('IOStatus')) {
  rm (IOStatus)
} else {
  stop ("Error: Google Sheets with post messages was not properly downloaded.")
} 

# clean up
#----------------------------------------------------------------------------------------
rm (GoogleSheetsPostsKey, path)
#========================================================================================