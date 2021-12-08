#!/bin/bash

# Get execution directory from the command line (second argument with index 1)
#----------------------------------------------------------------------------------------
WITNESSTREEPATH=$1

# Get the date for the logs
#----------------------------------------------------------------------------------------
DATE=$(date +%Y-%m-%d" "%H:%M:%S)

# Read WITNESSTREEPATH from config file
#----------------------------------------------------------------------------------------
source ${WITNESSTREEPATH}config
if [ $? != 0 ]
then
   # write error message into log
   echo ${DATE} 'Error: Could not source config.' >> ${WITNESSTREEPATH}logs/logFileSpreadsheetUpdate.txt 
   exit 1 # terminate script and indicate error
fi

# Run the updatePostsSpreadsheet.R script to download the post spreadsheet
#----------------------------------------------------------------------------------------
Rscript ${WITNESSTREEPATH}code/rScripts/updatePostsSpreadsheet.R ${WITNESSTREEPATH} ${GoogleSheetsPostsKey}
if [ $? != 0 ] # add condition so that this in only run once a day
then 
   # write error message into log
   echo ${DATE} 'Error: Post spreadsheet download was not successful.' >> ${WITNESSTREEPATH}logs/logFileSpreadsheetUpdate.txt 
   exit 1 # terminate script and indicate error
fi

# Write time and date into log file in the tmp/ folder
#----------------------------------------------------------------------------------------
echo ${DATE} 'All smooth.' >> ${WITNESSTREEPATH}logs/logFileSpreadsheetUpdate.txt