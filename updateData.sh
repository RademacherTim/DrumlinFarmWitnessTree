#!/bin/bash

# Get execution directory from the command line (second argument with index 1)
#----------------------------------------------------------------------------------------
WITNESSTREEPATH=$1

# Read WITNESSTREEPATH from config file
#----------------------------------------------------------------------------------------
source ${WITNESSTREEPATH}code/config
if [ $? != 0 ]
then
   # write error message into log
   echo 'Error: Could not source config.' >> ${WITNESSTREEPATH}logs/logfileDataUpdate.txt 
   exit 1 # terminate script and indicate error
fi

# Run the updateData R script to generate messages
#----------------------------------------------------------------------------------------
Rscript ${WITNESSTREEPATH}code/rScripts/updateData.R ${WITNESSTREEPATH} ${GoogleSheetsPostsKey}
if [ $? != 0 ]
then 
   # write error message into log
   echo 'Error: witnessTree.R did not execute.' >> ${WITNESSTREEPATH}logs/logFileDataUpdate.txt 
   exit 1 # terminate script and indicate error
fi

# Write time and date into log file in the tmp/ folder
#----------------------------------------------------------------------------------------
DATE=$(date +%Y-%m-%d" "%H:%M:%S)
echo ${DATE} >> ${WITNESSTREEPATH}logs/logfileDataUpdate.txt