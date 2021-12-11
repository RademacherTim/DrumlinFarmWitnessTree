#!/bin/bash

# Get execution directory from the command line (second argument with index 1)
#----------------------------------------------------------------------------------------
WITNESSTREEPATH=$1

# Get the date for the logs
#----------------------------------------------------------------------------------------
DATE=$(date +%Y-%m-%d" "%H:%M:%S)

# Read WITNESSTREEPATH from config file
#----------------------------------------------------------------------------------------
source ${WITNESSTREEPATH}code/config
if [ $? != 0 ]
then
   # write error message into log
   echo ${DATE} 'Error: Could not source config.' >> ${WITNESSTREEPATH}logs/logFileDataUpdate.txt 
   exit 1 # terminate script and indicate error
fi

# Run the updateClimateData R script to download climate data
#----------------------------------------------------------------------------------------
Rscript ${WITNESSTREEPATH}code/rScripts/updateClimateData.R ${WITNESSTREEPATH}
if [ $? != 0 ]
then 
   # write error message into log
   echo ${DATE} 'Error: Climate data download was not successful.' >> ${WITNESSTREEPATH}logs/logFileDataUpdate.txt 
   exit 1 # terminate script and indicate error
fi

# Run the updatePhenoData R script to download phenocam data and images 
#----------------------------------------------------------------------------------------
Rscript ${WITNESSTREEPATH}code/rScripts/updatePhenoData.R ${WITNESSTREEPATH}
if [ $? != 0 ]
then 
   # write error message into log
   echo ${DATE} 'Error: Phenocam data download was not successful.' >> ${WITNESSTREEPATH}logs/logFileDataUpdate.txt 
   exit 1 # terminate script and indicate error
fi

# Write time and date into log file in the tmp/ folder
#----------------------------------------------------------------------------------------
echo ${DATE} >> ${WITNESSTREEPATH}logs/logFileDataUpdate.txt