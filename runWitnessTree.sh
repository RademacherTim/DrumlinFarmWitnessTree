#!/bin/bash

# Get execution directory from the command line (second argument with index 1)
#----------------------------------------------------------------------------------------
WITNESSTREEPATH=$1

# Read WITNESSTREEPATH from config file
#----------------------------------------------------------------------------------------
source ${WITNESSTREEPATH}config
if [ $? != 0 ]
then
   # write error message into log
   echo 'Error: Could not source config.' >> ${WITNESSTREEPATH}logs/logFileWitnessTree.txt 
   exit 1 # terminate script and indicate error
fi

# Run the witnessTree R script to generate messages
#----------------------------------------------------------------------------------------
Rscript ${WITNESSTREEPATH}code/rScripts/witnessTree.R ${WITNESSTREEPATH} ${GSPostsKey}
if [ $? != 0 ]
then 
   # write error message into log
   echo 'Error: witnessTree.R did not execute.' >> ${WITNESSTREEPATH}logs/logFileWitnessTree.txt 
   exit 1 # terminate script and indicate error
fi

# Run bot to post generated messages to twitter and facebook
#----------------------------------------------------------------------------------------
python ${WITNESSTREEPATH}code/pythonScripts/witnessTreeBot.py ${consumer_key} ${consumer_secret} ${access_token} ${access_token_secret} ${page_access_token} ${facebook_page_id} ${WITNESSTREEPATH}
if [ $? != 0 ]
then 
   # write error message into log
   echo 'Error: witnessTreeBot.py did not execute.' >> ${WITNESSTREEPATH}logs/logFileWitnessTree.txt 
   exit 1 # terminate script and indicate error
fi

# Write time and date into log file in the tmp/ folder
#----------------------------------------------------------------------------------------
DATE=$(date +%Y-%m-%d" "%H:%M:%S)
echo ${DATE} >> ${WITNESSTREEPATH}logs/logFileWitnessTree.txt
