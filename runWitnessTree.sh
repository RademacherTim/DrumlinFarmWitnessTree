#!/bin/bash

# get execution directory from the command line (second argument with index 1)
#----------------------------------------------------------------------------------------
WITNESSTREEPATH=$1

# get date
#----------------------------------------------------------------------------------------
DATE=$(date +%Y-%m-%d" "%H:%M:%S)

# read WITNESSTREEPATH from config file
#----------------------------------------------------------------------------------------
source ${WITNESSTREEPATH}code/config
if [ $? != 0 ]
then
   # write error message into log
   echo "${DATE}; Error: Could not source config." >> ${WITNESSTREEPATH}logs/logFileWitnessTree.txt 
   exit 1 # terminate script and indicate error
fi

# run the witnessTree R script to generate messages
#----------------------------------------------------------------------------------------
Rscript ${WITNESSTREEPATH}code/rScripts/witness_tree.R ${WITNESSTREEPATH} ${GSPostsKey}
if [ $? != 0 ]
then 
   # write error message into log
   echo "${DATE}; Error: witnessTree.R did not execute." >> ${WITNESSTREEPATH}logs/logFileWitnessTree.txt 
   exit 1 # terminate script and indicate error
fi

# run bot to post generated messages to twitter and facebook
#----------------------------------------------------------------------------------------
python3 ${WITNESSTREEPATH}code/pythonScripts/witnessTreeBot.py ${API_key} ${API_secret} ${access_token} ${access_token_secret} ${page_access_token} ${facebook_page_id} ${WITNESSTREEPATH}
if [ $? != 0 ]
then 
   # write error message into log
   echo "${DATE}; Error: witnessTreeBot.py did not execute." >> ${WITNESSTREEPATH}logs/logFileWitnessTree.txt 
   exit 1 # terminate script and indicate error
fi

# write time and date into log file in the tmp/ folder
#----------------------------------------------------------------------------------------
echo ${DATE} >> ${WITNESSTREEPATH}logs/logFileWitnessTree.txt
