#!/bin/bash

# get execution directory from the command line (second argument with index 1) ----------
WITNESSTREEPATH=$1

# get the date for the logs -------------------------------------------------------------
DATE=$(date +%Y-%m-%d" "%H:%M:%S)

# read WITNESSTREEPATH from config file -------------------------------------------------
source ${WITNESSTREEPATH}code/config
if [ $? != 0 ]
then
   # write error message into log
   echo ${DATE} 'Error: Could not source config.' >> ${WITNESSTREEPATH}logs/logFileDataUpdate.txt 
   exit 1 # terminate script and indicate error
fi

# run the update_climate.R R script to download climate data ----------------------------
Rscript ${WITNESSTREEPATH}code/rScripts/update_climate.R ${WITNESSTREEPATH}
if [ $? != 0 ]
then 
   # write error message into log
   echo ${DATE} 'Error: Climate data download was not successful.' >> ${WITNESSTREEPATH}logs/logFileDataUpdate.txt 
   exit 1 # terminate script and indicate error
fi

# run the update_pheno_data R script to download phenocam data and images ---------------
Rscript ${WITNESSTREEPATH}code/rScripts/update_pheno_data.R ${WITNESSTREEPATH}
if [ $? != 0 ]
then 
   # write error message into log
   echo ${DATE} 'Error: Phenocam data download was not successful.' >> ${WITNESSTREEPATH}logs/logFileDataUpdate.txt 
   exit 1 # terminate script and indicate error
fi

# run the update_witness_tree_data R script to download witness tree kits data ----------
#Rscript ${WITNESSTREEPATH}code/rScripts/update_witness_tree_data.R ${WITNESSTREEPATH}
#if [ $? != 0 ]
#then 
   # write error message into log
#   echo ${DATE} 'Error: Witness tree data download was not successful.' >> ${WITNESSTREEPATH}logs/logFileDataUpdate.txt 
#   exit 1 # terminate script and indicate error
#fi

# write time and date into log file in the tmp/ folder ----------------------------------
echo ${DATE} 'All ran smoothly.' >> ${WITNESSTREEPATH}logs/logFileDataUpdate.txt