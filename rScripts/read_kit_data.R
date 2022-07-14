#========================================================================================
# This script updates the local copy of the witness tree kit data, designed, build, 
# installed, and maintained by Taylor Jones of Boston Unioversity. The kit contains a 
# dendrometer, temperature and humidity sensors, and a heat pulse sap flow sensor. 
# 
#----------------------------------------------------------------------------------------

# get arguments from command line (i.e., absolute path to working directory) -----------
args = commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("Error: At least one argument must be supplied (path to witnessTree directory).",
       call. = FALSE)
} else if (length(args) >= 1) {
  path = args[1] # absolute path
} else {
  stop("Error: Too many command line arguments supplied to R.")
}
#print(path)

# load dependencies ---------------------------------------------------------------------
if (!existsFunction("drive_rm")) library("googledrive") # for download of google sheet
if (!existsFunction("read_excel")) library("readxl")    # for reading of data file 
if (!existsFunction("mutate")) library("tidyverse")     # for tidy data handling
if (!existsFunction("as_date")) library("lubridate")    # for converting date formats
if (!existsFunction("rollmean")) library("zoo")         # for calculating moving average
  
# get posts spreadsheet -----------------------------------------------------------------
IOStatus <- suppressMessages(
  googledrive::drive_download(file = googledrive::as_id("1d52LFGaTsMX6CfduuL31_GPkRPcs5GP6Uhcv0pXRzzA"), 
                              path = paste0(path,'tmp/raw.xlsx'),
                              #type = 'csv',
                              overwrite = TRUE)
)

# verify that download worked properly --------------------------------------------------
if (exists('IOStatus')) {
  rm(IOStatus)
} else {
  stop("Error: Google Sheets with post messages was not properly downloaded.")
} 
  
# read excel file -----------------------------------------------------------------------
raw_data <- readxl::read_excel(path = paste0(path,"tmp/raw.xlsx"),
                               sheet = "HF Witness Tree",
                               col_names = FALSE)

# get slope and intercept of the dendrometer for conversion to mm -----------------------
s <- raw_data[2, 2] %>% as.numeric()
i <- raw_data[3, 2] %>% as.numeric() 

# get column names of data --------------------------------------------------------------
names_raw <- raw_data[11, 3:13] %>% as.character() %>% unlist() 

# extract only relevant data ------------------------------------------------------------
raw_data <- raw_data[-c(1:11),-c(1:2)]

# rename the columns --------------------------------------------------------------------
names(raw_data) <- names_raw

# create corrected time stamp column ----------------------------------------------------
raw_data$timestamp <- as_datetime(raw_data$`Timestamp (Raw)`, format = "%Y-%m-%dT%H:%M:%S")

# create correted dendrometer measurement -----------------------------------------------
raw_data$dendro <- as.numeric(raw_data$`Dendro (Raw)`) * s + i
# TR - I should add the initial diameter to this!

# convert data types --------------------------------------------------------------------
raw_data <- raw_data %>% mutate(temp = as.numeric(`Temperature (C)`),
                                pres = as.numeric(`Pressure (hPa)`),
                                rhum = as.numeric(`Humidity (%)`),
                                sapf = as.numeric(`Sapflow (cm/hr)`)) %>%
  select(timestamp, temp, pres, rhum, sapf, dendro)

# cut everything before the 18th of April 2022, when the kit was not working properly ---
raw_data <- raw_data %>% filter(timestamp >= as_datetime("2022-04-18"))
raw_data$dendro[raw_data$timestamp < as_datetime("2022-04-22")] <- NA

# replace all sap flow values lower than -5 cm/hr and everything above ??? talk to Taylor
raw_data$sapf[raw_data$sapf < -5 & raw_data$sapf > 35] <-  NA

# remove lines without values -----------------------------------------------------------
raw_data <- raw_data %>% filter(!is.na(temp) & !is.na(pres) & !is.na(rhum) & 
                                  !is.na(sapf) & !is.na(dendro))

# clean dendrometer values --------------------------------------------------------------
raw_data$dendro[raw_data$dendro < 5 | raw_data$dendro > 10] <- NA
# TR - These are arbitrary thresholds at the moment, which are difficult to generalise

# remove value if it is 0.5 smaller or larger than the preceding value -----------------
for (i in 2:length(raw_data$dendro)) {
  if(anyNA(raw_data$dendro[c(i,i - 1)])) next
  if(is.na(raw_data$dendro[i])) next
  if (raw_data$dendro[i] > raw_data$dendro[i - 1] + 0.5 | 
      raw_data$dendro[i] < raw_data$dendro[i - 1] - 0.5) raw_data$dendro [i] <- NA
}
  
# remove any temperature values below -50 -----------------------------------------------
raw_data$temp[raw_data$temp <= -50] <- NA

# remove any pressure values above 1050 -------------------------------------------------
raw_data$pres[raw_data$pres >= 1050] <- NA

# plot values for quality control -------------------------------------------------------
PLOT <- FALSE
if (PLOT) {
  plot(x = raw_data$timestamp, y = raw_data$sapf, typ = "l") # TR - currently not working
  plot(x = raw_data$timestamp, y = raw_data$dendro, typ = "l")
  plot(x = raw_data$timestamp, y = raw_data$temp, typ = "l")
  plot(x = raw_data$timestamp, y = raw_data$rhum, typ = "l")
  plot(x = raw_data$timestamp, y = raw_data$pres, typ = "l")
}

# rename data ---------------------------------------------------------------------------
kit_data <- raw_data

# clean workspace -----------------------------------------------------------------------
rm(raw_data, i, names_raw, s)
#========================================================================================