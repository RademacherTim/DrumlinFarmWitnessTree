#========================================================================================
# This script does include some statistics of the witness tree. The values have to be 
# revised.
#----------------------------------------------------------------------------------------

# Set birthday and calculate the age
#----------------------------------------------------------------------------------------
birthDay         <- as.POSIXct ("1897/04/10", format = "%Y/%m/%d") # in years C.E.    
age              <- floor (lubridate::time_length (Sys.time () - birthDay, "years"))

# LIDAR-derived quantities from scan by Peter Boucher (PhD Candidate, School of 
# Environment, University of Massachusetts Boston) with LEICA BLK360
#---------------------------------------------------------------------------------------- TR add year nunbers
totalVolume      <- NA                                     # L
trunkVolume      <- NA                                      # L
branchVolume     <- NA                                      # L
treeHeight       <- NA                                      # m
trunkLength      <- NA                                      # m
branchLength     <- NA                                    # m
branchNumber     <- NA                                      # m
maxBranchOrder   <- NA                                        # unitless
totalSurfaceArea <- NA                                     # m2
dbh_qsm          <- NA                                     # m of cylinder at 1.3m height
dbh_cyl          <- NA                                     # m mean of cylinders fitted between 1.1 and 1.5 m height

# Values measured in the field
#----------------------------------------------------------------------------------------
cbh                 <- NA     # Circumference at breast height measured on 2018-10-11 (m)
dbh                 <- cbh / pi # Derived diameter at breast height (m)
bark                <- NA      # bark thickness (cm)
rHeartWood          <- NA      # heartwood radius (m)
k                   <- 0.5      # thermal conductivity of sapwood (W mK-1)
percentWaterContent <- 80       # TR Made up, but that is what the literature reports for similar species.
sapWoodArea         <- NA       # I need to add this

# Values from literature
#----------------------------------------------------------------------------------------
rhoWood       <- 740            # kg/m3 TR guesstimate which needs to be based of literature eventually.
carbonContent <-   0.505        # For Acer saccharum from Lamlom (2006)
RSRatio       <-   0.5          # unitless TR guesstimate which needs to be based of literature eventually.
totalMass     <- (totalVolume / 1000.0 * rhoWood) / RSRatio  # kg
totalCarbon   <- totalMass * carbonContent
meanAnnualCarbonSequestration <- totalCarbon / age

# Location
#----------------------------------------------------------------------------------------
treeSpecies       <- "sugar maple (Acer saacharum)" # name of species
treeLocationName  <- "Drumlin Farm" # name of location of this witnessTree
treeState         <- "MA"             # name of the state the tree is located in
treeCountry       <- "USA"            # name of the country the tree is located in
treeWebPage       <- "https://harvardforest.fas.harvard.edu/witness-tree-social-media-project" # link to the webpage of this witnessTree
treePrivacyPolicy <- "https://harvardforest.fas.harvard.edu/witness-tree-privacy-policy" # link to the privacy policy
coreImageLink     <- "" # Needs to be taken!
contactEmail      <- "HFoutreach@fas.harvard.edu" # email to contact the person in charge
treeLon           <-  -71.3323328051268 # approximate longitude from google maps 
treeLat           <-  42.40894958218316 # approximate latitude from google maps
treeTimeZone      <- 'EST'            # time zone of the witnessTree 
#========================================================================================