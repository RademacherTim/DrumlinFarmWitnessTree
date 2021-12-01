#========================================================================================
# This script does include some statistics of the witness tree. The values have to be 
# revised.
#----------------------------------------------------------------------------------------

# Set birthday and calculate the age
#----------------------------------------------------------------------------------------
birthDay         <- as.POSIXct ("1909/04/12", format = "%Y/%m/%d") # in years C.E.    
age              <- floor (lubridate::time_length (Sys.time () - birthDay, "years"))

# LIDAR-derived quantities from scan by Peter Boucher (PhD Candidate, School of 
# Environment, University of Massachusetts Boston) with LEICA BLK360 on 2018/07/27 
#---------------------------------------------------------------------------------------- TR add year nunbers
totalVolume      <- 13937                                     # L
trunkVolume      <- 5624                                      # L
branchVolume     <- 8313                                      # L
treeHeight       <- 26.6                                      # m
trunkLength      <- 25.0                                      # m
branchLength     <- 2589.0                                    # m
branchNumber     <- 2015                                      # m
maxBranchOrder   <- 9                                         # unitless
totalSurfaceArea <- 437.0                                     # m2
dbh_qsm          <- 0.965                                     # m of cylinder at 1.3m height
dbh_cyl          <- 0.780                                     # m mean of cylinders fitted between 1.1 and 1.5 m height

# Values measured in the field
#----------------------------------------------------------------------------------------
cbh                 <- 2.65     # Circumference at breast height measured on 2018-10-11 (m)
dbh                 <- cbh / pi # Derived diameter at breast height (m)
bark                <- 2.5      # bark thickness (cm)
rHeartWood          <- 0.5      # heartwood radius (m)
k                   <- 0.5      # thermal conductivity of sapwood (W mK-1)
percentWaterContent <- 80       # TR Made up, but that is what the literature reports for similar species.
sapWoodArea         <- NA       # I need to add this

# Values from literature
#----------------------------------------------------------------------------------------
rhoWood       <- 740            # kg/m3 TTR guesstimate which needs to be based of literature eventually.
carbonContent <-   0.463        # For Quercus rubra from Lamlom & Savidge (2003)
RSRatio       <-   0.5          # unitless TTR guesstimate which needs to be based of literature eventually.
totalMass     <- (totalVolume / 1000.0 * rhoWood) / RSRatio  # kg
totalCarbon   <- totalMass * carbonContent
meanAnnualCarbonSequestration <- totalCarbon / age

# Location
#----------------------------------------------------------------------------------------
treeLocationName  <- "Harvard Forest" # name of location of this witnessTree
treeState         <- "MA"             # name of the state the tree is located in
treeCountry       <- "USA"            # name of the country the tree is located in
treeWebPage       <- "https://harvardforest.fas.harvard.edu/witness-tree-social-media-project" # link to the webpage of this witnessTree
treePrivacyPolicy <- "https://harvardforest.fas.harvard.edu/witness-tree-privacy-policy" # link to the privacy policy
coreImageLink     <- "https://harvardforest.fas.harvard.edu/sites/default/files/witnessTreeCoreZoomed_core by Dave Orwig_photo by Tim Rademacher.jpg"
contactEmail      <- "HFoutreach@fas.harvard.edu" # email to contact the person in charge
treeLon           <-  -72.189706396   # longitude of Koen's camera by the witnessTree measured on 2018/06/02
treeLat           <-  42.535685326    # lattitude of Koen's camera by the witnessTree measured on 2018/06/02
treeTimeZone      <- 'EST'            # time zone of the witnessTree 
#========================================================================================