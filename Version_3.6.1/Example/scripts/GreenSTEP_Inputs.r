#==================
#GreenSTEP_Inputs.r
#==================
#Copyright 2009 - 2015, Oregon Department of Transportation
#Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at
#http://www.apache.org/licenses/LICENSE-2.0
#Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
#Version: 3.5 Beta
#Date: 5/24/15

#Description
#===========
#This scripts loads all of the model objects and data needed to run the GreenSTEP model. The objects are stored in 3 lists:
#  Abbr_ contains naming vectors
#  Model_ contains all of the model objects
#  Inputs_ contains all of the scenario inputs
#After the inputs are read in, formatted and added to the lists, the lists are 'attached' to expose the names of the objects to the global environment.

#Helper functions
#================
#Remove NA values from a vector
withoutNA <- function(X.) {
  X.[!is.na(X.)]
}

#Read a 2-column CSV formatted file as a named vector
#One column is names and other is values
readVectorFile <- function(File, NameCol, ValueCol, ...) {
  Data.. <- read.csv(File, ...)
  Values. <- Data..[[ValueCol]]
  names(Values.) <- Data..[[NameCol]]
  Values.
}

#Define naming vectors
#=====================
Abbr_ <- list()

#Counties and Metropolitan Areas
setwd(ModelDir)
Filename <- "county_groups.csv"
CountyGroups.. <- read.csv(Filename, as.is = TRUE)
Abbr_$Co <- unique(CountyGroups..$County)
Abbr_$Ma <- withoutNA(unique(CountyGroups..$Msa))
setwd(RunDir)

#Categories for the age of persons
Abbr_$Ap <- 
  c("Age0to14", "Age15to19", "Age20to29", "Age30to54", "Age55to64", "Age65Plus")

#Categories for urban types ( Metropolitan vs. Town )
Abbr_$Ut <- c("Metropolitan", "Town")

#Categories for all development types ( Metropolitan, Town, Rural )
Abbr_$Dt <- c("Metropolitan", "Town", "Rural")

#Vehicle types
Abbr_$Vt <- c("Auto", "LtTruck", "HvyTruck", "Bus")

#Income groups
Abbr_$Ig <-
  c("0to20K",
    "20Kto40K",
    "40Kto60K",
    "60Kto80K",
    "80Kto100K",
    "100KPlus")

#Fuel types
Abbr_$Ft <- c("ULSD", "Biodiesel", "Gasoline", "Ethanol", "CNG")

#Congestion levels
Abbr_$Cl <- c("None", "Mod", "Hvy", "Sev", "Ext")

#Types of vehicles
Abbr_$Ty <- c("LtVeh", "Truck", "Bus")

#Functional class of roadways
Abbr_$Fc <- c("Fwy", "Art", "Other")

#Powertrain types
Abbr_$Pt <-
  c(
    "LdIceEco",
    "LdIceNonEco",
    "LdHev",
    "LdFcv",
    "LdEv",
    "TruckIce",
    "TruckEv",
    "BusIce",
    "BusEv"
  )

#Modes for public transit
Abbr_$Mp <- 
  c("DR", "VP", "MB", "RB", "MG", "SR", "HR", "CR")

#Attach the list
attach(Abbr_)

#Read in global values
#=====================
#The global_values.txt file contains various parameters used by the model.
source(paste(ModelDir, "/global_values.txt", sep = ""))

#Load estimated models and functions
#===================================
#The submodels of GreenSTEP are combinations of data and functions. For example,
#the submodel that adjusts household income distributions based on changes in
#the distribution of the household age structure consists of a matrix of
#probabilities and a function to calculate income adjustments based on the
#matrix of probabilities and other inputs. All of the submodels are contained in
#the GreenSTEP_.RData file that is composed of a list containing the data and
#functions for the submodels. This list is attached to the workspace so that
#each submodel can be called using it's individual name.

#Load the estimated model
setwd(ModelDir)
load("GreenSTEP_.RData")
#Modify the congestion model Lambda values if present
if (file.exists("mpo_lambda_values.csv")) {
  Lambda.. <- read.csv("mpo_lambda_values.csv")
  Lambda.Ma <- Lambda..$Lambda
  names(Lambda.Ma) <- Lambda..$Area
  GreenSTEP_$CongModel_$Lambda.Ma <- Lambda.Ma
  rm(Lambda.., Lambda.Ma)
}
attach(GreenSTEP_)
setwd(RunDir)

#Load model starting inventory data
#==================================
#The starting inventories for the model are also contained in the model
#directory.
setwd(ModelDir)
Model_ <- list()

#Crosswalk between counties and metropolitan areas
Filename <- "county_groups.csv"
CountyGroups.. <- read.csv(Filename, as.is = TRUE, row.names = 1)
Model_$CountyGroups.. <- CountyGroups..
rm(Filename, CountyGroups..)

#Base year metropolitan and non-metropolitan UGB areas by county
Filename <- "ugb_areas.csv"
TempInput.. <- read.csv(Filename, row.names = 1)
Model_$BaseUgbAreas.CoUt <- as.matrix(TempInput..)[Co, Ut]
rm(Filename, TempInput..)

#Base year metropolitan, town and rural population splits by county
Filename <- "urban_rural_pop_splits.csv"
TempInput.. <- read.csv(Filename, row.names = 1)
Model_$UrbRurPopProp.CoDt <- as.matrix(TempInput..)[Co, Dt]
rm(Filename, TempInput..)

#Revenue-mile factors to convert to bus revenue miles
Model_$RevMiFactors <- 
  readVectorFile(File = "revenue_mile_factors.csv", 
                 NameCol = "Mode",
                 ValueCol = "Factor")[Mp]

#Travel demand management parameters for the proportional reduction in VMT by
#program area
Filename <- "tdm_parameters.csv"
Temp.. <- read.csv( Filename )
Model_$TdmParm. <- Temp..$Value
names( Model_$TdmParm. ) <- Temp..$Parm
rm( Filename, Temp.. )

#Base year parameters for metropolitan light vehicle DVMT, truck DVMT
#proportions, and proportions of DVMT on freeways and arterials
Filename <- "mpo_base_dvmt_parm.csv"
MpoBaseDvmtParm..Ma <- read.csv( Filename, row.names=1 )
Model_$MpoBaseDvmtParm..Ma <- MpoBaseDvmtParm..Ma[ Ma, ]
rm( Filename, MpoBaseDvmtParm..Ma )

#Proportions of truck and bus DVMT by functional class
Filename <- "truck_bus_fc_dvmt_split.csv"
TempInput.. <- read.csv( Filename )
Va <- TempInput..$Value
TruckBusFcDvmtSplit_Va.. <- split( TempInput..[,-1], Va )
TruckBusFcDvmtSplit_Va.. <- lapply( TruckBusFcDvmtSplit_Va.., function(x) {
  RowNames. <- x[,1]
  Parm.. <- x[,-1]
  rownames( Parm.. ) <- RowNames.
  ColNames. <- colnames( Parm.. )
  ColNames. <- gsub( "X", "", ColNames. )
  colnames( Parm.. ) <- ColNames.
  Parm..
} )
Model_$TruckBusFcDvmtSplit_Va.. <- TruckBusFcDvmtSplit_Va..
rm( Filename, TempInput.., Va, TruckBusFcDvmtSplit_Va.. )

#Average rural density by county
Filename <- "ave_rural_pop_density.csv"
TempInput.. <- read.csv( Filename )
Model_$AveRuralDen.Co <- TempInput..$Density
names( Model_$AveRuralDen.Co ) <- TempInput..$County
rm( Filename, TempInput.. )

#Factor to convert metropolitan household DVMT to metropolitan light vehicle
#road DVMT
if( file.exists( "LtVehDvmtFactor.Ma.RData" ) ) {
  Model_$LtVehDvmtFactor.Ma <- assignLoad( "LtVehDvmtFactor.Ma.RData" )
} else {
  Filename <- "hh_dvmt_to_road_dvmt.csv"
  TempInput.. <- read.csv( Filename )
  LtVehDvmtFactor.Ma <- TempInput..[,2]
  names( LtVehDvmtFactor.Ma ) <- TempInput..[,1]
  Model_$LtVehDvmtFactor.Ma <- LtVehDvmtFactor.Ma[ Ma ]
  rm( Filename, TempInput.., LtVehDvmtFactor.Ma )
}

#Specify bus and rail modes
Model_$BusModes <-  c("MB", "RB")
Model_$RailModes <-  c("SR", "HR", "CR")

#Set the working directory back to the run directory and attach the list
setwd( RunDir )
attach( Model_ )

#Load the policy inputs
#======================
#Policy inputs are stored in the scenario\inputs folder
setwd( InputDir )

Inputs_ <- list()

#Statewide real per capita income
Filename <- "per_cap_inc.csv"
TempInput.. <- read.csv( Filename )
Inputs_$PerCapInc.Yr <- TempInput..$Income
names( Inputs_$PerCapInc.Yr ) <- TempInput..$Year 
rm( Filename, TempInput.. )

#County proportions of statewide real per capita income
Filename <- "county_inc_prop.csv"
TempInput.. <- read.csv( Filename )
colnames(TempInput..) <- gsub("X", "", colnames(TempInput..))
Inputs_$IncProp.CoYr <- as.matrix( TempInput.. )[ Co, Yr ]
rm( Filename, TempInput.. )

#Metropolitan, town and rural area growth splits by county
Filename <- "urban_rural_growth_splits.csv"
TempInput.. <- read.csv( Filename, row.names=1 )
Inputs_$UrbRurGrowthSplit.CoDt <- as.matrix( TempInput.. )[ Co, Dt ]
rm( Filename, TempInput.. )

#Growth rates of UGB areas by county and type relative to population growth
#e.g. a value of 1 means a 10% growth of area for a 10% growth in population
Filename <- "ugb_area_growth_rates.csv"
TempInput.. <- read.csv( Filename, row.names=1 )
Inputs_$UgbAreaGrowthRates.CoUt <- as.matrix( TempInput..)[ Co, Ut ]
rm( Filename, TempInput.. )

#Proportions of metropolitan areas living in urban mixed use neighborhoods
Filename <- "metropolitan_urban_type_proportions.csv"
Inputs_$UrbanTypeProp.YrMa <- as.matrix( read.csv( Filename, row.names=1 ) )
rm( Filename )

#Freeway and arterial lane miles by metropolitan area and year
Filename <- "lane_miles.csv"
TempInput.. <- read.csv(Filename)
TempInput_Yr.. <- split(TempInput.., TempInput..$Year)
Inputs_$LaneMiles_Yr.MaFc <- lapply(TempInput_Yr.., function(x) {
  LaneMiles.MaFc <- as.matrix(x[,c("Fwy", "Art")])
  rownames(LaneMiles.MaFc) <- x$Msa
  LaneMiles.MaFc[Ma,]
})
rm(Filename, TempInput.., TempInput_Yr..)

#Transit revenue miles by mode, metropolitan area, and year
Filename <- "transit_service.csv"
TempInput.. <- read.csv(Filename)
TempInput_Yr.. <- split(TempInput.., TempInput..$Year)
Inputs_$PTService_Yr.MaMp <- lapply(TempInput_Yr.., function(x) {
  PTService.MaMp <- as.matrix(x[,Mp])
  rownames(PTService.MaMp) <- x$Msa
  PTService.MaMp[Ma,]
})
rm(Filename, TempInput.., TempInput_Yr..)

#Carshare input parameters
Filename <- "carshare.csv"
TempInput.. <- read.csv( Filename )
Va <- TempInput..$Value
CarshareParm_Va.. <- split( TempInput..[,-1], Va )
CarshareParm_Va.. <- lapply( CarshareParm_Va.., function(x) {
  RowNames. <- x[,1]
  Parm.. <- x[,-1]
  rownames( Parm.. ) <- RowNames.
  ColNames. <- colnames( Parm.. )
  ColNames. <- gsub( "X", "", ColNames. )
  colnames( Parm.. ) <- ColNames.
  Parm..
} )
Inputs_$CarshareParm_Va.. <- CarshareParm_Va..
rm( Filename, TempInput.., Va, CarshareParm_Va.. )

#Congestion pricing input parameters
Filename <- "congestion_charges.csv"
TempInput.. <- read.csv( Filename )
Va <- TempInput..$Value
CongPriceParm_Va.. <- split( TempInput..[,-1], Va )
CongPriceParm_Va.. <- lapply( CongPriceParm_Va.., function(x) {
  RowNames. <- x[,1]
  Parm.. <- x[,-1]
  rownames( Parm.. ) <- RowNames.
  ColNames. <- colnames( Parm.. )
  ColNames. <- gsub( "X", "", ColNames. )
  colnames( Parm.. ) <- ColNames.
  Parm..
} )
Inputs_$CongPriceParm_Va.. <- CongPriceParm_Va..
rm( Filename, TempInput.., Va, CongPriceParm_Va.. )

# Proportion of households participating in individualized marketing program (IMP)
Filename <- "imp_prop_goal.csv"
ImpPropGoal.MaYr <- as.matrix( read.csv( Filename, row.names=1 ) )
colnames( ImpPropGoal.MaYr ) <- gsub( "X", "", colnames( ImpPropGoal.MaYr ) )
Inputs_$ImpPropGoal.MaYr <- ImpPropGoal.MaYr
rm( Filename, ImpPropGoal.MaYr )

# Proportion of workers at employers with strong employee commute options (ECO) program
Filename <- "prop_wrk_eco.csv"
PropWrkEco.MaYr <- as.matrix( read.csv( Filename, row.names=1 ) )
colnames( PropWrkEco.MaYr ) <- gsub( "X", "", colnames( PropWrkEco.MaYr ) )
Inputs_$PropWrkEco.MaYr <- PropWrkEco.MaYr
rm( Filename, PropWrkEco.MaYr )

#Operations program deployment inputs
Filename <- "ops_deployment.csv"
TempInput.. <- read.csv( Filename )
Va <- TempInput..$Value
OpsDeployParm_Va.. <- split( TempInput..[,-1], Va )
OpsDeployParm_Va.MaYr <- lapply( OpsDeployParm_Va.., function(x) {
  RowNames. <- x[,1]
  Parm.. <- x[,-1]
  rownames( Parm.. ) <- RowNames.
  ColNames. <- colnames( Parm.. )
  ColNames. <- gsub( "X", "", ColNames. )
  colnames( Parm.. ) <- ColNames.
  Parm..
} )
Inputs_$OpsDeployParm_Va.MaYr <- OpsDeployParm_Va.MaYr
rm( Filename, TempInput.., Va, OpsDeployParm_Va.., OpsDeployParm_Va.MaYr )

#Speed smoothing and ecodriving inputs
Filename <- "speed_smooth_ecodrive.csv"
TempInput.. <- read.csv( Filename )
Va <- TempInput..$Value
SmoothEcoDriveParm_Va.. <- split( TempInput..[,-1], Va )
SmoothEcoDriveParm_Va.. <- lapply( SmoothEcoDriveParm_Va.., function(x) {
  RowNames. <- x[,1]
  Parm.. <- x[,-1]
  rownames( Parm.. ) <- RowNames.
  ColNames. <- colnames( Parm.. )
  ColNames. <- gsub( "X", "", ColNames. )
  colnames( Parm.. ) <- ColNames.
  Parm..
} )
Inputs_$SmoothEcoDriveParm_Va.. <- SmoothEcoDriveParm_Va..
rm( Filename, TempInput.., Va, SmoothEcoDriveParm_Va.. )

#Other operations deployment inputs
Filename <- "other_ops.csv"
TempInput_ <- as.list( read.csv( Filename, as.is=TRUE ) )
Ty <- TempInput_[[1]][ c(1,5,9,13) ]
Lv <- TempInput_[[2]][ 1:4 ]
OtherOps_Yr.LvTy <- lapply( TempInput_[ 3:length( TempInput_ ) ], function(x) {
  array( x, dim=c( length(Lv), length(Ty) ), dimnames=list( Lv, Ty ) ) } )
names( OtherOps_Yr.LvTy ) <- gsub( "X", "", names( OtherOps_Yr.LvTy ) )
Inputs_$OtherOps_Yr.LvTy <- OtherOps_Yr.LvTy
rm( Filename, TempInput_, OtherOps_Yr.LvTy )                   

#Eco-driving and low rolling-resistance tire inputs
Filename <- "eco_tire.csv"
TempInput.. <- read.csv( Filename, row.names=1 )
Inputs_$EcoTire..Yr <- TempInput..
rm( Filename, TempInput.. )

#Proportions of vehicles that are light trucks
Filename <- "lttruck_prop.csv"
TempInput.. <- read.csv( Filename )
Inputs_$LtTruckProp.CoYr <- TempInput..[,2:ncol(TempInput..)]
colnames( Inputs_$LtTruckProp.CoYr ) <- gsub("X", "", colnames( Inputs_$LtTruckProp.CoYr ))
rownames( Inputs_$LtTruckProp.CoYr ) <- TempInput..[,1]
rm( Filename, TempInput.. )

#Light weight vehicles input parameters
Filename <- "light_vehicles.csv"
TempInput.. <- read.csv( Filename )
Va <- TempInput..$Value
LtVehParm_Va.. <- split( TempInput..[,-1], Va )
LtVehParm_Va.. <- lapply( LtVehParm_Va.., function(x) {
  RowNames. <- x[,1]
  Parm.. <- x[,-1]
  rownames( Parm.. ) <- RowNames.
  ColNames. <- colnames( Parm.. )
  ColNames. <- gsub( "X", "", ColNames. )
  colnames( Parm.. ) <- ColNames.
  Parm..
} )
Inputs_$LtVehParm_Va.. <- LtVehParm_Va..
rm( Filename, TempInput.., Va, LtVehParm_Va.. )

#Private vehicle MPG & electric power consumption (MPKwh) by vehicle year and type
Filename <- "auto_lighttruck_mpg.csv"
Inputs_$AutoLtTrkMpg..Yr <- read.csv( Filename, row.names=1 )
rm( Filename )

#Proportions of households who optimize
Filename <- "optimize.csv"
TempInput.. <- read.csv( Filename )
Inputs_$OptimProp.Yr <- TempInput..$OptimProp
names( Inputs_$OptimProp.Yr ) <- TempInput..$Year
rm( Filename, TempInput.. )

#Load the plug in hybrid vehicle data
Filename <- "phev_characteristics.csv"
Inputs_$PhevRangeProp..Yr <- read.csv( Filename, row.names=1 )
rm( Filename )

#Fuel types and carbon emissions by vehicle type
Filename <- "fuel_co2.csv"
Inputs_$FuelCo2..Yr <- read.csv( Filename, row.names=1 )
rm( Filename )

#Auto and light-truck fuel type mix
Filename <- "auto_lighttruck_fuel.csv"
Inputs_$AutoLtTrkFuels..Yr <- read.csv( Filename, row.names=1 )
rm( Filename )

#Carbon emissions per KWH
Filename <- "power_co2.csv"
PowerCo2.CoYr <- as.matrix( read.csv( Filename, row.names=1 ) )
colnames( PowerCo2.CoYr  ) <- gsub( "X", "", colnames( PowerCo2.CoYr ) )
Inputs_$PowerCo2.CoYr <- PowerCo2.CoYr
rm( Filename, PowerCo2.CoYr )

#Costs for fuel, electricity and optional VMT-based charges
Filename <- "costs.csv"
Costs..Yr <- read.csv( Filename, row.names=1 )
Costs.YrCs <- as.matrix( Costs..Yr )
Inputs_$Costs.YrCs <- Costs.YrCs
rm( Filename, Costs.YrCs )

#PAYD input parameters
Filename <- "payd.csv"
TempInput.. <- read.csv( Filename, row.names=1 )
Inputs_$Payd..Yr <- TempInput..
rm( Filename, TempInput.. )

#Parking pricing
Filename <- "parking.csv"
TempInput.. <- read.csv( Filename )
Va <- TempInput..$Value
PkgParm_Va.. <- split( TempInput..[,-1], Va )
PkgParm_Va.. <- lapply( PkgParm_Va.., function(x) {
  RowNames. <- x[,1]
  Parm.. <- x[,-1]
  rownames( Parm.. ) <- RowNames.
  ColNames. <- colnames( Parm.. )
  ColNames. <- gsub( "X", "", ColNames. )
  colnames( Parm.. ) <- ColNames.
  Parm..
} )
Inputs_$PkgParm_Va.. <- PkgParm_Va..
rm( Filename, TempInput.., Va, PkgParm_Va.. )

#Electric vehicle range and proportion of VMT in range traveled by electricity
Filename <- "ev_characteristics.csv"
Inputs_$EvRangeProp..Yr <- read.csv( Filename, row.names=1 )
rm( Filename )

#Heavy vehicle MPG & power consumption by vehicle year and type:
Filename <- "hvy_veh_mpg_mpk.csv"
Inputs_$HvyVehMpgMpk..Yr <- read.csv( Filename, row.names=1 )
rm( Filename )

#Factors for adjusting 95 percentile age of vehicles
Filename <- "age_adj.csv"
Inputs_$AgeAdj.YrTy <- as.matrix( read.csv( Filename, row.names=1 ) )
rm( Filename )

#Congestion efficiency of vehicles by year (Yr) and powertrain (Pt)
Filename <- "cong_efficiency.csv"
Inputs_$CongEfficiency.YrPt <- as.matrix( read.csv( Filename, row.names=1 ) )
rm( Filename )

#Hybrid electric vehicle characteristics
Filename <- "hev_characteristics.csv"
Inputs_$HevMpgProp..Yr <- read.csv( Filename, row.names=1 )
rm( Filename )

#Heavy truck fuel type mix
Filename <- "heavy_truck_fuel.csv"
Inputs_$HvyTruckFuels..Yr <- read.csv( Filename, row.names=1 )
rm( Filename )

#Bus fuel type mix, hydrocarbon by type and electric
Filename <- "bus_fuels.csv"
TempInput.. <- read.csv( Filename, as.is=TRUE )
names(TempInput..) <- gsub("X", "", names(TempInput..))
#Create matrix of electric proportion by metropolitan area and year
BusPropElectric.. <- TempInput..[grep("PropElectric", TempInput..$Fuel), -2]
BusPropElectric.MaYr <- as.matrix(BusPropElectric..[,-1])
dimnames(BusPropElectric.MaYr) <- list(BusPropElectric..$Area, names(BusPropElectric..)[-1])  
Inputs_$BusPropElectric.MaYr <- BusPropElectric.MaYr[Ma, ]
#Create an array of hydrocarbon fuel proportions by fuel, year and metropolitan area
TempInput.. <- TempInput..[-grep("PropElectric", TempInput..$Fuel), ]
TempInput_ <- split( TempInput.., TempInput..[,1] )
Fp <- c( "PropGas", "PropCng", "DieselPropBio", "GasPropEth" )
BusFuels.FpYrMa <- array( 0, dim=c( length( Fp ), length( Yr ), length( Ma ) ),
                          dimnames=list( Fp, Yr, Ma ) )
for( ma in Ma ) {
  DataIn.. <- TempInput_[[ma]]
  DataOut.. <- DataIn..[ , 3:ncol( DataIn.. ) ]
  colnames( DataOut.. ) <- gsub( "X", "", colnames( DataOut.. ) )
  rownames( DataOut.. ) <- DataIn..[ , 2 ]
  BusFuels.FpYrMa[ , , ma ] <- as.matrix( DataOut.. )[ Fp, Yr ]
  rm( DataIn.., DataOut.. )
}
Inputs_$BusFuels.FpYrMa <- BusFuels.FpYrMa
rm( Filename, BusPropElectric.., BusPropElectric.MaYr, TempInput.., TempInput_, BusFuels.FpYrMa, ma, Fp )

#Commercial service vehicle light truck proportions
Filename <- "comm_service_lttruck_prop.csv"
TempInput.. <- read.csv( Filename, as.is=TRUE )
CommServiceLtTruckProp.Yr <- TempInput..$LtTruckProp
names( CommServiceLtTruckProp.Yr ) <- TempInput..$Year
Inputs_$CommServiceLtTruckProp.Yr <- CommServiceLtTruckProp.Yr
rm( Filename, TempInput.., CommServiceLtTruckProp.Yr )

#Commercial service vehicle fuels
Filename <- "comm_service_fuel.csv"
Inputs_$CommServiceFuels..Yr <- read.csv( Filename, row.names=1 )
rm( Filename )

#Commercial service vehicle EV VMT proportions
Filename <- "comm_service_pt_prop.csv"
Inputs_$CommServicePtProp..Yr <- read.csv( Filename, row.names=1 )
rm( Filename )

setwd( RunDir )

attach( Inputs_ )

