#modify_functions.R
#------------------
#This script modifies GreenSTEP functions for use in version 3.6.2.
#
#Version 3.6.2 is a bug-fix version which corrects a bug in the calculation of
#the base 


#MODIFY PAY-AS-YOU-DRIVE WEIGHTS FUNCTION
#========================================
# The pay-as-you-drive (PAYD) household weights that are used to determine which
# households are most likely to use PAYD insurance are modified. A 
# 'Metropolitan' weight is added and the the 'Teenager' weight is lowered (see 
# UpdatedPAYD_Wts.txt). PAYD insurance is assigned to the household as a whole, 
# not to individual members of the household. The function "estPaydWeights" 
# assigns weights to households based on several household attributes and 
# weighting factors related to several attributes. This script lowers the weight
# attribute for teenage drivers from 3 to 2. It adds a new weight for 
# metropolitan households with a value of 3.

#Define revised function to assign PAYD propensity weights to households
#-----------------------------------------------------------------------
#' Assign pay-as-you-drive insurance propensity weights to households
#' 
#' \code{estPaydWeights} Calculates household weight that reflect the relative
#' propensity of a household to purchase pay-as-you-drive insurance based on the
#' household characteristics
#' 
#' Household PAYD propensity weights are assigned based on the presence of 
#' teenagers, whether the average annual vehicle mileage is low, whether drivers
#' are older, whether household income is relatively low, whether their vehicles
#' tend to be autos rather than light trucks, and whether the household lives in
#' a metropolitan area. The likelihood that household is selected is directly
#' proportion to the weight assigned to the household. In order for a household
#' to qualify for PAYD insurance, all household vehicles must be a 1996 or later
#' model year. If the target proportion of PAYD households is greater than the
#' proportion of qualifying households, the result will be the qualifying
#' households.
#' 
#' @param Data.. A data frame of household characteristics. The data frame must
#'   contain these fields: "Age0to14", "Age15to19", "Age20to29", "Age30to54",
#'   "Age55to64", "Age65Plus", "Dvmt", "Hhvehcnt", "Hhincttl", "VehType",
#'   "VehAge", "Houseid", "DevType".
#' @param RunMode A string which determines whether function is run in "test"
#'   mode or "run" mode. When run in "test" mode, the function returns a data
#'   frame which includes a number of intermediate household calculations as
#'   well as the resulting assignment of households as PAYD or not. The default
#'   value is "run".
#' @param Weights_ A list of the weights assigned to each factor.
#' @return A numeric vector or a data frame. In "run" mode, a numeric vector 
#'   corresponding to the rows of the Data.. argument. Values of each element
#'   are the PAYD weights assigned to the households. These weights are used in
#'   the selection of households. In "test" mode, a data frame corresponding to
#'   the rows of the Data.. argument. The data frame columns are as follows:
#'   Weight.Hh: Numeric weight assigned to the household 
#'   Qualifies.Hh: Logical whether the household qualifies
#'   HasTeenager.Hh: Logical whether household has one or more teenage drivers
#'   HasLowMileage.Hh: Logical whether the average annual vehicle mileage < 15K
#'   PropOlderDrivers.Hh: Proportion of drivers over 19 years old 
#'   IsLowerInc.Hh: Logical whether the household income is less than $45,000
#'   PropAuto.Hh: Porportion of household vehicles that are automobiles
#'   IsMetropolitan.Hh: Logical whether household is in metropolitan area
#'   Houseid.Hh - The unique household id.
estPaydWeights <- 
  function(Data.., yr,
           RunMode = "run",
           Weights_ = list(
             TeenagerWt = 2,
             LowMileageWt = 3,
             OlderDriverWt = 2,
             IncomeWt = 2,
             AutoWt = 2,
             MetropolitanWt = 3)) {
    #Set up
    #------
    if(!(RunMode %in% c("run", "test"))) {
      stop("PAYD: estPaydWeights(): RunMode argument can only be 'run' or 'test'")
    }
    NumHh <- nrow(Data..)
    Houseid.Hh <- Data..$Houseid
    #Household qualifies if all household vehicles are later than 1995 model year
    #----------------------------------------------------------------------------
    HasOldCars.Hh <- unlist(lapply(Data..$VehAge, function(x) as.numeric(any(as.numeric(yr) - x <= 1995))))
    Qualifies.Hh <- !HasOldCars.Hh
    Qualifies.Hh[is.na(Qualifies.Hh)] <- 0
    HasOldCars.Hh[is.na(HasOldCars.Hh)] <- 0
    NumQualifying <- sum(Qualifies.Hh)
    #Calculate household PAYD weights
    #--------------------------------
    Weight.Hh <- rep(1, NumHh)
    #Add 2 points if any teenagers in household
    HasTeenager.Hh <- Data..$Age15to19 > 0
    with(Weights_, Weight.Hh <<- Weight.Hh + TeenagerWt * as.numeric(HasTeenager.Hh))
    #Add 2 points to weight if average annual vehicle miles is less than 15,000
    HasLowMileage.Hh <- (365 * Data..$Dvmt / Data..$Hhvehcnt) < 15000
    with(Weights_, Weight.Hh <<- Weight.Hh + LowMileageWt *  as.numeric(HasLowMileage.Hh))
    #Add 2 points times the proportion of drivers 30 or older
    PropOlderDrivers.Hh <- (Data..$Age20to29 + Data..$Age30to54 + Data..$Age55to64 + Data..$Age65Plus) /
      (Data..$Age15to19 + Data..$Age20to29 + Data..$Age30to54 + Data..$Age55to64 + Data..$Age65Plus )
    with(Weights_, Weight.Hh <<- Weight.Hh + OlderDriverWt * PropOlderDrivers.Hh )
    #Add 3 points if household income is less than $45,000
    IsLowerInc.Hh <- Data..$Hhincttl < 45000
    with(Weights_, Weight.Hh <<- Weight.Hh + IncomeWt * as.numeric(IsLowerInc.Hh))
    #Add 2 points times the proportion of vehicles that are autos
    PropAuto.Hh <- unlist( lapply( Data..$VehType, function(x) sum(x == "Auto") / length(x) ) )
    PropAuto.Hh[ is.na(PropAuto.Hh) ] <- 0
    with(Weights_, Weight.Hh <<- Weight.Hh + AutoWt * PropAuto.Hh)
    #Add 3 points for households that are located within a metropolitan area
    IsMetropolitan.Hh <- Data..$DevType == "Metropolitan"
    with(Weights_, Weight.Hh <<- Weight.Hh + MetropolitanWt * as.numeric(IsMetropolitan.Hh))
    #Set weights for non-qualifying households to 0
    Weight.Hh <- round(Weight.Hh * Qualifies.Hh, 1)
    #Return the result
    if(RunMode == "test"){
      return(
        data.frame(
          Weight.Hh, 
          HasOldCars.Hh, 
          Qualifies.Hh, 
          HasTeenager.Hh, 
          HasLowMileage.Hh,
          PropOlderDrivers.Hh, 
          IsLowerInc.Hh, 
          PropAuto.Hh, 
          IsMetropolitan.Hh,
          Houseid.Hh, 
          stringsAsFactors=FALSE))               
    } else {
      return(Weight.Hh)
    }
  }


#MODIFY COST CALCULATION FUNCTION
#================================
# The calcCosts function is modified to calculate a EV VMT surcharge tax that is
# equivalent to what the average gas tax is on a per mile basis. This replaces
# the previous function in which the EV VMT surcharge tax was calculated from an
# exogenously input per mile tax. The problem with the previous approach is that
# if the per mile EV VMT surcharge tax is too high, the road revenues will
# exceed road costs. Also, the vehicle registration fee is added as a component
# of the road tax. Previously it was considered a vehicle ownership cost, but
# not considered a road tax.

#Define revised function to calculate household vehicle costs
#------------------------------------------------------------
#' Calculate household vehicle use and ownership costs
#' 
#' \code{calcCosts} Calculates household vehicle use and ownership costs
#' 
#' Household vehicle use and ownership costs are tabulated. These include costs
#' that accrue directly to households as well as social (externality) costs that
#' accrue to society (present and future) and not to households directly.
#' 
#' @param Data.. A data frame of household characteristics. The data frame must
#'   contain these fields: "Dvmt", "FuelGallons", "FuelCo2e", "ElecCo2e",
#'   "ElecKwh", "HcVehDvmt", "EvVehDvmt", "DevType", "Payd", "Hhvehcnt",
#'   "Hhincttl".
#' @param Costs. A named numeric vector of unit costs that contains the
#' following elements: "AirPollution", "OtherResource", "Safety", "Noise",
#' "EnergySecurity", "ClimateChange", "PropExtPaid", "FuelCost", "KwhCost",
#' "GasTax", "EVVmtSurchargeProp", "VmtTax", "VehMaint", "VehTire",
#' "VehFin", "VehIns", "VehReg", "BaseMod", "PresOpMaint", "OtherRoad" 
#' @param PaydRate A number that represents the pay-as-you-drive insurance rate
#' in dollars per mile.
#' @param CongPrice A number that represents the average cost of congestion
#' prices in dollars per mile.
#' @param VmtSurcharge A number that represents the VMT surcharge tax paid to
#' balance road costs and road revenues.
#' @param ExtraModCost A number that represents the cost per mile to pay for
#' adding freeway and arterial lanes attributable to light-duty vehicle travel.
#' @param NonPrivateFactor A number which is a multiplier of the average
#' household vehicle use cost per mile to estimate the cost of using taxi
#' services.
#' @return A list containing the following numeric vector components of costs
#' per household:
#' FuelCost: Average daily cost of fuel in dollars.
#' PowerCost: Average daily cost of power in dollars. 
#' RoadUseTax: Average daily cost of road use taxes in dollars
#' (gas tax, equivalent EV mile tax, congestion price, VMT surcharge tax).
#' AddedExtTax: Average daily cost in dollars to pay externality cost taxes.
#' PaydCost: Average daily cost in dollars for pay-as-you-drive insurance. 
#' TotExtCost: Average daily cost in dollars of externalities (paid and unpaid).
#' HhTotCost: Total daily cost of household vehicle travel in dollars, not
#' including vehicle ownership costs. 
#' FutrCostPerMi: Total household cost in dollars per mile vehicle mile. 
#' VehOwnExp: Average daily cost to own vehicles by the household.
#' TotRoadCost: Average daily cost in dollars that household imposes on roadway 
#' use (maintenance, preservation, operations, modernization).  
#' CongTax: Average daily cost in dollars for congestion pricing.
calcCosts <- function( Data.., Costs., PaydRate, CongPrice, VmtSurcharge, ExtraModCost, 
                       NonPrivateFactor=5 ) {
  
  # Calculate external costs
  #-------------------------
  # These may or may not be charged to households depending on the scenarios
  # Calculate VMT-based costs
  VmtExtCost.Hh <- Data..$Dvmt * sum( Costs.[ c( "AirPollution", "OtherResource", "Safety", "Noise" ) ] )
  FuelExtCost.Hh <- Data..$FuelGallons * Costs.[ "EnergySecurity" ]
  Co2eExtCost.Hh <- ( Data..$FuelCo2e + Data..$ElecCo2e ) * Costs.[ "ClimateChange" ]
  TotExtCost.Hh <- VmtExtCost.Hh + FuelExtCost.Hh + Co2eExtCost.Hh
  # Calculate externality taxes exclusive of any carbon tax that is paid
  AddedExtTax.Hh <- TotExtCost.Hh * Costs.[ "PropExtPaid" ]		
  
  # Calculate household vehicle use costs for households that own vehicles 
  #-----------------------------------------------------------------------
  # Calculate daily fuel and power costs
  FuelCost.Hh <- Data..$FuelGallons * Costs.[ "FuelCost" ]
  PowerCost.Hh <- Data..$ElecKwh * Costs.[ "KwhCost" ]
  # Tabulate household DVMT powered by fuel and by electricity
  HcDvmt.Hh <- unlist(lapply(Data..$HcVehDvmt, sum))
  HcDvmt.Hh[is.na(HcDvmt.Hh)] <- 0
  EvDvmt.Hh <- unlist(lapply(Data..$EvVehDvmt, sum))
  EvDvmt.Hh[is.na(EvDvmt.Hh)] <- 0
  # Calculate gas tax and equivalent EV VMT taxes
  GasTax.Hh <- Data..$FuelGallons * Costs.[ "GasTax" ]
  EvGasEqDvmtTax <- 
    Costs.["EVVmtSurchargeProp"] * sum(GasTax.Hh) / sum(HcDvmt.Hh)
  # Calculate congestion taxes
  CongTax.Hh <- Data..$Dvmt * CongPrice
  CongTax.Hh[ Data..$DevType != "Metropolitan" ] <- 0
  # Calculate total VMT taxes
  EvDvmtTax <- Costs.["VmtTax"] + VmtSurcharge + EvGasEqDvmtTax
  HcDvmtTax <- Costs.["VmtTax"] + VmtSurcharge
  VmtTax.Hh <- EvDvmt.Hh * EvDvmtTax + HcDvmt.Hh * HcDvmtTax
  # Calculate daily cost of vehicle registration fee
  DailyEqVehReg.Hh <- Data..$Hhvehcnt * sum(Costs.["VehReg"]) / 365
  # Calculate total road use taxes
  RoadUseTax.Hh <- GasTax.Hh + CongTax.Hh + VmtTax.Hh + DailyEqVehReg.Hh
  # Calculate total daily PAYD cost
  PaydCost.Hh <- Data..$Dvmt * Data..$Payd * PaydRate
  # Calculate total vehicle cost
  TotCost.Hh <- FuelCost.Hh + PowerCost.Hh + RoadUseTax.Hh + AddedExtTax.Hh +
    PaydCost.Hh + Data..$DailyPkgCost
  
  # Calculate household vehicle use costs for households that don't own vehicles
  #----------------------------------------------------------------------------- 
  # Calculate the average cost per mile for households that have vehicles and DVMT
  BaseCost.Hh <- FuelCost.Hh + PowerCost.Hh + RoadUseTax.Hh + AddedExtTax.Hh
  HasVeh.Hh <- Data..$Hhvehcnt >= 1
  HasDvmt.Hh <- Data..$Dvmt > 0
  AveBaseCostMile <- mean( BaseCost.Hh[ HasVeh.Hh & HasDvmt.Hh ] / Data..$Dvmt[ HasVeh.Hh & HasDvmt.Hh ] )
  AveExtCostMile <- mean( TotExtCost.Hh[ HasVeh.Hh & HasDvmt.Hh ] / Data..$Dvmt[ HasVeh.Hh & HasDvmt.Hh ] )
  # Calculate vehicle costs for zero vehicle households and households that have no DVMT
  HasNoVehOrNoDvmt.Hh <- !HasVeh.Hh | !HasDvmt.Hh
  TotCost.Hh[ HasNoVehOrNoDvmt.Hh  ] <- Data..$Dvmt[ HasNoVehOrNoDvmt.Hh ] * 5 * AveBaseCostMile
  TotExtCost.Hh[ HasNoVehOrNoDvmt.Hh  ] <- Data..$Dvmt[ HasNoVehOrNoDvmt.Hh ] * AveExtCostMile
  FuelCost.Hh[ HasNoVehOrNoDvmt.Hh ] <- 0
  PowerCost.Hh[ HasNoVehOrNoDvmt.Hh ] <- 0
  RoadUseTax.Hh[ HasNoVehOrNoDvmt.Hh ] <- 0
  AddedExtTax.Hh[ HasNoVehOrNoDvmt.Hh ] <- 0
  PaydCost.Hh[ HasNoVehOrNoDvmt.Hh ] <- 0
  
  # Calculate the average household costs per mile to use in budget calculations
  #-----------------------------------------------------------------------------
  # Calculate the average cost per mile
  FutrCostPerMile.Hh <- TotCost.Hh / Data..$Dvmt
  # Calculate average cost per mile for households with no vehicles or no DVMT
  FutrCostPerMile.Hh[ HasNoVehOrNoDvmt.Hh ] <- 5 * AveBaseCostMile
  # Cap FutrCostPerMile.Hh at the 95th percentile
  MaxCostPerMile <- quantile(FutrCostPerMile.Hh, 0.95)
  FutrCostPerMile.Hh[FutrCostPerMile.Hh > MaxCostPerMile] <- MaxCostPerMile

  # Calculate household vehicle ownership costs
  #--------------------------------------------
  # Calculate total vehicle ownership expenses
  IsPayd.Hh <- Data..$Payd == 1
  VehOwnExp.Hh <-numeric( length( IsPayd.Hh ) )
  VehOwnExp.Hh[ HasVeh.Hh & IsPayd.Hh ] <- Data..$DepExp[ HasVeh.Hh & IsPayd.Hh ] / 365 + 
    Data..$Dvmt[ HasVeh.Hh & IsPayd.Hh ] * sum( Costs.[ c( "VehMaint", "VehTire" ) ] ) +
    Data..$Hhvehcnt[ HasVeh.Hh & IsPayd.Hh ] * sum( Costs.[ c( "VehFin", "VehReg" ) ] ) / 365
  VehOwnExp.Hh[ HasVeh.Hh & !IsPayd.Hh ] <- Data..$DepExp[ HasVeh.Hh & !IsPayd.Hh ] / 365 + 
    Data..$Dvmt[ HasVeh.Hh & !IsPayd.Hh ] * sum( Costs.[ c( "VehMaint", "VehTire" ) ] ) +
    Data..$Hhvehcnt[ HasVeh.Hh & !IsPayd.Hh ] * sum( Costs.[ c( "VehFin", "VehIns", "VehReg" ) ] ) / 365
  # Cap total ownership expenses at 25% of household income
  ExpTooHigh. <- ( VehOwnExp.Hh * 365 / Data..$Hhincttl ) > 0.25
  VehOwnExp.Hh[ ExpTooHigh. ] <- 0.25 * Data..$Hhincttl[ ExpTooHigh. ] / 365
  
  # Calculate total road expenses (to build, repair, operate, etc.)
  #----------------------------------------------------------------
  BaseModCost.Hh <- Data..$Dvmt * Costs.[ "BaseMod" ]
  PresOpMaintCost.Hh <- Data..$Dvmt * Costs.[ "PresOpMaint" ]
  OtherRoadCost.Hh <- Data..$Dvmt * Costs.[ "OtherRoad" ]
  ExtraModCost.Hh <- Data..$Dvmt * ExtraModCost
  TotRoadCost.Hh <- BaseModCost.Hh + PresOpMaintCost.Hh + OtherRoadCost.Hh + ExtraModCost.Hh 
  
  # Return the result
  list( FuelCost=FuelCost.Hh, PowerCost=PowerCost.Hh, RoadUseTax=RoadUseTax.Hh,
        AddedExtTax=AddedExtTax.Hh, PaydCost=PaydCost.Hh, TotExtCost=TotExtCost.Hh, 
        HhTotCost=TotCost.Hh, FutrCostPerMi=FutrCostPerMile.Hh, VehOwnExp=VehOwnExp.Hh, 
        TotRoadCost=TotRoadCost.Hh, CongTax=CongTax.Hh, EvGasEqDvmtTax=EvGasEqDvmtTax )
}


#CORRECT ERROR IN calcCommVehTravelFromHhDvmt FUNCTION
#=====================================================
#' Calculate commercial service vehicle DVMT from household DVMT
#' 
#' \code{calcCommVehTravelFromHhDvmt} Calculates commercial service vehicle
#' DVMT from household DVMT
#' 
#' Light-duty commercial service vehicle travel can be calculated from household
#' DVMT or household income. If the CommVehDvmtMethod is set equal to
#' "HouseholdDvmt", the calcCommVehTravelFromHhDvmt function is called. It
#' either uses a factor specified in the CommVmtFactor parameter or if that
#' parameter is NULL it calculates the factor using a model input for the state
#' of the total light-duty vehicle DVMT to calculate it.
#' 
#' @param Dvmt.CoDt A numeric matrix of the total household DVMT by county and
#' development type.
#' @return A list containing the following two named components:
#' CommVehDvmt: The statewide total commercial service vehicle DVMT,
#' CommVehDvmt.CoDt: Commercial service vehicle DVMT by county and development
#' type.
calcCommVehTravelFromHhDvmt <- function(Dvmt.CoDt) {
  if (yr == BaseYear) {
    if (!(is.null(CommVmtFactor))) {
      CommVehDvmt.CoDt <- CommVmtFactor * Dvmt.CoDt
    } else {
      CommVehVmt <- (BaseYrVmt / 365) - sum(Dvmt.CoDt)
      CommVmtFactor <- CommVehVmt / sum(Dvmt.CoDt)
      Filename <-
        paste(ModelDir, "/", "CommVehFactor", ".RData", sep = "")
      save(CommVmtFactor, file = Filename)
      CommVehDvmt.CoDt <- CommVehVmt * Dvmt.CoDt / sum(Dvmt.CoDt)
    }
  } else {
    if (!(is.null(CommVmtFactor))) {
      CommVehDvmt.CoDt <- CommVmtFactor * Dvmt.CoDt
    } else {
      Filename <-
        paste(ModelDir, "/", "CommVehFactor", ".RData", sep = "")
      load(Filename)
      CommVehDvmt.CoDt <- CommVmtFactor * Dvmt.CoDt
    }
  }
  CommVehDvmt <- sum(CommVehDvmt.CoDt)
  list(CommVehDvmt = CommVehDvmt, CommVehDvmt.CoDt = CommVehDvmt.CoDt)
}

#ADD MODIFIED FUNCTIONS TO GREENSTEP AND SAVE
#============================================

#Load previous GreenSTEP_
load("data/previous/GreenSTEP_.RData")

#Replace estPaydWeights function
GreenSTEP_$estPaydWeights <- estPaydWeights

#Replace calcCosts function
GreenSTEP_$calcCosts <- calcCosts

#Replace calcCommVehTravelFromHhDvmt function
GreenSTEP_$calcCommVehTravelFromHhDvmt <- calcCommVehTravelFromHhDvmt
  
#Save the revised GreenSTEP_
save(GreenSTEP_, file = "data/revised/GreenSTEP_.RData")
