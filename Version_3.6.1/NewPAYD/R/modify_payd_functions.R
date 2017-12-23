#modify_payd_functions.R
#-----------------------
# This script modifies the pay-as-you-drive (PAYD) household weights that are 
# used to determine which households are most likely to use PAYD insurance A
# 'Metropolitan' weight is added and the the 'Teenager' weight is lowered (see
# UpdatedPAYD_Wts.txt). PAYD insurance is assigned to the household as a whole,
# not to individual members of the household. The function "estPaydWeights" 
# assigns weights to households based on several household attributes and 
# weighting factors related to several attributes. This script lowers the weight
# attribute for teenage drivers from 3 to 2. It adds a new weight for
# metropolitan households with a value of 3.

#Define new function to assign PAYD propensity weights to households
#-------------------------------------------------------------------
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


#Incorporate new function into previous GreenSTEP_ and save as new GreenSTEP_
#----------------------------------------------------------------------------
#Load previous GreenSTEP_
load("data/previous/GreenSTEP_.RData")

#Replace estPaydWeights function
GreenSTEP_$estPaydWeights <- estPaydWeights

#Save the revised GreenSTEP_
save(GreenSTEP_, file = "data/revised/GreenSTEP_.RData")
