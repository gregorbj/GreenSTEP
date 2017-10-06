#===============
#GreenSTEP_Sim.r
#===============
#Copyright 2009 - 2016, Oregon Department of Transportation 
#Licensed under the Apache License, Version 2.0 (the "License"); you may not use
#this file except in compliance with the License. You may obtain a copy of the
#License at http://www.apache.org/licenses/LICENSE-2.0 
#Unless required by applicable law or agreed to in writing, software distributed
#under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
#CONDITIONS OF ANY KIND, either express or implied.  See the License for the
#specific language governing permissions and limitations under the License.
#Version: 3.6.1 
#Date: 8/15/17

#Description
#===========
#This module performs all of the household microsimulation calculations for
#determining household income, vehicle ownership, household travel and vehicle
#characteristics and use. The results are aggregated to arrays by county, income
#and development type and saved to disk.

#This version incorporates the alternative modes trip models from the RSPM V3.6.
#It also changes the way that lane-miles and transit revenue miles are
#calculated. For lane-mile calculations, rather than calculating lane-miles from
#base-year values and model year growth rates, the lane-mile values are input by
#year. The same approach is taken for transit revenue miles. In another
#change for calculating transit revenue miles, the revenue miles are input for
#8 transit modes. These are multiplied by bus-equivalency factors to compute
#bus-equivalent revenue miles. Those bus-equivalent revenue miles are used to
#compute transit revenue miles per capita.

#===================================
#SET UP AND PRELIMINARY CALCULATIONS
#===================================

# Make a scenario outputs directory
if(!file.exists(OutputDir)) {
  dir.create(OutputDir)
}
# Make a vector of years that are past years (base year and before )
PastYears. <-
  as.character(as.numeric(Yr)[as.numeric(Yr) <= as.numeric(BaseYear)])

#Calculate Roadway Construction Costs
#------------------------------------
#In order to annualize the estimate of roadway construction costs (i.e. the cost
#of adding lane miles), it is necessary to calculate using all years for which
#there are input data, not just the run years. To do the calculations, the
#metropolitan area population needs to be estimated because freeway and arterial
#lane miles are specified in per capita terms. The metropolitan population
#estimates will not exactly equal the estimates calculated in later steps below
#because of stochastic effects. The difference will be very small and the effect
#on the calculation of annual cost will be very small.

#Initialize arrays
Pop.CoYr <- array(0, dim=c(length(Co), length(Yr)), dimnames=list(Co, Yr))
#Iterate by year and tabulate total population by county
for( yr in Yr ) {
  #Load the population data
  PopDir <- paste0(ModelDir, "/pop_forecasts")
  Pop.. <- read.csv(paste0(PopDir, "/pop_by_age_", yr, ".csv"), row.names=1)
  Pop.CoYr[,yr] <- rowSums(Pop..)[Co]
}
rm(PopDir, Pop.., yr)
#Estimate the metropolitan area population of each county
MetroPop.CoYr <- Pop.CoYr * 0
for (yr in Yr) {
  if (yr %in% PastYears.) {
    MetroPop.CoYr[, yr] <-
      UrbRurPopProp.CoDt[, "Metropolitan"] * Pop.CoYr[, yr]
  } else {
    BasePop.Co <-  Pop.CoYr[, BaseYear]
    PopGrowth.Co <- Pop.CoYr[, yr] - BasePop.Co
    MetroPop.CoYr[, yr] <-
      UrbRurGrowthSplit.CoDt[, "Metropolitan"] * PopGrowth.Co +
      UrbRurPopProp.CoDt[, "Metropolitan"] * BasePop.Co
  }
}
MetroPop.MaYr <-
  apply(MetroPop.CoYr, 2, function(x)
    tapply(x, CountyGroups..$Msa, sum))[Ma, Yr]
rm(Pop.CoYr, MetroPop.CoYr, BasePop.Co, PopGrowth.Co, yr)
#Calculate the annual metropolitan freeway construction cost for added
#lane-miles per year
FwyLnMi.Yr <- 
  unlist(lapply(LaneMiles_Yr.MaFc, function(x) {
    sum(x[,"Fwy"])
  }))
FwyLnMiChg.Yx <- diff(FwyLnMi.Yr)
NumYear.Yx <- diff(as.numeric(names(FwyLnMi.Yr)))
FwyLnMiAnnualCosts.Yr <- FwyLnMi.Yr * 0
FwyLnMiAnnualCosts.Yr[names(FwyLnMiChg.Yx)] <-
  (FwyLnMiChg.Yx / NumYear.Yx) * 1000 * Costs.YrCs[names(FwyLnMiChg.Yx), "FwyLnMi"]
FwyLnMiAnnualCosts.Yr[1] <- FwyLnMiAnnualCosts.Yr[2]
rm(FwyLnMi.Yr, FwyLnMiChg.Yx, NumYear.Yx)
#Calculate the annual metropolitan arterial construction cost for added 
#lane-miles per year
ArtLnMi.Yr <- 
  unlist(lapply(LaneMiles_Yr.MaFc, function(x) {
    sum(x[,"Art"])
  }))
ArtLnMiChg.Yx <- diff(ArtLnMi.Yr)
NumYear.Yx <- diff(as.numeric(names(ArtLnMi.Yr)))
ArtLnMiAnnualCosts.Yr <- ArtLnMi.Yr * 0
ArtLnMiAnnualCosts.Yr[names(ArtLnMiChg.Yx)] <-
  (ArtLnMiChg.Yx / NumYear.Yx) * 1000 * Costs.YrCs[names(ArtLnMiChg.Yx), "ArtLnMi"]
ArtLnMiAnnualCosts.Yr[1] <- ArtLnMiAnnualCosts.Yr[2]
rm(ArtLnMi.Yr, ArtLnMiChg.Yx, NumYear.Yx)
#Calculate the total annual cost for adding lane miles
AnnLnMiAddCosts.Yr <- FwyLnMiAnnualCosts.Yr + ArtLnMiAnnualCosts.Yr
rm(FwyLnMiAnnualCosts.Yr, ArtLnMiAnnualCosts.Yr)


#====================================================================
#RUN THE MODEL FOR EACH RUN YEAR SPECIFIED IN THE RUN PARAMETERS FILE
#====================================================================

#The model iterates through each forecast year. The results for the year are stored in a list with a component for each year.

for (yr in RunYears) {

  #Set a random seed to make run replicable 
  #---------------------------------------- If the UsedSavedRandomSeed input
  #parameter is TRUE a saved random seed will be retrieved Otherwise a new
  #random seed will be set and saved
  RandomSeedFile <-
    paste(InputDir, "/RandomSeedValue", yr, ".RData", sep = "")
  if (UseSavedRandomSeed & file.exists(RandomSeedFile)) {
    RandomSeedValue <- assignLoad(RandomSeedFile)
  } else {
    RandomSeedValue <- sample(1:1000000, 1)
    save(RandomSeedValue, file = RandomSeedFile)
  }
  #Set random seed for model run
  set.seed(RandomSeedValue)
  
  #Print run year to console
  print(yr)
  #Save start time for year iteration
  OverallStart <- Sys.time()
  
  #Set up directories
  #------------------
  SynPopYearDir <- paste0(ModelDir, "/SynHsld/Year", yr)
  OutputYearDir <- paste0(OutputDir, "/Year", yr)
  if (!file.exists(OutputYearDir))
    dir.create(OutputYearDir)
  OutputBaseYearDir <- paste0(OutputDir, "/Year", BaseYear)
  
  
  #============================================================================
  #STEP 1: ADD LAND USE AND TRANSPORTATION SYSTEM CHARACTERISTICS TO HOUSEHOLDS
  #============================================================================
  
  #Step 1a: Model income and land use attributes
  #=============================================
  
  #Load files
  load(paste0(SynPopYearDir, "/Hsld_Co.HhAp.RData"))
  
  #Make matrices to store population sums and income sums by county and
  #development type
  Pop.CoDt <-
    array(0,
          dim = c(length(Co), length(Dt)),
          dimnames = list(Co, Dt))
  Inc.CoDt <-
    array(0,
          dim = c(length(Co), length(Dt)),
          dimnames = list(Co, Dt))
  
  #Iterate through counties and add attributes
  for (co in Co) {
    print(co)
    
    #Wrap inside a local function to reduce potential for naming conflicts
    local({
      #Extract the synthetic population for the county
      SynPop.. <- data.frame(Hsld_Co.HhAp[[co]])
      
      # Give each household a unique id
      SynPop..$Houseid <- 1:nrow(SynPop..)
      
      #Calculate the household size
      SynPop..$Hhsize <- rowSums(SynPop..[, Ap])
      
      #Predict household income
      PerCapInc <- PerCapInc.Yr[yr] * IncProp.CoYr[co, yr]
      Inc_ <- predictIncome(
        Data.. = SynPop..[,Ap],
        Model = IncModel_$Formula,
        Dispersion = IncModel_$Dispersion,
        Pow = IncModel_$Pow,
        AreaInc = PerCapInc,
        MatchInc = TRUE,
        MaxIter = 50,
        IncConverge = 0.001
      )
      SynPop..$Hhincttl <- Inc_$PredInc
      SynPop..$Hhincttl[SynPop..$Hhincttl <= 0] <- 1
      MinInc <- quantile(SynPop..$Hhincttl, prob = 0.01)
      SynPop..$Hhincttl[SynPop..$Hhincttl < MinInc] <- MinInc
      
      #Classify households according to income group
      MaxInc <- max(SynPop..$Hhincttl)
      IncBreaks. <-
        c(0, 20000, 40000, 60000, 80000, 100000, MaxInc)
      SynPop..$IncGrp <-
        cut(
          SynPop..$Hhincttl,
          breaks = IncBreaks.,
          labels = Ig,
          include.lowest = TRUE
        )
      
      #Calculate total population and households
      Pop <- sum(colSums(SynPop..[, 1:6]))
      Hhslds <- nrow(SynPop..)
      
      #Calculate the urban rural household proportions
      #Assume the household proportions are the same as the population
      #proportions
      if (yr %in% PastYears.) {
        UrbRurHhProp.Dt <- UrbRurPopProp.CoDt[co, ]
      } else {
        PopGrowth <- Pop - sum(BasePop.CoDt[co,])
        PopGrowth.Dt <- PopGrowth * UrbRurGrowthSplit.CoDt[co,]
        UrbRurPop.Dt <- BasePop.CoDt[co,] + PopGrowth.Dt
        UrbRurHhProp.Dt <- UrbRurPop.Dt / sum(UrbRurPop.Dt)
      }
      
      #Assign development types to households
      SynPop..$DevType <-
        sample(Dt, Hhslds, replace = TRUE, prob = UrbRurHhProp.Dt)
      
      #Calculate population by development type and save
      UrbRurPop.Dt <- rep(0, length(Dt))
      names(UrbRurPop.Dt) <- Dt
      UrbRurPop.Dx <-
        tapply(SynPop..$Hhsize, SynPop..$DevType, sum)
      UrbRurPop.Dt[names(UrbRurPop.Dx)] <- UrbRurPop.Dx
      Pop.CoDt[co,] <<- UrbRurPop.Dt
      
      #Recalculate the population growth by development type because the 
      #assumption that household proportions are the same as population 
      #proportions and the household sampling process changes the population
      #growth proportions from inputs
      if (!(yr %in% PastYears.)) {
        PopGrowth.Dt <- UrbRurPop.Dt - BasePop.CoDt[co,]
      }
      
      #Calculate the urban growth boundary area
      UgbAreas.Ut <- BaseUgbAreas.CoUt[co,]
      if (!(yr %in% PastYears.)) {
        PopGrowthRate.Dt <- PopGrowth.Dt / BasePop.CoDt[co,]
        UgbAreas.Ut <-
          (1 + PopGrowthRate.Dt[Ut] * UgbAreaGrowthRates.CoUt[co, ]) *
          UgbAreas.Ut
      }
      
      # Calculate urban density
      HasMetro <- UrbRurPop.Dt["Metropolitan"] != 0
      TownDen <- UrbRurPop.Dt["Town"] / UgbAreas.Ut["Town"]
      if (HasMetro) {
        MetroDen <-
          UrbRurPop.Dt["Metropolitan"] / UgbAreas.Ut["Metropolitan"]
      }
      
      #Calculate density distribution for metropolitan areas
      if (HasMetro) {
        ma <- CountyGroups..[co, "Msa"]
        UrbProp <- UrbanTypeProp.YrMa[yr, ma]
        DenUrbResults_ <-
          predictDensityUrban(MetroDen, UbzDenModel_, UrbProp = UrbProp)
      }
      
      #Calculate average rural density
      AveRuralDen <- AveRuralDen.Co[co]
      if (as.numeric(yr) > 2005) {
        if (PopGrowth.Dt["Rural"] > 0) {
          RuralPop <- UrbRurPop.Dt["Rural"]
          BaseRuralPop <- BasePop.CoDt[co, "Rural"]
          RuralPopGrowth <- PopGrowth.Dt["Rural"]
          AveRuralDen <- (AveRuralDen * BaseRuralPop / RuralPop) +
            (120 * RuralPopGrowth / RuralPop)
        }
      }
      
      #Assign density and urban values to households
      Den. <- Urb. <- numeric(nrow(SynPop..))
      Den.[SynPop..$DevType == "Town"] <- TownDen
      Den.[SynPop..$DevType == "Rural"] <- AveRuralDen
      if (HasMetro) {
        NumMetroHh <- sum(SynPop..$DevType == "Metropolitan")
        MetroDen. <- sample(
          DenUrbResults_$DenValues.,
          NumMetroHh,
          replace = TRUE,
          prob = DenUrbResults_$DenProbs.
        )
        Den.[SynPop..$DevType == "Metropolitan"] <- MetroDen.
        UrbProb. <- DenUrbResults_$UrbanProbs.[match(MetroDen.,
                                                     DenUrbResults_$DenValues.)]
        MetroUrb. <- sapply(UrbProb., function(x) {
          sample(c(1, 0), 1, prob = c(x, 1 - x))
        })
        Urb.[SynPop..$DevType == "Metropolitan"] <- MetroUrb.
      }
      SynPop..$Htppopdn <- Den.
      SynPop..$Urban <- Urb.
      
      #Calculate the natural log of density
      SynPop..$LogDen <- log(SynPop..$Htppopdn)
      
      #Calculate driving age population
      SynPop..$DrvAgePop <- rowSums(SynPop..[, Ap[-1]])
      
      #Create a variable identifying driver population levels
      DrvLevels. <- c(0, 1, 2, max(SynPop..$DrvAgePop))
      SynPop..$DrvLevels <-
        as.character(cut(
          SynPop..$DrvAgePop,
          breaks = DrvLevels.,
          labels = c("Drv1", "Drv2", "Drv3Plus")
        ))
      
      #Identify households having only elderly persons
      SynPop..$OnlyElderly <-
        as.numeric(SynPop..$DrvAgePop == SynPop..$Age65Plus)
      
      #Sum income by development type
      Inc.Dt <- tapply(SynPop..$Hhincttl, SynPop..$DevType, sum)
      Inc.CoDt[co, names(Inc.Dt)] <<- Inc.Dt
      
      #Save the synthetic population for the county
      Filename <- paste(OutputYearDir, "/", co, ".RData", sep = "")
      save(SynPop.., file = Filename)
      
      #Exit local function
    })
    
    #End for loop through counties
  }
  
  #Save the population and income tabulations
  save(Pop.CoDt, file = paste0(OutputYearDir, "/Pop.CoDt.RData"))
  save(Inc.CoDt, file = paste0(OutputYearDir, "/Inc.CoDt.RData"))
  
  if (yr != BaseYear) {
    BasePop.CoDt <-
      assignLoad(paste0(OutputBaseYearDir, "/Pop.CoDt.RData"))
    BaseInc.CoDt <-
      assignLoad(paste0(OutputBaseYearDir, "/Inc.CoDt.RData"))
  } else {
    BasePop.CoDt <- Pop.CoDt
    BaseInc.CoDt <- Inc.CoDt
  }
  
  #Step 1b: Calculate freeway, arterial and transit supply by metropolitan area
  #============================================================================
  
  #Log start of procedure
  StartTime <- Sys.time()
  print("Calculating freeway and transit supply")
  
  #Wrap in local function to reduce clutter and potential for conflict in global
  #environment
  local({
    # Initialize objects to store results
    FwyLnMiCap.Ma <- numeric(length(Ma))
    names(FwyLnMiCap.Ma) <- Ma
    ArtLnMiCap.Ma <- numeric(length(Ma))
    names(ArtLnMiCap.Ma) <- Ma
    FwyLnMi.Ma <- numeric(length(Ma))
    names(FwyLnMi.Ma) <- Ma
    ArtLnMi.Ma <- numeric(length(Ma))
    names(ArtLnMi.Ma) <- Ma
    TranRevMiCap.Ma <- numeric(length(Ma))
    names(TranRevMiCap.Ma) <- Ma
    BusRevMi.Ma <- numeric(length(Ma))
    names(BusRevMi.Ma) <- Ma
    RailRevMi.Ma <- numeric(length(Ma))
    names(RailRevMi.Ma) <- Ma
    
    #Iterate through metropolitan areas and do the calculations
    for (ma in Ma) {
      #Select the counties in the metropolitan area
      Mc <- rownames(CountyGroups..)[CountyGroups..$Msa %in% ma]
      
      #Calculate the metropolitan population
      MetroPop <- sum(BasePop.CoDt[Mc, "Metropolitan"])
      
      #Calculate per capita freeway lane miles
      FwyLnMi.Ma[ma] <- LaneMiles_Yr.MaFc[[yr]][ma, "Fwy"]
      FwyLnMiCap.Ma[ma] <- 1000 * FwyLnMi.Ma[ma] / MetroPop

      #Calculate per capita arterial lane miles
      ArtLnMi.Ma[ma] <- LaneMiles_Yr.MaFc[[yr]][ma, "Art"]
      ArtLnMiCap.Ma[ma] <- 1000 * ArtLnMi.Ma[ma] / MetroPop

      # Calculate rail and transit servie and bus-equivalent per capita 
      # transit revenue miles
      BusRevMi.Ma[ma] <- sum(PTService_Yr.MaMp[[yr]][ma, BusModes])
      RailRevMi.Ma[ma] <- sum(PTService_Yr.MaMp[[yr]][ma, RailModes])
      BusEqService.Mp <- 
        PTService_Yr.MaMp[[yr]][ma, Mp] * RevMiFactors[Mp]
      TranRevMiCap.Ma[ma] <- sum(BusEqService.Mp) / MetroPop
      
    # Assign results to the global environment
    FwyLnMiCap.Ma <<- FwyLnMiCap.Ma
    ArtLnMiCap.Ma <<- ArtLnMiCap.Ma
    FwyLnMi.Ma <<- FwyLnMi.Ma
    ArtLnMi.Ma <<- ArtLnMi.Ma
    TranRevMiCap.Ma <<- TranRevMiCap.Ma
    BusRevMi.Ma <<- BusRevMi.Ma
    RailRevMi.Ma <<- RailRevMi.Ma
    
  }
  })
  
  # Save the results
  #-----------------
  Filename <- paste(OutputYearDir, "/FwyLnMiCap.Ma.RData", sep = "")
  save(FwyLnMiCap.Ma, file = Filename)
  rm(Filename)
  Filename <- paste(OutputYearDir, "/ArtLnMiCap.Ma.RData", sep = "")
  save(ArtLnMiCap.Ma, file = Filename)
  rm(Filename)
  Filename <-
    paste(OutputYearDir, "/TranRevMiCap.Ma.RData", sep = "")
  save(TranRevMiCap.Ma, file = Filename)
  rm(Filename)
  Filename <- paste(OutputYearDir, "/BusRevMi.Ma.RData", sep = "")
  save(BusRevMi.Ma, file = Filename)
  rm(Filename)
  Filename <- paste(OutputYearDir, "/RailRevMi.Ma.RData", sep = "")
  save(RailRevMi.Ma, file = Filename)
  rm(Filename)
  
  #Report end of procedure
  print(StartTime)
  print(Sys.time())
  
  #Step 1c: Add the freeway and transit supply data to the SynPop..
  #================================================================
  
  #Log start of procedure
  StartTime <- Sys.time()
  print("Adding freeway and transit supply to synthetic households")
  
  for (co in Co) {
    print(co)
    
    local({
      # Load county file
      Filename <- paste(OutputYearDir, "/", co, ".RData", sep = "")
      SynPop.. <- assignLoad(Filename)
      
      # Make calculations
      SynPop..$Fwylnmicap <- 0
      SynPop..$Fwylnmicap[SynPop..$DevType == "Metropolitan"] <-
        FwyLnMiCap.Ma[CountyGroups..[co, "Msa"]]
      SynPop..$Tranmilescap <- 0
      SynPop..$Tranmilescap[SynPop..$DevType == "Metropolitan"] <-
        TranRevMiCap.Ma[CountyGroups..[co, "Msa"]]
      
      # Save results
      Filename <- paste(OutputYearDir, "/", co, ".RData", sep = "")
      save(SynPop.., file = Filename, compress = TRUE)
      
      rm(SynPop..)
      gc()
      
    })
    
  }
  
  #Report end of procedure
  print(StartTime)
  print(Sys.time())
  
  
  #=================================================================
  #STEP 2: SIMULATE HOUSEHOLD TRAVEL CHARACTERISTICS FOR EACH COUNTY
  #=================================================================
  
  #Log start of procedure
  StartTime <- Sys.time()
  print("Simulation of household travel characteristics")
  
  #Iterate through counties
  #========================
  
  for (co in Co) {
    #Report county
    print(co)
    
    # Load county file
    Filename <- paste(OutputYearDir, "/", co, ".RData", sep = "")
    SynPop.. <- assignLoad(Filename)
    
    # Identify metropolitan area
    MetroArea <- CountyGroups..[co, "Msa"]
    IsMetro. <- SynPop..$DevType == "Metropolitan"
    
    #Step 2a: Identify households affected by travel demand management or
    #vehicle o&m programs
    #=====================================================================
    
    #Identify ECO and IMP households
    #-------------------------------
    SynPop..$NumEco <- 0
    ModelVar. <- c("DrvAgePop", "Houseid")
    if (any(IsMetro.)) {
      SynPop..$NumEco[IsMetro.] <-
        idEcoWorkers(SynPop..[IsMetro., ModelVar.],
                     PropWrkEco = PropWrkEco.MaYr[MetroArea, yr])
    }
    rm(ModelVar.)
    SynPop..$ImpHh <- 0
    ModelVar. <- c("Htppopdn", "Urban", "Houseid")
    if (any(IsMetro.)) {
      ImpHh_ <- idImpHouseholds(SynPop..[IsMetro., ModelVar.],
                                ImpPropGoal = ImpPropGoal.MaYr[MetroArea, yr])
      SynPop..$ImpHh[IsMetro.] <- ImpHh_$ImpHh
      rm(ImpHh_)
    }
    rm(ModelVar.)
    
    #Identify eco-driver and low rolling-resistance tire households
    #--------------------------------------------------------------
    # Note: reason for form of 1st argument is to pass a data frame with
    # minimal size to the functions.
    ModelVar. <- "Houseid"
    TempInputData.. <- data.frame(SynPop..[, "Houseid"])
    SynPop..$IsEcoDriver <- idEcoDriverHh(TempInputData..,
                                          EcoTire..Yr[yr, "EcoDrvProp"])
    SynPop..$IsLowRollTire <- idLowRollTire(TempInputData..,
                                            EcoTire..Yr[yr, "LowRollProp"])
    rm(ModelVar., TempInputData..)
    
    #Step 2b: Calculate vehicle ownership and adjust for carsharing
    #==============================================================
    
    #Calculate initial vehicle ownership
    #-----------------------------------
    #Initialize Hhvehcnt and VehPerDrvAgePop variables
    SynPop..$Hhvehcnt <- 0
    SynPop..$VehPerDrvAgePop <- 0
    # Predict ownership for metropolitan households if any exist
    if (any(IsMetro.)) {
      ModelVar. <- c(
        "Hhincttl",
        "Htppopdn",
        "Tranmilescap",
        "Urban",
        "Fwylnmicap",
        "OnlyElderly",
        "DrvLevels",
        "DrvAgePop"
      )
      MetroVehOwn_ <-
        predictVehOwn(SynPop..[IsMetro., ModelVar.],
                      Model_ = VehicleOwnModels_, Type =
                        "Metro")
      rm(ModelVar.)
    }
    #Predict ownership for nonmetropolitan households if any exist
    if (any(!IsMetro.)) {
      ModelVar. <-
        c("Hhincttl",
          "Htppopdn",
          "OnlyElderly",
          "DrvLevels",
          "DrvAgePop")
      NonMetroVehOwn_ <-
        predictVehOwn(SynPop..[!IsMetro., ModelVar.],
                      Model_ = VehicleOwnModels_, Type =
                        "NonMetro")
      rm(ModelVar.)
    }
    #Assign values to SynPop.. and return the result
    if (any(IsMetro.)) {
      SynPop..$Hhvehcnt[IsMetro.] <- MetroVehOwn_$NumVeh
      SynPop..$VehPerDrvAgePop[IsMetro.] <- MetroVehOwn_$VehRatio
    }
    if (any(!IsMetro.)) {
      SynPop..$Hhvehcnt[!IsMetro.] <- NonMetroVehOwn_$NumVeh
      SynPop..$VehPerDrvAgePop[!IsMetro.] <-
        NonMetroVehOwn_$VehRatio
    }
    #Clean up
    if (exists("MetroVehOwn_"))
      rm(MetroVehOwn_)
    if (exists("NonMetroVehOwn_"))
      rm(NonMetroVehOwn_)
    
    # Identify carshare households
    #-----------------------------
    SynPop..$Carshare <- 0
    ModelVar. <-
      c("Hhvehcnt", "Hhsize", "Age65Plus", "Htppopdn", "Houseid")
    if (any(IsMetro.)) {
      SynPop..$Carshare[IsMetro.] <-
        idCarshareHh(
          SynPop..[IsMetro., ModelVar.],
          CarshareRates. = c(
            MedDen = CarshareParm_Va..$MedDenRate[MetroArea, yr],
            HighDen =
              CarshareParm_Va..$HighDenRate[MetroArea, yr]
          )
        )
    }
    rm(ModelVar.)
    
    #Adjust vehicle ownership to account for carsharing
    #--------------------------------------------------
    if (any(IsMetro.)) {
      SynPop.. <-
        adjCarshareOwn(
          SynPop..,
          OneCarProb. = c(C0 = 0.66, C1 = 0.34),
          TwoCarProb. = c(C0 = 0.17, C1 = 0.56, C2 =
                            0.27),
          ThreeCarProb. = c(
            C0 = 0.15,
            C1 = 0.21,
            C2 = 0.22,
            C3 = 0.42
          )
        )
    }
    
    #Step 2c: 1st DVMT calculation (no adjustment for costs)
    #=======================================================
    
    #Calculate the average DVMT
    #--------------------------
    ModelVar. <-
      c(
        "Hhincttl",
        "Htppopdn",
        "Hhvehcnt",
        "Tranmilescap",
        "Fwylnmicap",
        "DrvAgePop",
        "Hhsize",
        "Age0to14",
        "Age15to19",
        "Age20to29",
        "Age30to54",
        "Age55to64",
        "Age65Plus",
        "Urban",
        "BaseCostPerMi",
        "FutrCostPerMi"
      )
    #Assume a base and future cost of 4 cents per mile
    #so that budget constraints don't impinge on the amount of vehicle travel
    SynPop..$BaseCostPerMi <- 4 / 100
    SynPop..$FutrCostPerMi <- 4 / 100
    SynPop..$Dvmt <- 0
    if (any(IsMetro.)) {
      SynPop..$Dvmt[IsMetro.] <-
        calcAdjAveDvmt(
          SynPop..[IsMetro., ModelVar.],
          DvmtLmModels_,
          "Metro",
          BudgetProp = BudgetProp,
          AnnVmtInflator = AnnVmtInflator,
          TrnstnProp = 1
        )[[1]]
    }
    if (any(!IsMetro.)) {
      SynPop..$Dvmt[!IsMetro.] <-
        calcAdjAveDvmt(
          SynPop..[!IsMetro., ModelVar.],
          DvmtLmModels_,
          "NonMetro",
          BudgetProp = BudgetProp,
          AnnVmtInflator = AnnVmtInflator,
          TrnstnProp = 1
        )[[1]]
    }
    
    #Step 2d: Calculate non-price TDM and light-weight vehicle DVMT adjustment
    #factors
    #=========================================================================
    
    #Calculate the TDM adjustment factor
    #-----------------------------------
    TdmAdjDvmt.Hh <- SynPop..$Dvmt
    ModelVar. <- c("Dvmt", "NumEco", "ImpHh")
    if (any(IsMetro.)) {
      TdmAdjDvmt.Hh[IsMetro.] <-
        adjDvmtEcoImp(SynPop..[IsMetro., ModelVar.],
                      EcoReduction = TdmParm.["EcoReduction"],
                      ImpReduction = TdmParm.["ImpReduction"])
    }
    TdmAdjFactor.Hh <- TdmAdjDvmt.Hh / SynPop..$Dvmt
    TdmAdjFactor.Hh[SynPop..$Dvmt == 0] <- 1
    
    #Calculate the light vehicle adjustment factor
    #---------------------------------------------
    # Predict light vehicle ownership
    LtVehOwn.Hh <- rep(0, nrow(SynPop..))
    SynPop..$LogDen <- log(SynPop..$Htppopdn)
    ModelVar. <-
      c(
        "LogDen",
        "Hhsize",
        "Hhincttl",
        "Age15to19",
        "Age20to29",
        "Age30to54",
        "Age55to64",
        "Age65Plus",
        "VehPerDrvAgePop",
        "DrvAgePop"
      )
    if (any(IsMetro.)) {
      LtVehOwn.Hh[IsMetro.] <-
        predictLightVehicles(
          SynPop..[IsMetro., ModelVar.],
          LtVehOwnModels_ =
            LtVehOwnModels_,
          Type = "Metro",
          TargetProp = LtVehParm_Va..$TargetProp[MetroArea, yr]
        )
    }
    if (any(!IsMetro.)) {
      LtVehOwn.Hh[!IsMetro.] <-
        predictLightVehicles(
          SynPop..[!IsMetro., ModelVar.],
          LtVehOwnModels_ =
            LtVehOwnModels_,
          Type = "NonMetro",
          TargetProp = LtVehParm_Va..$TargetProp["NonMetro", yr]
        )
    }
    SynPop..$LtVehCnt <- LtVehOwn.Hh
    rm(LtVehOwn.Hh, ModelVar.)
    SynPop..$LogDen <- NULL
    
    #Predict light vehicle DVMT
    #--------------------------
    LtVehDvmt.Hh <- SynPop..$Dvmt
    SynPop..$LogDen <- log(SynPop..$Htppopdn)
    SynPop..$LogSize <- log(SynPop..$Hhsize)
    SynPop..$LogDvmt <- log(SynPop..$Dvmt)
    ModelVar. <-
      c(
        "Hhincttl",
        "LogDen",
        "LogSize",
        "Urban",
        "LogDvmt",
        "Dvmt",
        "LtVehCnt",
        "DrvAgePop"
      )
    if (any(IsMetro.)) {
      LtVehDvmt.Hh[IsMetro.] <-
        calcLtVehDvmt(
          SynPop..[IsMetro., ModelVar.],
          AveSovPropModels_,
          Threshold = LtVehParm_Va..$Threshold[MetroArea, yr],
          PropSuitable = LtVehParm_Va..$PropSuitable[MetroArea, yr],
          Sharing = FALSE
        )
    }
    if (any(!IsMetro.)) {
      LtVehDvmt.Hh[!IsMetro.] <-
        calcLtVehDvmt(
          SynPop..[!IsMetro., ModelVar.],
          AveSovPropModels_,
          Threshold = LtVehParm_Va..$Threshold["NonMetro", yr],
          PropSuitable = LtVehParm_Va..$PropSuitable["NonMetro", yr],
          Sharing = FALSE
        )
    }
    #Calculate adjustment factor
    LtVehAdjFactor.Hh <-
      (SynPop..$Dvmt - LtVehDvmt.Hh) / SynPop..$Dvmt
    LtVehAdjFactor.Hh[SynPop..$Dvmt == 0] <- 1
    
    #Calculate overall adjustment factor
    #-----------------------------------
    SynPop..$LtVehAdjFactor <-
      LtVehAdjFactor.Hh  #Save the factor in SynPop..
    SynPop..$TdmAdjFactor <-
      TdmAdjFactor.Hh  #Save the factor in SynPop..
    SynPop..$TdmLtVehAdjFactor <-
      TdmAdjFactor.Hh * LtVehAdjFactor.Hh  #Save the factor in SynPop..
    rm(LtVehDvmt.Hh,
       TdmAdjFactor.Hh,
       LtVehAdjFactor.Hh,
       ModelVar.,
       TdmAdjDvmt.Hh)
    SynPop..$LogSize <- NULL
    SynPop..$LogDvmt <- NULL
    
    #Step 2e: Calculate the 95th percentile and maximum DVMT from the adjusted
    #DVMT
    #=========================================================================
    
    SynPop..$MaxDvmt <- 0
    SynPop..$Dvmt95 <- 0
    if (any(IsMetro.)) {
      MetroMax95th.2d <-
        predictMaxDvmt(SynPop..[IsMetro., c("Dvmt", "MaxDvmt", "Dvmt95")],
                       DvmtLmModels_, "Metro")
      SynPop..$MaxDvmt[IsMetro.] <- MetroMax95th.2d[, 1]
      SynPop..$Dvmt95[IsMetro.] <- MetroMax95th.2d[, 2]
      rm(MetroMax95th.2d)
    }
    if (any(!IsMetro.)) {
      NonMetroMax95th.2d <-
        predictMaxDvmt(SynPop..[!IsMetro., c("Dvmt", "MaxDvmt", "Dvmt95")],
                       DvmtLmModels_, "NonMetro")
      SynPop..$MaxDvmt[!IsMetro.] <- NonMetroMax95th.2d[, 1]
      SynPop..$Dvmt95[!IsMetro.] <- NonMetroMax95th.2d[, 2]
      rm(NonMetroMax95th.2d)
    }
    gc()
    
    
    #Step 2f: Calculate Walk, Bike, & Transit Trips
    #==============================================
    #Set up walk, bike and transit fields
    SynPop..$AveWalkTrips <- numeric(nrow(SynPop..))
    SynPop..$AveBikeTrips <- numeric(nrow(SynPop..))
    SynPop..$AveTransitTrips <- numeric(nrow(SynPop..))
    
    #Calculate for metropolitan households
    if (any(IsMetro.)) {
      #Identify data fields to use
      ModelVar. <-
        c(
          "Age0to14",
          "Age15to19",
          "Age20to29",
          "Age30to54",
          "Age55to64",
          "Age65Plus",
          "Hhsize",
          "Hhincttl",
          "Htppopdn",
          "Tranmilescap",
          "Urban",
          "Dvmt",
          "Hhvehcnt"
        )
      
      #Calculate the alternative mode trips and add to household dataset
      AltModeTrips_ <-
        calcAltModeTrips(SynPop..[IsMetro., ModelVar.],
                         AltModeModels_, "Metro")
      SynPop..$AveWalkTrips[IsMetro.] <- AltModeTrips_$Walk
      SynPop..$AveBikeTrips[IsMetro.] <- AltModeTrips_$Bike
      SynPop..$AveTransitTrips[IsMetro.] <- AltModeTrips_$Transit
      rm(ModelVar., AltModeTrips_)
    }
    
    #Calculate for non-metropolitan households
    if (any(!IsMetro.)) {
      #Identify data fields to use
      ModelVar. <-
        c(
          "Age0to14",
          "Age15to19",
          "Age20to29",
          "Age30to54",
          "Age55to64",
          "Age65Plus",
          "Hhsize",
          "Hhincttl",
          "Htppopdn",
          "Urban",
          "Dvmt",
          "Hhvehcnt"
        )
      
      #Calculate the alternative mode trips and add to household dataset
      AltModeTrips_ <-
        calcAltModeTrips(SynPop..[!IsMetro., ModelVar.],
                         AltModeModels_, "NonMetro")
      SynPop..$AveWalkTrips[!IsMetro.] <- AltModeTrips_$Walk
      SynPop..$AveBikeTrips[!IsMetro.] <- AltModeTrips_$Bike
      SynPop..$AveTransitTrips[!IsMetro.] <- AltModeTrips_$Transit
      rm(ModelVar., AltModeTrips_)
    }
    
    
    #Step 2g: Apply parking model to identify parkers and calculate daily
    #parking costs
    #=====================================================================
    
    #Calculate parking costs for households that live in metropolitan areas
    SynPop..$DailyPkgCost <- 0
    SynPop..$CashOutIncAdj <- 0
    ModelVar. <- c("DrvAgePop", "Houseid", "Dvmt", "Hhvehcnt")
    if (any(IsMetro.)) {
      Parkers_ <- idPayingParkers(
        SynPop..[IsMetro., ModelVar.],
        PropWrkPkg = PkgParm_Va..$PropWrkPkg[MetroArea, yr],
        PropWrkChrgd = PkgParm_Va..$PropWrkChrgd[MetroArea, yr],
        PropCashOut = PkgParm_Va..$PropCashOut[MetroArea, yr],
        PropOthChrgd = PkgParm_Va..$PropOthChrgd[MetroArea, yr],
        PkgCost = PkgParm_Va..$PkgCost[MetroArea, yr],
        PropWrkTrav = 0.22,
        WrkDaysPerYear = 260
      )
      PkgCosts_ <-
        calcParkCostAdj(SynPop..[IsMetro., ModelVar.], Parkers_)
      SynPop..$DailyPkgCost[IsMetro.] <- PkgCosts_$DailyPkgCost
      SynPop..$CashOutIncAdj[IsMetro.] <- PkgCosts_$CashOutIncAdj
      rm(Parkers_, PkgCosts_)
    }
    rm(ModelVar.)
    gc()
    
    #Remove variables from SynPop.. not needed
    #-----------------------------------------
    SynPop..$PowPerCapInc <- NULL

    #Save results
    #------------
    Filename <- paste(OutputYearDir, "/", co, ".RData", sep = "")
    save(SynPop.., file = Filename, compress = TRUE)
    rm(SynPop..)
    gc()
    
    #End loop through counties
    gc()
  }
  
  #Report end of procedure
  print(StartTime)
  print(Sys.time())
  
  
  #==================================================
  #STEP 3: SIMULATE HOUSEHOLD VEHICLE CHARACTERISTICS
  #==================================================
  
  #Log start of procedure
  StartTime <- Sys.time()
  print("Simulation of household vehicle characteristics")
  
  #Iterate through counties
  #========================
  
  for (co in Co) {
    print(co)
    
    #Load county file
    Filename <- paste(OutputYearDir, "/", co, ".RData", sep = "")
    SynPop.. <- assignLoad(Filename)
    
    #Identify metropolitan area
    MetroArea <- CountyGroups..[co, "Msa"]
    IsMetro. <- SynPop..$DevType == "Metropolitan"
    
    
    #Step 3a: Calculate vehicle types, ages, initial fuel economy, and assign
    #vehicle DVMT
    #=========================================================================
    
    #Predict light truck ownership and vehicle ages
    #----------------------------------------------
    #Apply vehicle type model
    ModelVar. <-
      c("Hhincttl", "Htppopdn", "Urban", "Hhvehcnt", "Hhsize")
    SynPop..$VehType <-
      predictLtTruckOwn(SynPop..[, ModelVar.], Model_ = LtTruckModels_,
                        TruckProp = LtTruckProp.CoYr[co, yr])
    rm(ModelVar.)
    #Apply vehicle age model
    ModelVar. <- c("IncGrp", "Hhvehcnt", "VehType")
    VehTypeAgeResults_ <-
      calcVehicleAges(SynPop..[, ModelVar.], VProp_ = VehProp_,
                      AdjRatio = AgeAdj.YrTy[yr, ])
    rm(ModelVar.)
    #Add type and age model results to the TestHh..
    SynPop..$VehType[SynPop..$Hhvehcnt == 0] <- NA
    SynPop..$VehAge <- VehTypeAgeResults_$VehAge
    SynPop..$VehAge[SynPop..$Hhvehcnt == 0] <- NA
    rm(VehTypeAgeResults_)
    gc()
    
    #Assign initial fuel economy and DVMT to vehicles
    #------------------------------------------------
    #Assign fuel economy to vehicles
    HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
    SynPop..$VehMpg <- NA
    ModelVar. <- c("VehType", "VehAge", "Hhvehcnt")
    SynPop..$VehMpg[HasVeh.Hh] <-
      assignFuelEconomy(SynPop..[HasVeh.Hh, ModelVar.],
                        AutoLtTrkMpg..Yr, CurrYear =
                          yr)
    rm(ModelVar.)
    #Assign vehicle mileage proportions to household vehicles
    SynPop..$DvmtProp <- NA
    ModelVar. <- c("Hhvehcnt", "Houseid")
    SynPop..$DvmtProp[HasVeh.Hh] <-
      apportionDvmt(SynPop..[HasVeh.Hh,],
                    DP_ = DvmtProp_)
    rm(ModelVar.)
    #Assign vehicle mileage to household vehicles
    SynPop..$VehDvmt <-
      calcVehDvmt(SynPop..$Dvmt, SynPop..$DvmtProp)
    gc()
    
    #Step 3b: Identify HEVs & PHEVs
    #==============================
    
    #Apply HEV/PHEV model
    HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
    ModelVar. <-
      c(
        "Houseid",
        "Hhvehcnt",
        "VehType",
        "VehAge",
        "VehDvmt",
        "Carshare",
        "DevType",
        "Hhincttl",
        "Htppopdn",
        "Hhsize",
        "Age0to14",
        "Age65Plus",
        "Tranmilescap",
        "Urban",
        "VehMpg"
      )
    PhevResults_ <- assignPhev(
      SynPop..[HasVeh.Hh, ModelVar.],
      PhevRangeProp..Yr = PhevRangeProp..Yr,
      CurrYear = yr,
      PhevPropModel_ = PhevMilePropModel_,
      HevMpgProp..Yr = HevMpgProp..Yr,
      OptimProp = OptimProp.Yr[yr]
    )
    rm(ModelVar.)
    
    #Update SynPop.. data
    SynPop..$VehDvmt[HasVeh.Hh] <- PhevResults_$VehDvmt_
    SynPop..$DvmtProp[HasVeh.Hh] <- PhevResults_$DvmtProp_
    SynPop..$EvVehDvmt <- NA
    SynPop..$EvVehDvmt[HasVeh.Hh] <- PhevResults_$EvVehDvmt_
    SynPop..$HcVehDvmt <- NA
    SynPop..$HcVehDvmt[HasVeh.Hh] <- PhevResults_$HcVehDvmt_
    SynPop..$VehMpg[HasVeh.Hh] <- PhevResults_$VehMpg_
    SynPop..$VehMpkwh <- NA
    SynPop..$VehMpkwh[HasVeh.Hh] <- PhevResults_$VehMpkwh_
    SynPop..$Powertrain <- NA
    SynPop..$Powertrain[HasVeh.Hh] <- PhevResults_$Powertrain_
    rm(PhevResults_, HasVeh.Hh)
    gc()
    
    #Step 3c: Identify EVs
    #=====================
    
    #Apply EV model
    ModelVar. <-
      c(
        "Houseid",
        "Hhvehcnt",
        "VehType",
        "VehAge",
        "VehDvmt",
        "Dvmt95",
        "DvmtProp",
        "Powertrain",
        "VehMpg",
        "VehMpkwh",
        "EvVehDvmt",
        "HcVehDvmt"
      )
    HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
    HasDvmt.Hh <- SynPop..$Dvmt > 0
    EvResults_ <-
      assignEv(SynPop..[HasVeh.Hh & HasDvmt.Hh, ModelVar.],
               EvRangeProp..Yr = EvRangeProp..Yr,
               CurrYear = yr)
    SynPop..$EvVehDvmt[HasVeh.Hh & HasDvmt.Hh] <- EvResults_$EvVehDvmt_
    SynPop..$HcVehDvmt[HasVeh.Hh & HasDvmt.Hh] <- EvResults_$HcVehDvmt_
    SynPop..$VehMpg[HasVeh.Hh & HasDvmt.Hh] <- EvResults_$VehMpg_
    SynPop..$VehMpkwh[HasVeh.Hh & HasDvmt.Hh] <- EvResults_$VehMpkwh_
    SynPop..$Powertrain[HasVeh.Hh & HasDvmt.Hh] <- EvResults_$Powertrain_
    rm(EvResults_, HasVeh.Hh, HasDvmt.Hh, ModelVar.)
    gc()
    
    #Step 3d: Calculate vehicle depreciation expenses
    #================================================
    
    SynPop..$DepExp <- 0
    HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
    ModelVar. <- c("Houseid", "Hhvehcnt", "VehType", "VehAge")
    SynPop..$DepExp[HasVeh.Hh] <-
      calcVehDepreciationExp(SynPop..[HasVeh.Hh, ModelVar.])
    rm(HasVeh.Hh, ModelVar.)
    gc()
    
    #Step 3e: Assign PAYD Insurance
    #==============================
    
    ModelVar. <-
      c(
        "Houseid",
        "Age0to14",
        "Age15to19",
        "Age20to29",
        "Age30to54",
        "Age55to64",
        "Age65Plus",
        "Dvmt",
        "Hhvehcnt",
        "Hhincttl",
        "VehType",
        "VehAge"
      )
    PaydWeights.Hh <-
      estPaydWeights(Data.. = SynPop..[ModelVar.], yr = yr)
    SynPop..$Payd <-
      selectFromWeights(PaydWeights.Hh, PropToSelect = Payd..Yr[yr, "Proportion"])
    rm(ModelVar., PaydWeights.Hh)
    gc()
    
    #Save county results
    #===================
    
    Filename <- paste(OutputYearDir, "/", co, ".RData", sep = "")
    save(SynPop.., file = Filename, compress = TRUE)
    rm(SynPop..)
    gc()
    gc()
    
    # End loop through counties
    gc()
  }
  
  #Report end of procedure
  print(StartTime)
  print(Sys.time())
  
  
  #=====================================================
  #STEP 4: EQUILIBRATE DVMT, COSTS, REVENUES, CONGESTION
  #=====================================================
  
  #Calculate and save unit emissions for fuel and electricity for year
  #--------------------------------------------------------------------
  #These are calculated and saved for the purpose of computing output measures
  #Calculate average fuel CO2e per gallon
  AveFuelCo2e. <- calcAveFuelCo2e(
    yr,
    Fuels..Yr = AutoLtTrkFuels..Yr,
    Co2..Yr = FuelCo2..Yr,
    MjPerGallon = 121,
    OutputType = "MetricTons"
  )
  Filename <- paste(OutputYearDir, "AveFuelCo2e.RData", sep = "/")
  save(AveFuelCo2e., file = Filename, compress = TRUE)
  rm(Filename)
  # Calculate average electricity CO2e per Kwh
  AveElectricCo2e.Co <-
    calcAveElectricCo2e(yr, Co2.CoYr = PowerCo2.CoYr, OutputType = "MetricTons")
  Filename <-
    paste(OutputYearDir, "AveElectricCo2e.Co.RData", sep = "/")
  save(AveElectricCo2e.Co, file = Filename, compress = TRUE)
  rm(Filename)
  
  #Loop to equilibrate DVMT, travel costs, revenues and congestion
  #---------------------------------------------------------------
  
  #Create list to store DVMT, cost totals
  Dvmt.CoDt <- Pop.CoDt * 0
  Va <-
    c(
      "Dvmt",
      "AdjDvmt",
      "CongPrice",
      "FuelCost",
      "PowerCost",
      "RoadUseTax",
      "CarbonTax",
      "AddedExtTax",
      "PaydCost",
      "TotExtCost",
      "HhTotCost",
      "FutrCostPerMi",
      "VehOwnExp",
      "TotRoadCost"
    )
  CostSummary.CoVa <-
    array(0,
          dim = c(length(Co), length(Va)),
          dimnames = list(Co, Va))
  AveCongTaxPerMi.Ma <- numeric(length(Ma))
  names(AveCongTaxPerMi.Ma) <- Ma
  ExtraModCost <- 0
  
  #Loop 4 times if equilibrating cost and revenues, 2 time otherwise
  if (CalcVmtSurcharge) {
    It <- 1:4
  } else {
    It <- 1:2
  }
  VmtSurcharge.It <- numeric(length(It))
  rm(Va)
  
  #Begin iteration loop
  for (it in It) {
    
    #Log start of iteration
    StartTime <- Sys.time()
    print(paste(
      "Iteration",
      it,
      "Calculate emissions and cost and adjust DVMT"
    ))
    
    #Steps 4a & 4b: calculate energy consumption, CO2e, production, household
    #costs, and adjust DVMT from costs
    #========================================================================
    for (co in Co) {
      
      #Report county
      print(co)
      
      # Load county file
      Filename <-
        paste(OutputYearDir, "/", co, ".RData", sep = "")
      SynPop.. <- assignLoad(Filename)
      
      # Identify metropolitan area
      MetroArea <- CountyGroups..[co, "Msa"]
      IsMetro. <- SynPop..$DevType == "Metropolitan"
      
      
      #Step 4a: Calculate fuel & electricity consumption, CO2e production, &
      #household costs
      #======================================================================
      
      #Calculate fuel & electricity consumption and CO2e production
      #------------------------------------------------------------
      ModelVar. <-
        c("Hhvehcnt",
          "HcVehDvmt",
          "VehMpg",
          "VehType",
          "EvVehDvmt",
          "VehMpkwh",
          "Dvmt")
      FuelElecCo2e_ <-
        calcVehFuelElecCo2(
          SynPop..[, ModelVar.],
          AveFuelCo2e. = AveFuelCo2e.,
          AveElectricCo2e = AveElectricCo2e.Co[co],
          CsharEffFactor = 1
        )
      SynPop..$FuelGallons <- FuelElecCo2e_$FuelGallons
      SynPop..$FuelCo2e <- FuelElecCo2e_$FuelCo2e
      SynPop..$ElecKwh <- FuelElecCo2e_$ElecKwh
      SynPop..$ElecCo2e <- FuelElecCo2e_$ElecCo2e
      rm(FuelElecCo2e_)
      rm(ModelVar.)
      gc()
      
      #Calculate household travel costs
      #--------------------------------
      #Identify congestion price for metropolitan area if any
      if (!is.na(MetroArea)) {
        CongPrice <- AveCongTaxPerMi.Ma[MetroArea]
      } else {
        CongPrice <- 0
      }
      #Identify the VmtSurcharge calculated to balance costs and revenues
      #Is 0 on first iteration and if no equilibration of costs and revenues
      if (it == 1 | !CalcVmtSurcharge) {
        VmtSurcharge <- 0
      } else {
        VmtSurcharge <-
          VmtSurcharge.It[it - 1]   #Is value calculated in previous iteration
      }
      #Calculate household costs
      ModelVar. <-
        c(
          "Dvmt",
          "FuelGallons",
          "FuelCo2e",
          "ElecCo2e",
          "ElecKwh",
          "DevType",
          "Payd",
          "DailyPkgCost",
          "Hhvehcnt",
          "DepExp",
          "Hhincttl", 
          "EvVehDvmt", 
          "HcVehDvmt"
        )
      Costs_ <- calcCosts(
        Data.. = SynPop..[, ModelVar.],
        Costs. = Costs.YrCs[yr,],
        PaydRate = Payd..Yr[yr, "RatePerMile"],
        CongPrice = CongPrice,
        VmtSurcharge = VmtSurcharge,
        ExtraModCost = ExtraModCost
      )
      rm(VmtSurcharge)
      #Add selected cost data to household records
      SynPop..$FutrCostPerMi <- Costs_$FutrCostPerMi
      SynPop..$TotExtCost <- Costs_$TotExtCost
      SynPop..$HhTotCost <- Costs_$HhTotCost
      SynPop..$VehOwnExp <- Costs_$VehOwnExp
      #Add sums to DVMT and cost summary
      Dvmt.CoDt[co, ] <-
        tapply(SynPop..$Dvmt, SynPop..$DevType, sum)[Dt]
      Dvmt.CoDt[is.na(Dvmt.CoDt)] <- 0
      CostSummary.CoVa[co, "Dvmt"] <- sum(Dvmt.CoDt[co, ])
      CostSummary.CoVa[co, "FuelCost"] <- sum(Costs_$FuelCost)
      CostSummary.CoVa[co, "PowerCost"] <- sum(Costs_$PowerCost)
      CostSummary.CoVa[co, "RoadUseTax"] <- sum(Costs_$RoadUseTax)
      CostSummary.CoVa[co, "CarbonTax"] <- sum(Costs_$CarbonTax)
      CostSummary.CoVa[co, "AddedExtTax"] <-
        sum(Costs_$AddedExtTax)
      CostSummary.CoVa[co, "PaydCost"] <- sum(Costs_$PaydCost)
      CostSummary.CoVa[co, "TotExtCost"] <- sum(Costs_$TotExtCost)
      CostSummary.CoVa[co, "HhTotCost"] <- sum(Costs_$HhTotCost)
      CostSummary.CoVa[co, "FutrCostPerMi"] <-
        sum(Costs_$FutrCostPerMi)
      CostSummary.CoVa[co, "VehOwnExp"] <- sum(Costs_$VehOwnExp)
      CostSummary.CoVa[co, "TotRoadCost"] <-
        sum(Costs_$TotRoadCost)
      rm(Costs_, ModelVar.)
      gc()
      
      #Step 4b: Calculate DVMT with new costs and reallocate to vehicles
      #=================================================================
      
      #Recalculate DVMT
      #----------------
      PrevDvmt.Hh <- SynPop..$Dvmt
      ModelVar. <-
        c(
          "Hhincttl",
          "CashOutIncAdj",
          "Htppopdn",
          "Hhvehcnt",
          "Tranmilescap",
          "Fwylnmicap",
          "DrvAgePop",
          "Hhsize",
          "Age0to14",
          "Age15to19",
          "Age20to29",
          "Age30to54",
          "Age55to64",
          "Age65Plus",
          "Urban",
          "BaseCostPerMi",
          "FutrCostPerMi"
        )
      if (any(IsMetro.)) {
        SynPop..$Dvmt[IsMetro.] <-
          calcAdjAveDvmt(
            SynPop..[IsMetro., ModelVar.],
            DvmtLmModels_,
            "Metro",
            BudgetProp = BudgetProp,
            AnnVmtInflator = AnnVmtInflator,
            TrnstnProp = 1
          )[[1]]
      }
      if (any(!IsMetro.)) {
        SynPop..$Dvmt[!IsMetro.] <-
          calcAdjAveDvmt(
            SynPop..[!IsMetro., ModelVar.],
            DvmtLmModels_,
            "NonMetro",
            BudgetProp = BudgetProp,
            AnnVmtInflator = AnnVmtInflator,
            TrnstnProp = 1
          )[[1]]
      }
      
      #Calculate light-weight vehicle DVMT and adjust household DVMT
      #-------------------------------------------------------------
      SynPop..$LtVehDvmt <-
        SynPop..$Dvmt * (1 - SynPop..$LtVehAdjFactor)
      SynPop..$Dvmt <- SynPop..$Dvmt * SynPop..$TdmLtVehAdjFactor
      
      #Calculate 95th percentile and maximum DVMT
      #------------------------------------------
      SynPop..$MaxDvmt <- 0
      SynPop..$Dvmt95 <- 0
      if (any(IsMetro.)) {
        MetroMax95th.2d <-
          predictMaxDvmt(SynPop..[IsMetro., c("Dvmt", "MaxDvmt", "Dvmt95")],
                         DvmtLmModels_, "Metro")
        SynPop..$MaxDvmt[IsMetro.] <- MetroMax95th.2d[, 1]
        SynPop..$Dvmt95[IsMetro.] <- MetroMax95th.2d[, 2]
        rm(MetroMax95th.2d)
      }
      if (any(!IsMetro.)) {
        NonMetroMax95th.2d <-
          predictMaxDvmt(SynPop..[!IsMetro., c("Dvmt", "MaxDvmt", "Dvmt95")],
                         DvmtLmModels_, "NonMetro")
        SynPop..$MaxDvmt[!IsMetro.] <- NonMetroMax95th.2d[, 1]
        SynPop..$Dvmt95[!IsMetro.] <- NonMetroMax95th.2d[, 2]
        rm(NonMetroMax95th.2d)
      }
      gc()
      
      #Split adjusted DVMT among vehicles
      #----------------------------------
      DvmtAdjFactor.Hh <- SynPop..$Dvmt / PrevDvmt.Hh
      HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
      ModelVar. <- c("VehDvmt", "HcVehDvmt", "EvVehDvmt")
      AdjDvmt_ <-
        allocateAdjDvmt(SynPop..[HasVeh.Hh, ModelVar.], 
                        DvmtAdjFactor.Hh[HasVeh.Hh])
      SynPop..$VehDvmt[HasVeh.Hh] <- AdjDvmt_$VehDvmt
      SynPop..$EvVehDvmt[HasVeh.Hh] <- AdjDvmt_$EvVehDvmt
      SynPop..$HcVehDvmt[HasVeh.Hh] <- AdjDvmt_$HcVehDvmt
      rm(DvmtAdjFactor.Hh, HasVeh.Hh, ModelVar., AdjDvmt_)
      gc()
      
      #Tabulate DVMT
      #-------------
      Dvmt.CoDt[co,] <-
        tapply(SynPop..$Dvmt, SynPop..$DevType, sum, na.rm = TRUE)[Dt]
      Dvmt.CoDt[is.na(Dvmt.CoDt)] <- 0
      CostSummary.CoVa[co, "AdjDvmt"] <-
        sum(SynPop..$Dvmt, na.rm = TRUE)
      
      #Save the household dataset
      #--------------------------
      Filename <- paste(OutputYearDir, "/", co, ".RData", sep = "")
      save(SynPop.., file = Filename, compress = TRUE)
      rm(SynPop..)
      gc()
      gc()
      
      #End iteration through counties
    }
    
    #Save the tabulations of Dvmt and Costs
    #--------------------------------------
    Filename <-
      paste(OutputYearDir, "/", "Dvmt.CoDt", ".RData", sep = "")
    save(Dvmt.CoDt, file = Filename)
    rm(Filename)
    Filename <-
      paste(OutputYearDir, "/", "CostSummary.CoVa", ".RData", sep = "")
    save(CostSummary.CoVa, file = Filename)
    rm(Filename)
    EndTime <- Sys.time()
    print(EndTime - StartTime)
    
    #Step 4c: Calculate Effects of Congestion
    #========================================
    
    local({
      #Calculate truck VMT by metropolitan area
      #----------------------------------------
      #Calculate growth in total percapita income from base year
      #Calculate change in income
      BaseInc <- sum(BaseInc.CoDt)
      FutrInc <- sum(Inc.CoDt)
      IncGrowth <- FutrInc / BaseInc
      #Calculate truck DVMT
      TruckDvmt <-
        IncGrowth * TruckVmtGrowthMultiplier * BaseTruckVmt / 365
      #Allocate truck VMT to metropolitan areas for later congestion calculation
      TruckDvmt.Ma <- TruckDvmt * MpoBaseDvmtParm..Ma$PropTruckDvmt
      
      #Calculate bus DVMT by metropolitan area
      #---------------------------------------
      #Calculate bus DVMT
      BusDvmt.Ma <- BusRevMi.Ma * TranAdjFactor / 365
      
      #Calculate light vehicle DVMT by metropolitan area
      #-------------------------------------------------
      #Sum household light vehicle DVMT by metropolitan area
      CoToMa. <- CountyGroups..$Msa
      names(CoToMa.) <- rownames(CountyGroups..)
      CoToMa. <- CoToMa.[!is.na(CoToMa.)]
      HhDvmt.Ma <-
        tapply(Dvmt.CoDt[names(CoToMa.), "Metropolitan"], CoToMa., sum)[Ma]
      #Calculate commercial service vehicle DVMT
      if (CommVehDvmtMethod == "HouseholdIncome") {
        CommVehDvmt.CoDt <- 
          calcCommVehTravelFromHhIncome(Dvmt.CoDt, Inc.CoDt)$CommVehDvmt.CoDt
      }
      if (CommVehDvmtMethod == "HouseholdDvmt") {
        CommVehDvmt.CoDt <- 
          calcCommVehTravelFromHhDvmt(Dvmt.CoDt)$CommVehDvmt.CoDt
      }
      CommVehDvmt.Ma <-
        tapply(CommVehDvmt.CoDt[names(CoToMa.), "Metropolitan"], 
               CoToMa., sum)[Ma]
      # Calculate total light vehicle DVMT that is on metropolitan area roadways
      LtVehDvmt.Ma <-
        (HhDvmt.Ma + CommVehDvmt.Ma) * LtVehDvmtFactor.Ma
      # Clean up
      rm(CoToMa.)
      
      #Calculate total DVMT by metropolitan area and type
      #--------------------------------------------------
      Dvmt.MaTy <-
        cbind(LtVeh = LtVehDvmt.Ma,
              Truck = TruckDvmt.Ma,
              Bus = BusDvmt.Ma)
      
      #Sum population and base year population by metropolitan area
      #------------------------------------------------------------
      CoToMa. <- CountyGroups..$Msa
      names(CoToMa.) <- rownames(CountyGroups..)
      CoToMa. <- CoToMa.[!is.na(CoToMa.)]
      Pop.Ma <-
        tapply(Pop.CoDt[names(CoToMa.), "Metropolitan"], CoToMa., sum)[Ma]
      BasePop.Ma <-
        tapply(BasePop.CoDt[names(CoToMa.), "Metropolitan"], CoToMa., sum)[Ma]
      rm(CoToMa.)
      
      #Initialize arrays to store results
      #----------------------------------
      Ty <- Abbr_$Ty
      MpgMpkwhAdj.MaPt <-
        array(0,
              dim = c(length(Ma), length(Pt)),
              dimnames = list(Ma, Pt))
      VehHr.MaTy <-
        array(0,
              dim = c(length(Ma), length(Ty)),
              dimnames = list(Ma, Ty))
      AveSpeed.MaTy <-
        array(0,
              dim = c(length(Ma), length(Ty)),
              dimnames = list(Ma, Ty))
      FfVehHr.MaTy <-
        array(0,
              dim = c(length(Ma), length(Ty)),
              dimnames = list(Ma, Ty))
      DelayVehHr.MaTy <-
        array(0,
              dim = c(length(Ma), length(Ty)),
              dimnames = list(Ma, Ty))
      CongVmt.ClFcTyMa <-
        array(
          0,
          dim = c(length(Cl), length(Fc), length(Ty), length(Ma)),
          dimnames = list(Cl, Fc, Ty, Ma)
        )
      AveCongTaxPerMi.Ma <- numeric(length(Ma))
      names(AveCongTaxPerMi.Ma) <- Ma
      
      #Calculate effects of congestion on speed and emissions
      #------------------------------------------------------
      for (ma in Ma) {
        #Make an array of congestion prices
        CongPrice.ClFc <-
          array(0,
                dim = c(length(Cl), length(Fc)),
                dimnames = list(Cl, Fc))
        CongPrice.ClFc["Sev", "Fwy"] <-
          CongPriceParm_Va..$FwySev[ma, yr]
        CongPrice.ClFc["Ext", "Fwy"] <-
          CongPriceParm_Va..$FwyExt[ma, yr]
        CongPrice.ClFc["Sev", "Art"] <-
          CongPriceParm_Va..$ArtSev[ma, yr]
        CongPrice.ClFc["Ext", "Art"] <-
          CongPriceParm_Va..$ArtExt[ma, yr]
        
        #Calculate congestion results
        CongResults_ <- calcCongestion(
          CongModel_ = CongModel_,
          Dvmt.Ty = Dvmt.MaTy[ma,],
          PerCapFwy = FwyLnMiCap.Ma[ma],
          PerCapArt = ArtLnMiCap.Ma[ma],
          Pop = Pop.Ma[ma],
          BasePop = BasePop.Ma[ma],
          FwyArtProp = MpoBaseDvmtParm..Ma[ma, "FwyArtProp"],
          BusVmtSplit.Fc = TruckBusFcDvmtSplit_Va..$BusVmt[ma,],
          TruckVmtSplit.Fc = TruckBusFcDvmtSplit_Va..$TruckVmt[ma,],
          OpsDeployParm_Va.MaYr = OpsDeployParm_Va.MaYr,
          SmoothEcoDriveParm_Va.. = SmoothEcoDriveParm_Va..,
          OtherOps_Yr.LvTy = OtherOps_Yr.LvTy,
          CongPrice.ClFc = CongPrice.ClFc,
          CongEfficiency.YrPt = CongEfficiency.YrPt,
          ValueOfTime = ValueOfTime,
          ma = ma
        )
        
        #Insert results in arrays
        MpgMpkwhAdj.MaPt[ma,] <- CongResults_$MpgMpkwhAdj.Pt
        VehHr.MaTy[ma,] <- CongResults_$VehHr.Ty
        AveSpeed.MaTy[ma,] <- CongResults_$AveSpeed.Ty
        FfVehHr.MaTy[ma,] <- CongResults_$FfVehHr.Ty
        DelayVehHr.MaTy[ma,] <- CongResults_$DelayVehHr.Ty
        CongVmt.ClFcTyMa[, , "LtVeh" , ma] <-
          CongResults_$LtVehDvmt.ClFc
        CongVmt.ClFcTyMa[, , "Truck" , ma] <-
          CongResults_$TruckDvmt.ClFc
        CongVmt.ClFcTyMa[, , "Bus" , ma] <-
          CongResults_$BusDvmt.ClFc
        AveCongTaxPerMi.Ma[ma] <-
          CongResults_$AveCongTaxPerMi
        
        # Clean up
        rm(CongResults_)
        
      }
      
      #Save the results
      #----------------
      Filename <-
        paste(OutputYearDir, "/", "CommVehDvmt.CoDt", ".RData", sep = "")
      save(CommVehDvmt.CoDt, file = Filename)
      Filename <-
        paste(OutputYearDir, "/", "CommVehDvmt.Ma", ".RData", sep = "")
      save(CommVehDvmt.Ma, file = Filename)
      Filename <-
        paste(OutputYearDir, "/", "Dvmt.MaTy", ".RData", sep = "")
      save(Dvmt.MaTy, file = Filename)
      Filename <-
        paste(OutputYearDir, "/", "MpgMpkwhAdj.MaPt", ".RData", sep = "")
      save(MpgMpkwhAdj.MaPt, file = Filename)
      Filename <-
        paste(OutputYearDir, "/", "VehHr.MaTy", ".RData", sep = "")
      save(VehHr.MaTy, file = Filename)
      Filename <-
        paste(OutputYearDir, "/", "AveSpeed.MaTy", ".RData", sep = "")
      save(AveSpeed.MaTy, file = Filename)
      Filename <-
        paste(OutputYearDir, "/", "FfVehHr.MaTy", ".RData", sep = "")
      save(FfVehHr.MaTy, file = Filename)
      Filename <-
        paste(OutputYearDir, "/", "DelayVehHr.MaTy", ".RData", sep = "")
      save(DelayVehHr.MaTy, file = Filename)
      Filename <-
        paste(OutputYearDir, "/", "CongVmt.ClFcTyMa", ".RData", sep = "")
      save(CongVmt.ClFcTyMa, file = Filename)
      Filename <-
        paste(OutputYearDir, "/", "AveCongTaxPerMi.Ma", ".RData", sep = "")
      save(AveCongTaxPerMi.Ma, file = Filename)
      
      #Return results to enclosing environment
      #---------------------------------------
      TruckDvmt <<- TruckDvmt
      TruckDvmt.Ma <<- TruckDvmt.Ma
      BusDvmt.Ma <<- BusDvmt.Ma
      MpgMpkwhAdj.MaPt <<- MpgMpkwhAdj.MaPt
      AveCongTaxPerMi.Ma <<- AveCongTaxPerMi.Ma
      LtVehDvmt.Ma <<- LtVehDvmt.Ma
      HhRoadDvmt.Ma <<- HhDvmt.Ma * LtVehDvmtFactor.Ma
      Dvmt.MaTy <<- Dvmt.MaTy
      
    })
    
    
    #Step 4d: Calculate Commercial Fuel Consumption, Emissions, Costs
    #================================================================
    
    #Store all the commercial service vehicle calculations in a list
    CommServ_ <- list()
    #Calculate CS DVMT by county and development type
    if (CommVehDvmtMethod == "HouseholdIncome") {
      CommServ_ <- c(
        CommServ_,
        calcCommVehTravelFromHhIncome(Dvmt.CoDt, Inc.CoDt)
      )
    }
    if (CommVehDvmtMethod == "HouseholdDvmt") {
      CommServ_ <- c(CommServ_,
                     calcCommVehTravelFromHhDvmt(Dvmt.CoDt))
    }
    #Calculate DVMT by vehicle type a vehicle age characteristics
    CommServ_ <-
      c(CommServ_, with(
        CommServ_,
        calcCommVehTypeAgeProp(
          CommVehDvmt.CoDt,
          yr,
          CommServiceLtTruckProp.Yr,
          VehProp_$AgCumProp.AgTy,
          AgeAdj.YrTy
        )
      ))
    #Calculate the powertrain proportions, MPG and MPKWH
    CommServ_ <-
      c(CommServ_,
        with(
          CommServ_,
          calcCommVehPowertrainMpgMpkwh(
            CommServicePtProp..Yr,
            CommServAutoAgProp.Ag,
            CommServLtTruckAgProp.Ag,
            AutoLtTrkMpg..Yr,
            HevMpgProp..Yr,
            EvRangeProp..Yr,
            MpgMpkwhAdj.MaPt
          )
        ))
    #Calculate DVMT powered by hydrocarbons vs. electricity
    CommServ_ <-
      c(CommServ_, with(
        CommServ_,
        calcCommVehHcEvDvmt(
          CommServAutoDvmt.CoDt,
          CommServAutoProp.Pt,
          CommServLtTruckDvmt.CoDt,
          CommServLtTruckProp.Pt
        )
      ))
    #Calculate daily electricity and power consumption
    CommServ_ <-
      c(CommServ_, with(
        CommServ_,
        calcFuelElectricityUse(
          CommServAveAutoMpg.CoDt,
          CommServAveLtTruckMpg.CoDt,
          CommServAveAutoMpkwh.CoDt,
          CommServAveLtTruckMpkwh.CoDt,
          CommServAutoHcDvmt.CoDt,
          CommServAutoEvDvmt.CoDt,
          CommServLtTruckHcDvmt.CoDt,
          CommServLtTruckEvDvmt.CoDt
        )
      ))
    #Calculate average emissions per gallon of fuel consumed
    CommServ_ <- c(CommServ_,
                   calcAveFuelCarbonIntensity(yr, CommServiceFuels..Yr, 
                                              FuelCo2..Yr))
    #Calculate emissions
    CommServ_ <-
      c(CommServ_, with(
        CommServ_,
        calcCommVehEmissions(
          yr,
          MjPerGallon,
          PowerCo2.CoYr,
          AveAutoFuelCo2,
          AveLtTruckFuelCo2,
          CommServAutoFuel.CoDt,
          CommServLtTruckFuel.CoDt,
          CommServAutoPower.CoDt,
          CommServLtTruckPower.CoDt
        )
      ))
    #Calculate commercial vehicle costs
    CommServ_ <- c(CommServ_, with(
      CommServ_,
      calcCommVehCosts(
        yr,
        Costs.YrCs,
        CountyGroups..,
        it,
        CommVehDvmt.CoDt,
        CommServAutoDvmt.CoDt,
        CommServLtTruckDvmt.CoDt,
        CommServAutoFuel.CoDt,
        CommServLtTruckFuel.CoDt,
        CommServAutoPower.CoDt,
        CommServLtTruckPower.CoDt,
        CommServAutoHcCo2e.CoDt,
        CommServLtTruckHcCo2e.CoDt,
        CommServAutoEvCo2e.CoDt,
        CommServLtTruckEvCo2e.CoDt
      )
    ))
    #Calculate commercial auto and light truck emission rates by vehicle age
    CommServ_ <-
      c(CommServ_,
        with(
          CommServ_,
          calcCommVehEmissionRatesByAge(
            yr,
            CommServAutoProp.AgPt,
            CommServLtTruckProp.AgPt,
            CommServAutoMpgMpkwh.AgPt,
            CommServLtTruckMpgMpkwh.AgPt,
            AveAutoFuelCo2,
            AveLtTruckFuelCo2,
            MjPerGallon,
            PowerCo2.CoYr,
            CommServAutoHcDvmt.CoDt,
            CommServAutoEvDvmt.CoDt,
            CommServLtTruckHcDvmt.CoDt,
            CommServLtTruckEvDvmt.CoDt
          )
        ))
    #Save the results
    Filename <-
      paste(OutputYearDir, "/", "CommServ_", ".RData", sep = "")
    save(CommServ_, file = Filename)
    
    
    #Step 4e: Calculate Total Costs and VMT Surcharge to Pay for Infrastructure
    #==========================================================================
    
    #Calculate total costs
    #---------------------
    #Calculate total household road cost, adjusting for DVMT adjustment
    DvmtAdjRatio <-
      sum(CostSummary.CoVa[, "AdjDvmt"]) / sum(CostSummary.CoVa[, "Dvmt"])
    TotHhRoadCost <-
      sum(CostSummary.CoVa[, "TotRoadCost"]) * DvmtAdjRatio
    #Calculate light vehicle DVMT
    LtVehDvmt <-
      sum(CostSummary.CoVa[, "AdjDvmt"]) + CommServ_$CommVehDvmt
    #First iteration, calculate the extra modernization cost for new lanes
    #(ExtraModCost)
    if (it == 1) {
      HvyVehDvmtEq <-
        TruckDvmt * CongModel_$Pce.Ty["Truck"] + 
        sum(BusDvmt.Ma) * CongModel_$Pce.Ty["Bus"]
      LtVehAddCostProp <- LtVehDvmt / (LtVehDvmt + HvyVehDvmtEq)
      LnMiAddCost <- LtVehAddCostProp * AnnLnMiAddCosts.Yr[yr] / 365
      ExtraModCost <- LnMiAddCost / LtVehDvmt
      TotRoadCost <-
        TotHhRoadCost + CommServ_$CommServCosts.["TotRoadCost"] + LnMiAddCost
      rm(HvyVehDvmtEq, LtVehAddCostProp, LnMiAddCost)
    #Otherwise sum household & commercial vehicle costs because they include
    #the added lane-mile costs
    } else {
      TotRoadCost <-
        TotHhRoadCost + CommServ_$CommServCosts.["TotRoadCost"]
    }
    
    # Calculate total revenues
    #-------------------------
    # Calculate total household revenues, adjusting for DVMT adjustment
    TotHhRoadUseTax <-
      sum(CostSummary.CoVa[, "RoadUseTax"]) * DvmtAdjRatio
    #Add in estimated congestion tax if 1st iteration (since costs calculated
    #before congestion tax)
    if (it == 1) {
      TotHhRoadUseTax <-
        TotHhRoadUseTax + sum(HhRoadDvmt.Ma * AveCongTaxPerMi.Ma)
    }
    #Add in commercial light service vehicle
    TotRoadUseTax <-
      TotHhRoadUseTax + CommServ_$CommServCosts.["RoadUseTax"]
    
    #Compare total costs to revenues and calculate VMT surcharge to pay for
    #system
    #----------------------------------------------------------------------
    #This procedure calculates how much to increase a VMT tax to pay for system
    #costs. It includes guards to keep the procedure from adding a negative
    #surcharge that counteracts VMT tax assumptions that are inputs to the
    #model. Calculate the gap between taxes and costs.
    TaxGap <- TotRoadCost - TotRoadUseTax
    if (CalcVmtSurcharge) {
      #If this is the first iteration, the tax gap per mile is added to the
      #starting surcharge of 0
      if (it == 1) {
        # Calculate a VmtSurcharge only if there is a positive tax gap
        if (TaxGap > 0) {
          VmtSurcharge.It[it] <- TaxGap / LtVehDvmt
          # If the tax gap is negative, the VMT surcharge is zero
        } else {
          VmtSurcharge.It[it] <- 0
        }
        #If later iteration, add the calculated tax gap per mile to the previous
        #surcharge
      } else {
        # If the VmtSurcharge for the previous iteration is positive,
        # calculate the added surcharge for the iteration
        if (VmtSurcharge.It[it - 1] > 0) {
          VmtSurcharge.It[it] <-
            VmtSurcharge.It[it - 1] +  TaxGap / LtVehDvmt
          # Otherwise the VmtSurcharge is zero
        } else {
          VmtSurcharge.It[it] <- 0
        }
      }
      #End calculation of VMT surcharge (if calculation is to be done)
    }
    
    #Clean up
    #--------
    RoadCostSummary. <-
      c(
        TotRoadCost = TotRoadCost,
        TotRoadUseTax = TotRoadUseTax,
        TaxGap = TaxGap
      )
    Filename <-
      paste(OutputYearDir, "/", "RoadCostSummary.", ".RData", sep = "")
    save(RoadCostSummary., file = Filename)
    rm(
      DvmtAdjRatio,
      TotHhRoadCost,
      LtVehDvmt,
      TotRoadCost,
      TotHhRoadUseTax,
      TotRoadUseTax
    )
    gc()

    
    #Step 4f: Adjust MPG & MPKWH for ecodriving and low rolling resistance tires
    #===========================
    #Only do this once, on the first iteration. Otherwise ecodriving would keep
    #increasing MPG with each iteration.
    if (it == 1) {
      StartTime <- Sys.time()
      print(paste("Iteration", it, "Adjust MPG & MPKWH for ecodriving etc."))
      
      for (co in Co) {
        print(co)
        
        #Load county file
        Filename <-
          paste(OutputYearDir, "/", co, ".RData", sep = "")
        SynPop.. <- assignLoad(Filename)
        rm(Filename)
        
        #Identify metropolitan area
        MetroArea <- CountyGroups..[co, "Msa"]
        IsMetro. <- SynPop..$DevType == "Metropolitan"
        
        #Calculate adjustments
        HasVeh.Hh <- SynPop..$Hhvehcnt >= 1
        ModelVar. <-
          c(
            "Houseid",
            "Hhvehcnt",
            "IsEcoDriver",
            "IsLowRollTire",
            "Powertrain",
            "VehMpg",
            "VehMpkwh"
          )
        
        #Get adjustments by powertrain for area. Note that non-mpo areas are
        #average of smaller MPO areas (should be changed in future)
        if (is.na(MetroArea)) {
          MpgMpkwhAdj.Pt <-
            colMeans(MpgMpkwhAdj.MaPt[c("RogueValley", "Bend", "Corvallis"),])
        } else {
          MpgMpkwhAdj.Pt <- MpgMpkwhAdj.MaPt[MetroArea,]
        }
        MpgMpkwhAdj_ <-
          adjEcoTire(
            Data.. = SynPop..[HasVeh.Hh, ModelVar.],
            MpgMpkwhAdj.Pt = MpgMpkwhAdj.Pt,
            TireMpgImp = EcoTire..Yr[yr, "TireMpgImp"],
            TireMpkwhImp = EcoTire..Yr[yr, "TireMpkwhImp"]
          )
        rm(ModelVar.)
        
        # Assign to households
        SynPop..$VehMpg[HasVeh.Hh] <- MpgMpkwhAdj_$VehMpg_
        SynPop..$VehMpkwh[HasVeh.Hh] <- MpgMpkwhAdj_$VehMpkwh_
        rm(HasVeh.Hh, MpgMpkwhAdj_)
        
        # Save the household dataset
        Filename <- paste(OutputYearDir, "/", co, ".RData", sep = "")
        save(SynPop.., file = Filename, compress = TRUE)
        rm(SynPop.., IsMetro., MetroArea)
        gc()
        
      }
      
      print(StartTime)
      print(Sys.time())
      
    }
    
    Filename <-
      paste(OutputYearDir, "/", "VmtSurcharge.It", ".RData", sep = "")
    save(VmtSurcharge.It, file = Filename)
    rm(Filename)
    #End of for loop to equilibrate DVMT, congestion, costs and road use taxes
  }
  
  
  #===============================================  
  #STEP 5: CALCULATE WALK, BIKE, AND TRANSIT TRIPS
  #===============================================
  for (co in Co) {
    
    # Load county file
    Filename <- paste(OutputYearDir, "/", co, ".RData", sep = "")
    SynPop.. <- assignLoad(Filename)
    
    # Identify metropolitan area
    MetroArea <- CountyGroups..[co, "Msa"]
    IsMetro. <- SynPop..$DevType == "Metropolitan"
    
    #Calculate for metropolitan households    
    if (any(IsMetro.)) {
      #Identify data fields to use
      ModelVar. <- 
        c("Age0to14", "Age15to19", "Age20to29", "Age30to54", "Age55to64",
          "Age65Plus", "Hhsize", "Hhincttl", "Htppopdn", "Tranmilescap",
          "Urban", "Dvmt", "Hhvehcnt")
      
      #Calculate the alternative mode trips and add to household dataset    
      AltModeTrips_ <- 
        calcAltModeTrips(SynPop..[IsMetro., ModelVar.], 
                         AltModeModels_, "Metro")
      SynPop..$AveWalkTrips[IsMetro.] <- AltModeTrips_$Walk
      SynPop..$AveBikeTrips[IsMetro.] <- AltModeTrips_$Bike
      SynPop..$AveTransitTrips[IsMetro.] <- AltModeTrips_$Transit
      rm(ModelVar., AltModeTrips_)
    }

    #Calculate for non-metropolitan households    
    if (any(!IsMetro.)) {
      #Identify data fields to use
      ModelVar. <- 
        c("Age0to14", "Age15to19", "Age20to29", "Age30to54", "Age55to64",
          "Age65Plus", "Hhsize", "Hhincttl", "Htppopdn",
          "Urban", "Dvmt", "Hhvehcnt")
      
      #Calculate the alternative mode trips and add to household dataset    
      AltModeTrips_ <- 
        calcAltModeTrips(SynPop..[!IsMetro., ModelVar.], 
                         AltModeModels_, "NonMetro")
      SynPop..$AveWalkTrips[!IsMetro.] <- AltModeTrips_$Walk
      SynPop..$AveBikeTrips[!IsMetro.] <- AltModeTrips_$Bike
      SynPop..$AveTransitTrips[!IsMetro.] <- AltModeTrips_$Transit
      rm(ModelVar., AltModeTrips_)
    }
    
    # Save the household dataset
    Filename <- paste(OutputYearDir, "/", co, ".RData", sep = "")
    save( SynPop.., file=Filename, compress=TRUE )
    rm( SynPop.. )
    gc()
    
  }
  
  
  #===========================================================================
  #STEP 6: CALCULATE METROPOLITAN AREA HEAVY VEHICLE CONSUMPTION AND EMISSIONS
  #===========================================================================
  
  #Calculate truck and bus age distributions
  #-----------------------------------------
  #Calculate the truck age distribution
  TruckAgProp.Ag <-
    adjustHvyVehAgeDistribution(TruckBusAgeDist.AgTy[, "Truck"],
                                AdjRatio = AgeAdj.YrTy[yr, "Truck"])
  #Calculate bus age distribution
  BusAgProp.Ag <-
    adjustHvyVehAgeDistribution(TruckBusAgeDist.AgTy[, "Bus"],
                                AdjRatio = AgeAdj.YrTy[yr, "Bus"])
  
  #Calculate truck and bus fuel economy
  #------------------------------------
  #Calculate truck fuel economy
  TruckMpg <-
    assignHvyVehFuelEconomy(
      TruckAgProp.Ag,
      Mpg..Yr = HvyVehMpgMpk..Yr,
      Type = "Truck_MPG",
      CurrYear = yr
    )
  #Calculate bus fuel economy
  BusMpg <-
    assignHvyVehFuelEconomy(
      BusAgProp.Ag,
      Mpg..Yr = HvyVehMpgMpk..Yr,
      Type = "Bus_MPG",
      CurrYear = yr
    )
  #Adjust fuel economy to account for congestion, eco-driving, etc for
  #metropolitan areas
  TruckMpg.Ma <- MpgMpkwhAdj.MaPt[, "TruckIce"] * TruckMpg
  BusMpg.Ma <- MpgMpkwhAdj.MaPt[, "BusIce"] * BusMpg
  #Calculate fuel economy for non-metropolitan areas
  NonMpoTruckMpg <-
    mean(MpgMpkwhAdj.MaPt[c("RogueValley", "Bend", "Corvallis"), "TruckIce"])
  
  #Calculate truck fuel consumption by fuel type
  #---------------------------------------------
  #Calculate overall fuel consumption
  TruckFuel.Ma <- Dvmt.MaTy[, "Truck"] / TruckMpg.Ma
  rm(TruckMpg.Ma)
  NonMpoTruckDvmt <- TruckDvmt - sum(Dvmt.MaTy[, "Truck"])
  NonMpoTruckFuel <- NonMpoTruckDvmt * NonMpoTruckMpg
  #Calculate fuel consumption by type
  TruckFuelProp.Ft <- numeric(5)
  names(TruckFuelProp.Ft) <- Ft
  TruckFuelInput. <- unlist(HvyTruckFuels..Yr[yr, ])
  PropDiesel <-
    1 - TruckFuelInput.["PropGas"] - TruckFuelInput.["PropCng"]
  TruckFuelProp.Ft["ULSD"] <-
    PropDiesel * (1 - TruckFuelInput.["DieselPropBio"])
  TruckFuelProp.Ft["Biodiesel"] <-
    PropDiesel * (TruckFuelInput.["DieselPropBio"])
  TruckFuelProp.Ft["Gasoline"] <- (TruckFuelInput.["PropGas"]) *
    (1 - TruckFuelInput.["GasPropEth"])
  TruckFuelProp.Ft["Ethanol"] <- (TruckFuelInput.["PropGas"]) *
    (TruckFuelInput.["GasPropEth"])
  TruckFuelProp.Ft["CNG"] <- (TruckFuelInput.["PropCng"])
  TruckFuel.MaFt <- outer(TruckFuel.Ma, TruckFuelProp.Ft, "*")
  NonMpoTruckFuel.Ft <- NonMpoTruckFuel * TruckFuelProp.Ft
  rm(TruckFuelInput., PropDiesel, TruckFuelProp.Ft)
  
  #Calculate Bus Fuel Consumption and Emissions
  #--------------------------------------------
  #Calculate overall fuel consumption
  BusPropFuel.Ma <- 1 - BusPropElectric.MaYr[, yr]
  BusFuel.Ma <- (Dvmt.MaTy[, "Bus"] * BusPropFuel.Ma) / BusMpg.Ma
  rm(BusMpg.Ma, BusPropFuel.Ma)
  #Calculate fuel consumption by type
  BusFuelProp.MaFt <-
    array(0,
          dim = c(length(Ma), length(Ft)),
          dimnames = list(Ma, Ft))
  for (ma in Ma) {
    BusFuelProp.Ft <- numeric(5)
    names(BusFuelProp.Ft) <- Ft
    BusFuelInput. <- unlist(BusFuels.FpYrMa[, yr, ma])
    PropDiesel <-
      1 - BusFuelInput.["PropGas"] - BusFuelInput.["PropCng"]
    BusFuelProp.Ft["ULSD"] <-
      PropDiesel * (1 - BusFuelInput.["DieselPropBio"])
    BusFuelProp.Ft["Biodiesel"] <-
      PropDiesel * (BusFuelInput.["DieselPropBio"])
    BusFuelProp.Ft["Gasoline"] <- (BusFuelInput.["PropGas"]) *
      (1 - BusFuelInput.["GasPropEth"])
    BusFuelProp.Ft["Ethanol"] <- (BusFuelInput.["PropGas"]) *
      (BusFuelInput.["GasPropEth"])
    BusFuelProp.Ft["CNG"] <- (BusFuelInput.["PropCng"])
    BusFuelProp.MaFt[ma,] <- BusFuelProp.Ft
    rm(BusFuelInput., PropDiesel, BusFuelProp.Ft)
  }
  BusFuel.MaFt <- sweep(BusFuelProp.MaFt, 1, BusFuel.Ma, "*")
  
  #Calculate emissions per gallon of fuel consumed
  #-----------------------------------------------
  FuelCo2.Ft <- numeric(5)
  names(FuelCo2.Ft) <- Ft
  FuelCo2Input. <- unlist(Inputs_$FuelCo2..Yr[yr,])
  FuelCo2.Ft["ULSD"] <- FuelCo2Input.["ULSD"]
  FuelCo2.Ft["Biodiesel"] <- FuelCo2Input.["Biodiesel"]
  if (yr == "1990") {
    FuelCo2.Ft["Gasoline"] <- FuelCo2Input.["RFG"]
  } else {
    FuelCo2.Ft["Gasoline"] <- FuelCo2Input.["CARBOB"]
  }
  FuelCo2.Ft["Ethanol"] <- FuelCo2Input.["Ethanol"]
  FuelCo2.Ft["CNG"] <- FuelCo2Input.["Cng"]
  
  #Calculate truck and bus emissions
  #---------------------------------
  #Calculate truck emissions
  TruckMj.MaTy <- TruckFuel.MaFt * MjPerGallon
  TruckCo2e.MaTy <-
    sweep(TruckMj.MaTy, 2, FuelCo2.Ft, "*") / 1000000
  TruckCo2e.Ma <- rowSums(TruckCo2e.MaTy)
  NonMpoTruckMj.Ty <- NonMpoTruckFuel.Ft * MjPerGallon
  NonMpoTruckCo2e <- sum(NonMpoTruckMj.Ty * FuelCo2.Ft) / 1000000
  rm(TruckMj.MaTy, TruckCo2e.MaTy, NonMpoTruckMj.Ty)
  #Calculate bus emissions
  BusMj.MaTy <- BusFuel.MaFt * MjPerGallon
  BusHcCo2e.MaTy <-
    sweep(BusMj.MaTy, 2, FuelCo2.Ft, "*") / 1000000
  BusHcCo2e.Ma <- rowSums(BusHcCo2e.MaTy)
  rm(BusMj.MaTy, BusHcCo2e.MaTy, FuelCo2.Ft)
  
  #Calculate bus and rail emissions from electricity consumption
  #-------------------------------------------------------------
  #Calculate DVMT and power consumed
  RailDvmt.Ma <- RailRevMi.Ma * TranAdjFactor / 365
  RailPower.Ma <- RailDvmt.Ma / HvyVehMpgMpk..Yr[yr, "Train_MPkWh"]
  BusEvDvmt.Ma <- Dvmt.MaTy[, "Bus"] * BusPropElectric.MaYr[, yr]
  BusPower.Ma <- BusEvDvmt.Ma / HvyVehMpgMpk..Yr[yr, "Bus_MPkWh"]
  RailBusPower.Ma <- RailPower.Ma + BusPower.Ma
  rm(RailDvmt.Ma, RailPower.Ma, BusEvDvmt.Ma, BusPower.Ma)
  #Calculate average emissions per kwh by metropolitan area
  CoToMa. <- CountyGroups..$Msa
  names(CoToMa.) <- rownames(CountyGroups..)
  CoToMa. <- CoToMa.[!is.na(CoToMa.)]
  PowerCo2.Ma <-
    tapply(PowerCo2.CoYr[names(CoToMa.), yr], CoToMa., mean)[Ma]
  #Calculate total emissions by metropolitan area
  RailBusEvCo2e.Ma <- RailBusPower.Ma * PowerCo2.Ma / 2204.62262
  rm(CoToMa., PowerCo2.Ma)
  
  #Save the results
  #----------------
  Filename <-
    paste(OutputYearDir, "/", "TruckFuel.MaFt", ".RData", sep = "")
  save(TruckFuel.MaFt, file = Filename)
  Filename <-
    paste(OutputYearDir, "/", "NonMpoTruckDvmt", ".RData", sep = "")
  save(NonMpoTruckDvmt, file = Filename)
  Filename <-
    paste(OutputYearDir, "/", "NonMpoTruckFuel.Ft", ".RData", sep = "")
  save(NonMpoTruckFuel.Ft, file = Filename)
  Filename <-
    paste(OutputYearDir, "/", "BusFuel.MaFt", ".RData", sep = "")
  save(BusFuel.MaFt, file = Filename)
  Filename <-
    paste(OutputYearDir, "/", "TruckCo2e.Ma", ".RData", sep = "")
  save(TruckCo2e.Ma, file = Filename)
  Filename <-
    paste(OutputYearDir, "/", "NonMpoTruckCo2e", ".RData", sep = "")
  save(NonMpoTruckCo2e, file = Filename)
  Filename <-
    paste(OutputYearDir, "/", "BusHcCo2e.Ma", ".RData", sep = "")
  save(BusHcCo2e.Ma, file = Filename)
  Filename <-
    paste(OutputYearDir, "/", "RailBusPower.Ma", ".RData", sep = "")
  save(RailBusPower.Ma, file = Filename)
  Filename <-
    paste(OutputYearDir, "/", "RailBusEvCo2e.Ma", ".RData", sep = "")
  save(RailBusEvCo2e.Ma, file = Filename)
  rm(
    TruckFuel.MaFt,
    BusFuel.MaFt,
    TruckCo2e.Ma,
    BusHcCo2e.Ma,
    RailBusPower.Ma,
    RailBusEvCo2e.Ma
  )
  
  
  #=========================
  #END THE LOOP FOR THE YEAR
  #=========================
  
  # Clean up
  gc()
  
}
