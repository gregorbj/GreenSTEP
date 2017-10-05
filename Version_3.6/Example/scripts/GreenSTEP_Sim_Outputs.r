#=======================
#GreenSTEP_Sim_Outputs.r
#=======================
#Copyright 2009 - 2015, Oregon Department of Transportation
#Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at
#http://www.apache.org/licenses/LICENSE-2.0
#Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
#Version: 3.5 Beta
#Date: 5/24/15

#Iterate through all years
#=========================

for( yr in RunYears ) {
  
  print( paste(yr, " outputs running") )
  print( Sys.time() )
  
  OutputYearDir <- paste0(OutputDir, "/Year", yr)
  
  #Make objects to store results
  #=============================
  
  Hh_ <- list()
  
  # Define density groupings (other groupings are already defined
  Dg <- c( "0-999", "1000-2499", "2500-4999", "5000-7499", "7500-9999", "10000-14999",
           "15000-19999", "20000-24999", "25000-30000", "30000+" )
  
  # Define urban mixed use designation ( Yes:"1", No: "0" )
  Mx <- c( "0", "1" )
  
  # Define vehicle types
  Ty <- c( "LtVeh", "Truck", "Bus" )
  
  # Define light vehicle types
  Vt <- c( "Auto", "LtTruck" )
  
  # Define age group
  Ag <- as.factor( as.character(0:32) )
  
  # Define powertrain types
  Pt <- c( "Ice", "Hev", "Phev", "Ev" )
  
  # Set up dimensions for arrays by county, income group and development type
  OutputDims. <- c( length( Co ), length( Ig ), length( Dt ), length( Dg ) )
  OutputDimnames_ <- list( Co, Ig, Dt, Dg ) 
  
  # Initialize arrays by county, income group, development type, and density group
  #-------------------------------------------------------------------------
  
  # Arrays that tabulate household characteristics
  Hh_$Pop.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$Pop.CoDtDgMx <- array( 0, dim=c( length(Co), length(Dt), length(Dg), length(Mx)),
                             dimnames=list(Co,Dt,Dg,Mx) )
  Hh_$Hh.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$UrbanHh.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$DrvAgePop.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$Dvmt.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$LtVehDvmt.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$LtWtVehDvmt.CoDtDgMx <- array( 0, dim=c( length(Co), length(Dt), length(Dg), length(Mx)),
                                     dimnames=list(Co,Dt,Dg,Mx) )
  Hh_$HhOpCost.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$HhExtCost.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$HhVehOwnCost.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$HhParkingCost.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$HhInc.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$HhCo2e.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$AveDensity.CoDg <- array( 0, dim=c( length(Co), length(Dg)), dimnames=list(Co,Dg) )
  Hh_$AveDensity.CoDtDg <- array( 0, dim=c( length(Co), length(Dt), length(Dg) ), dimnames=list(Co,Dt,Dg) )
  Hh_$MetroAreaDensity.CoDg <- array( 0, dim=c( length(Co), length(Dg)), dimnames=list(Co,Dg) )
  Hh_$WalkTrips.CoDtDgMx <- array( 0, dim=c( length(Co), length(Dt), length(Dg), length(Mx)), 
                                   dimnames=list(Co,Dt,Dg,Mx) )
  Hh_$BikeTrips.CoDtDgMx <- array( 0, dim=c( length(Co), length(Dt), length(Dg), length(Mx)), 
                                   dimnames=list(Co,Dt,Dg,Mx) )
  Hh_$TransitTrips.CoDtDgMx <- array( 0, dim=c( length(Co), length(Dt), length(Dg), length(Mx)), 
                                      dimnames=list(Co,Dt,Dg,Mx) )
  Hh_$FuelGallons.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$ElecKwh.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$FuelCo2e.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$ElecCo2e.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )          
  Hh_$CarshareHh.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$MedianInc.Co <- numeric( length( Co ) ); names( Hh_$MedianInc.Co ) <- Co
  Hh_$MedianInc.CoDt <- array( 0, dim=c( length(Co), length(Dt) ), dimnames=list( Co, Dt ) )
  
  # Arrays that tabulate light vehicle characteristics
  Hh_$AveAutoVehAge.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$AveLtTruckVehAge.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$NumAuto.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$NumLtTruck.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$AutoEvDvmt.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$LtTruckEvDvmt.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$AutoHcDvmt.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$LtTruckHcDvmt.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$AutoFuel.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$LtTruckFuel.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$AutoPower.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$LtTruckPower.CoIgDtDg <- array( 0, dim=OutputDims., dimnames=OutputDimnames_ )
  Hh_$AutoAge.CoAg <- array( 0, dim=c( length(Co), length(Ag) ), dimnames=list( Co, Ag ) )
  Hh_$LtTruckAge.CoAg <- array( 0, dim=c( length(Co), length(Ag) ), dimnames=list( Co, Ag ) )
  Hh_$NumPowertrain.CoDtDgPt <- array( 0, dim=c( length(Co), length(Dt), length(Dg), length(Pt) ),
                                       dimnames=list( Co, Dt, Dg, Pt ) ) 
  Hh_$NumPowertrain.CoVtAgPt <- array( 0, dim=c( length(Co), length(Vt), length(Ag), length(Pt) ),
                                       dimnames=list( Co, Vt, Ag, Pt ) ) 
  Hh_$HcDvmt.CoDtDgPt <- array( 0, dim=c( length(Co), length(Dt), length(Dg), length(Pt) ),
                                dimnames=list( Co, Dt, Dg, Pt ) ) 
  Hh_$EvDvmt.CoDtDgPt <- array( 0, dim=c( length(Co), length(Dt), length(Dg), length(Pt) ),
                                dimnames=list( Co, Dt, Dg, Pt ) ) 
  Hh_$Fuel.CoDtDgPt <- array( 0, dim=c( length(Co), length(Dt), length(Dg), length(Pt) ),
                              dimnames=list( Co, Dt, Dg, Pt ) )
  Hh_$NumPowertrain.CoVtPt <- Hh_$HcDvmt.CoVtPt <- Hh_$EvDvmt.CoVtPt <- Hh_$Fuel.CoVtPt <- 
    array( 0, dim=c( length(Co), length(Vt), length(Pt) ), dimnames=list( Co, Vt, Pt ) )
  Hh_$HcVehDvmt.CoAgVt <- Hh_$HcVehCo2e.CoAgVt <- Hh_$EvVehDvmt.CoAgVt <- Hh_$EvVehCo2e.CoAgVt <- 
    array( 0, dim=c( length(Co), length(Ag), length(Vt) ), dimnames=list( Co, Ag, Vt ) )
  
  #Add cost summary and other saved table
  #--------------------------------------
  TableNames. <- c( "CostSummary.CoVa", "Dvmt.CoDt", "Inc.CoDt",  "Pop.CoDt" )
  for( tn in TableNames. ) {
    TableFile <- paste( OutputYearDir, "/", tn, ".RData", sep="" )
    Hh_[[tn]] <- assignLoad( TableFile )
  }
  rm( TableNames. )
  Hh_$AveFuelCo2e. <- assignLoad(paste0(OutputYearDir, "/AveFuelCo2e.RData"))
  Hh_$AveElectricCo2e.Co <- assignLoad(paste0(OutputYearDir, "/AveElectricCo2e.Co.RData"))
  
  #Collect other data that was saved at run time into lists
  #========================================================
  
  #Save the metropolitan area congestion and other data
  Metropolitan_ <- list()
  TableNames. <- c( "ArtLnMiCap.Ma", "AveCongTaxPerMi.Ma", "AveSpeed.MaTy", "BusHcCo2e.Ma",
                    "BusFuel.MaFt", "BusRevMi.Ma", "CommVehDvmt.Ma", "CongVmt.ClFcTyMa",
                    "DelayVehHr.MaTy", "Dvmt.MaTy", "FfVehHr.MaTy", "FwyLnMiCap.Ma", 
                    "MpgMpkwhAdj.MaPt", "RailBusEvCo2e.Ma", "RailBusPower.Ma", "RailRevMi.Ma",
                    "TranRevMiCap.Ma", "TruckCo2e.Ma", "TruckFuel.MaFt", "VehHr.MaTy", 
                    "NonMpoTruckDvmt", "NonMpoTruckFuel.Ft", "NonMpoTruckCo2e" )
  for( tn in TableNames. ) {
    TableFile <- paste( OutputYearDir, "/", tn, ".RData", sep="" )
    Metropolitan_[[tn]] <- assignLoad( TableFile )
  }
  rm( TableNames. )
  
  #Iterate through counties (districts) and make summary tables
  #============================================================
  
  for( co in Co ) { local( {
    
    # Load county files
    Filename <- paste( OutputYearDir, "/", co, ".RData", sep="" )
    SynPop.. <- assignLoad( Filename )
    SynPop..$DevType <- factor( SynPop..$DevType, levels=c("Metropolitan", "Town", "Rural") )
    SynPop..$Urban <- factor( as.character( SynPop..$Urban ), levels=c( "0", "1" ) )
    
    # Calculate average densities
    #============================
    
    # Classify density group of each household
    DgCut <- c( 0, 1000, 2500, 5000, 7500, 10000, 15000, 20000, 25000, 30000, max(SynPop..$Htppopdn) )
    DenGroup. <- cut( SynPop..$Htppopdn, DgCut, right=FALSE, labels=Dg )   				
    
    # Compute the average density by density-group 
    Density.Dg <- tapply( SynPop..$Htppopdn, DenGroup., mean)[Dg]
    Density.Dg[ is.na( Density.Dg ) ] <- 0
    Hh_$AveDensity.CoDg[ co, ] <<- Density.Dg
    #Hh_$AveDensity.CoDg[ co, ] <- Density.Dg
    
    # Compute average density by density-group and development type
    Density.DtDg <- tapply( SynPop..$Htppopdn, list( SynPop..$DevType, DenGroup. ), mean )[Dt,Dg]
    Density.DtDg[ is.na( Density.DtDg ) ] <- 0
    Hh_$AveDensity.CoDtDg[ co, , ] <<- Density.DtDg  
    #Hh_$AveDensity.CoDtDg[ co, , ] <- Density.DtDg  
    
    # Compute the metro-area density by density-group
    Hh_$MetroAreaDensity.CoDg[ co, ] <<- Density.DtDg[ "Metropolitan", ]
    #Hh_$MetroAreaDensity.CoDg[ co, ] <- Density.DtDg[ "Metropolitan", ]
    
    # Tabulate household characteristics
    #===================================
    
    # Household population
    Pop.Hh <- SynPop..$Hhsize
    Pop.3d <- tapply( Pop.Hh, list( SynPop..$IncGrp, SynPop..$DevType, DenGroup. ),
                      function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
    Pop.3d[ is.na( Pop.3d ) ] <- 0
    Hh_$Pop.CoIgDtDg[ co, , , ] <<- Pop.3d
    #Hh_$Pop.CoIgDtDg[ co, , , ] <- Pop.3d
    
    # Household population by density group and urban mixed use designation
    Pop.3d <- tapply( Pop.Hh, list( SynPop..$DevType, DenGroup., SynPop..$Urban ),
                      function(x) sum( x, na.rm=TRUE ) )[Dt,Dg,Mx]
    Pop.3d[ is.na( Pop.3d ) ] <- 0
    Hh_$Pop.CoDtDgMx[ co, , , ] <<- Pop.3d
    #Hh_$Pop.CoDtDgMx[ co, , , ] <- Pop.3d
    
    # Number of households
    Tab1.3d <- table( SynPop..$IncGrp, SynPop..$DevType, DenGroup. )[Ig,Dt,Dg]
    Hh_$Hh.CoIgDtDg[ co, , , ] <<- Tab1.3d
    #Hh_$Hh.CoIgDtDg[ co, , , ] <- Tab1.3d
    
    # Number of Urban households              
    Tab1.3d <- tapply( as.numeric( as.character( SynPop..$Urban ) ), 
                       list( SynPop..$IncGrp, SynPop..$DevType, DenGroup. ), sum )[Ig,Dt,Dg]
    Tab1.3d[ is.na( Tab1.3d ) ] <- 0
    Hh_$UrbanHh.CoIgDtDg[ co, , , ] <<- Tab1.3d					   
    #Hh_$UrbanHh.CoIgDtDg[ co, , , ] <- Tab1.3d					   
    
    # Driver age population
    VehHhDrvPop.3d <- tapply( SynPop..$DrvAgePop, 
                              list( SynPop..$IncGrp, SynPop..$DevType, DenGroup. ), 
                              function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
    VehHhDrvPop.3d[ is.na( VehHhDrvPop.3d ) ] <- 0
    Hh_$DrvAgePop.CoIgDtDg[ co, , , ] <<- VehHhDrvPop.3d
    #Hh_$DrvAgePop.CoIgDtDg[ co, , , ] <- VehHhDrvPop.3d
    
    # Dvmt 
    Dvmt.3d <- tapply( SynPop..$Dvmt, 
                       list( SynPop..$IncGrp, SynPop..$DevType, DenGroup. ), 
                       function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
    Dvmt.3d[ is.na( Dvmt.3d ) ] <- 0				
    Hh_$Dvmt.CoIgDtDg[ co, , , ] <<- Dvmt.3d
    #Hh_$Dvmt.CoIgDtDg[ co, , , ] <- Dvmt.3d
    
    # Light Vehicle Dvmt 
    LtVehDvmt.3d <- tapply( SynPop..$LtVehDvmt, 
                            list( SynPop..$IncGrp, SynPop..$DevType, DenGroup. ), 
                            function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
    LtVehDvmt.3d[ is.na( LtVehDvmt.3d ) ] <- 0				
    Hh_$LtVehDvmt.CoIgDtDg[ co, , , ] <<- LtVehDvmt.3d
    #Hh_$LtVehDvmt.CoIgDtDg[ co, , , ] <- Dvmt.3d
    
    # Household total cost                                                                     
    TotCost.Hh <- SynPop..$HhTotCost
    TotCost.3d <- tapply( TotCost.Hh, 
                          list( SynPop..$IncGrp, SynPop..$DevType, DenGroup. ), 
                          function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
    TotCost.3d[ is.na( TotCost.3d ) ] <- 0				
    Hh_$HhOpCost.CoIgDtDg[ co, , , ] <<- TotCost.3d
    #Hh_$HhOpCost.CoIgDtDg[ co, , , ] <- TotCost.3d
    
    # Household external cost                                                                     
    HhExtCost.Hh <- SynPop..$TotExtCost
    HhExtCost.3d <- tapply( HhExtCost.Hh, 
                            list( SynPop..$IncGrp, SynPop..$DevType, DenGroup. ), 
                            function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
    HhExtCost.3d[ is.na( HhExtCost.3d ) ] <- 0				
    Hh_$HhExtCost.CoIgDtDg[ co, , , ] <<- HhExtCost.3d
    #Hh_$HhExtCost.CoIgDtDg[ co, , , ] <- HhExtCost.3d
    
    # Household vehicle ownership cost                                                                     
    HhVehOwnCost.Hh <- SynPop..$VehOwnExp
    HhVehOwnCost.3d <- tapply( HhVehOwnCost.Hh, 
                               list( SynPop..$IncGrp, SynPop..$DevType, DenGroup. ), 
                               function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
    HhVehOwnCost.3d[ is.na( HhVehOwnCost.3d ) ] <- 0				
    Hh_$HhVehOwnCost.CoIgDtDg[ co, , , ] <<- HhVehOwnCost.3d
    #Hh_$HhVehOwnCost.CoIgDtDg[ co, , , ] <- HhVehOwnCost.3d
    
    # Household parking cost
    ParkingCost.Hh <- SynPop..$DailyPkgCost 
    ParkingCost.3d <- tapply( ParkingCost.Hh, 
                              list( SynPop..$IncGrp, SynPop..$DevType, DenGroup. ), 
                              function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
    ParkingCost.3d[ is.na( ParkingCost.3d ) ] <- 0				                
    Hh_$HhParkingCost.CoIgDtDg[ co, , , ] <<- ParkingCost.3d
    #Hh_$HhParkingCost.CoIgDtDg[ co, , , ] <- ParkingCost.3d
    
    # Household income
    HhInc.3d <- tapply( SynPop..$Hhincttl, 
                        list( SynPop..$IncGrp, SynPop..$DevType, DenGroup. ), 
                        function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
    HhInc.3d[ is.na( HhInc.3d ) ] <- 0				
    Hh_$HhInc.CoIgDtDg[ co, , , ] <<- HhInc.3d
    #Hh_$HhInc.CoIgDtDg[ co, , , ] <- HhInc.3d
    
    # Household CO2e
    Co2e.Hh <- SynPop..$FuelCo2e + SynPop..$ElecCo2e   
    Co2e.3d <- tapply( Co2e.Hh, 
                       list( SynPop..$IncGrp, SynPop..$DevType, DenGroup. ), 
                       function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
    Co2e.3d[ is.na( Co2e.3d ) ] <- 0				
    Hh_$HhCo2e.CoIgDtDg[ co, , , ] <<- Co2e.3d
    #Hh_$HhCo2e.CoIgDtDg[ co, , , ] <- Co2e.3d
    
    # Household walk trips
    AveWalkTrips.3d <- tapply( SynPop..$AveWalkTrips, 
                               list( SynPop..$DevType, DenGroup., SynPop..$Urban ),
                               function(x) sum( x, na.rm=TRUE ) )[Dt,Dg,Mx]
    AveWalkTrips.3d[ is.na( AveWalkTrips.3d ) ] <- 0
    Hh_$WalkTrips.CoDtDgMx[ co, , , ] <<- AveWalkTrips.3d
    #Hh_$WalkTrips.CoDtDgMx[ co, , , ] <- AveWalkTrips.3d
    
    # Household bike trips
    AveBikeTrips.3d <- tapply( SynPop..$AveBikeTrips, 
                               list( SynPop..$DevType, DenGroup., SynPop..$Urban ),
                               function(x) sum( x, na.rm=TRUE ) )[Dt,Dg,Mx]
    AveBikeTrips.3d[ is.na( AveBikeTrips.3d ) ] <- 0
    Hh_$BikeTrips.CoDtDgMx[ co, , , ] <<- AveBikeTrips.3d
    #Hh_$BikeTrips.CoDtDgMx[ co, , , ] <- AveBikeTrips.3d
    
    # Household transit trips
    AveTransitTrips.3d <- tapply( SynPop..$AveTransitTrips, 
                               list( SynPop..$DevType, DenGroup., SynPop..$Urban ),
                               function(x) sum( x, na.rm=TRUE ) )[Dt,Dg,Mx]
    AveTransitTrips.3d[ is.na( AveTransitTrips.3d ) ] <- 0
    Hh_$TransitTrips.CoDtDgMx[ co, , , ] <<- AveTransitTrips.3d
    #Hh_$TransitTrips.CoDtDgMx[ co, , , ] <- AveTransitTrips.3d
    
    # Household light weight vehicle (e.g. bicycle) trips
    LtWtVehDvmt.3d <- tapply( SynPop..$LtVehDvmt, 
                              list( SynPop..$DevType, DenGroup., SynPop..$Urban ),
                              function(x) sum( x, na.rm=TRUE ) )[Dt,Dg,Mx]
    LtWtVehDvmt.3d[ is.na( LtWtVehDvmt.3d ) ] <- 0
    Hh_$LtWtVehDvmt.CoDtDgMx[ co, , , ] <<- LtWtVehDvmt.3d
    #Hh_$LtWtVehDvmt.CoDtDgMx[ co, , , ] <- LtWtVehDvmt.3d
    
    # Fuel consumed by household
    FuelGallons.Hh <- SynPop..$FuelGallons
    FuelGallons.3d <- tapply( FuelGallons.Hh, 
                              list( SynPop..$IncGrp, SynPop..$DevType, DenGroup. ), 
                              function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
    FuelGallons.3d[ is.na( FuelGallons.3d ) ] <- 0				
    Hh_$FuelGallons.CoIgDtDg[ co, , , ] <<- FuelGallons.3d
    #Hh_$FuelGallons.CoIgDtDg[ co, , , ] <- FuelGallons.3d
    
    # Electricity consumed by household
    ElecKwh.Hh <- SynPop..$ElecKwh
    ElecKwh.3d <- tapply( ElecKwh.Hh, 
                          list( SynPop..$IncGrp, SynPop..$DevType, DenGroup. ), 
                          function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
    ElecKwh.3d[ is.na( ElecKwh.3d ) ] <- 0				
    Hh_$ElecKwh.CoIgDtDg[ co, , , ] <<- ElecKwh.3d
    #Hh_$ElecKwh.CoIgDtDg[ co, , , ] <- ElecKwh.3d
    
    # Greenhouse gas emissions from fuel consumption
    FuelCo2e.Hh <- SynPop..$FuelCo2e
    FuelCo2e.3d <- tapply( FuelCo2e.Hh, 
                           list( SynPop..$IncGrp, SynPop..$DevType, DenGroup. ), 
                           function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
    FuelCo2e.3d[ is.na( FuelCo2e.3d ) ] <- 0				
    Hh_$FuelCo2e.CoIgDtDg[ co, , , ] <<- FuelCo2e.3d
    #Hh_$FuelCo2e.CoIgDtDg[ co, , , ] <- FuelCo2e.3d
    
    # Greenhouse gas emissions from electricity consumption
    ElecCo2e.Hh <- SynPop..$ElecCo2e
    ElecCo2e.3d <- tapply( ElecCo2e.Hh, 
                           list( SynPop..$IncGrp, SynPop..$DevType, DenGroup. ), 
                           function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
    ElecCo2e.3d[ is.na( ElecCo2e.3d ) ] <- 0				
    Hh_$ElecCo2e.CoIgDtDg[ co, , , ] <<- ElecCo2e.3d
    #Hh_$ElecCo2e.CoIgDtDg[ co, , , ] <- ElecCo2e.3d
    
    # Carsharing households
    CarshareHh.3d <- tapply( SynPop..$Carshare, 
                             list( SynPop..$IncGrp,SynPop..$DevType, DenGroup. ), 
                             function(x) sum( x, na.rm=TRUE ) )[Ig,Dt,Dg]
    CarshareHh.3d[ is.na( CarshareHh.3d ) ] <- 0				                
    Hh_$CarshareHh.CoIgDtDg[ co, , , ] <<- CarshareHh.3d
    #Hh_$CarshareHh.CoIgDtDg[ co, , , ] <- CarshareHh.3d
    
    # Median household income
    Hh_$MedianInc.Co[co] <<- median( SynPop..$Hhincttl )
    #Hh_$MedianInc.Co[co] <- median( SynPop..$Hhincttl )
    MedianInc.Dt <- tapply( SynPop..$Hhincttl, SynPop..$DevType, median )[Dt]
    Hh_$MedianInc.CoDt[co,] <<- MedianInc.Dt
    #Hh_$MedianInc.CoDt[co,] <- MedianInc.Dt
    
    # Tabulate vehicle characteristics
    #=================================
    
    HasVeh.Hh <- SynPop..$Hhvehcnt >= 1   
    
    # Tabulate average vehicle age
    Tab.4d <- tapply( unlist( SynPop..$VehAge[ HasVeh.Hh ] ),
                      list( rep( SynPop..$IncGrp, SynPop..$Hhvehcnt ), 
                            rep( SynPop..$DevType, SynPop..$Hhvehcnt ),
                            rep( DenGroup., SynPop..$Hhvehcnt ), 
                            unlist( SynPop..$VehType[HasVeh.Hh] ) ), 
                      mean )[Ig,Dt,Dg,Vt[1:2]]
    Tab.4d[ is.na( Tab.4d ) ] <- 0
    Hh_$AveAutoVehAge.CoIgDtDg[ co, , , ] <<- Tab.4d[ , , , "Auto" ]
    Hh_$AveLtTruckVehAge.CoIgDtDg[ co, , , ] <<- Tab.4d[ , , , "LtTruck" ]
    #Hh_$AveAutoVehAge.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "Auto" ]
    #Hh_$AveLtTruckVehAge.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "LtTruck" ]
    
    # Tabulate number of vehicles
    Tab.4d <- table( rep( SynPop..$IncGrp, SynPop..$Hhvehcnt ),
                     rep( SynPop..$DevType, SynPop..$Hhvehcnt ), 
                     rep( DenGroup., SynPop..$Hhvehcnt ),
                     unlist( SynPop..$VehType[ HasVeh.Hh] ) )[Ig,Dt,Dg,Vt[1:2]]
    Tab.4d[ is.na( Tab.4d ) ] <- 0
    Hh_$NumAuto.CoIgDtDg[ co, , , ] <<- Tab.4d[ , , , "Auto" ]
    Hh_$NumLtTruck.CoIgDtDg[ co, , , ] <<- Tab.4d[ , , ,"LtTruck" ]
    #Hh_$NumAuto.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "Auto" ]
    #Hh_$NumLtTruck.CoIgDtDg[ co, , , ] <- Tab.4d[ , , ,"LtTruck" ]
    
    # Tabulate vehicle DVMT for autos and light trucks
    # Tabulate EV DVMT by income group, development type, and density group
    Tab.4d <- tapply( unlist( SynPop..$EvVehDvmt[ HasVeh.Hh ] ),
                      list( rep( SynPop..$IncGrp, SynPop..$Hhvehcnt ), 
                            rep( SynPop..$DevType, SynPop..$Hhvehcnt ),
                            rep( DenGroup., SynPop..$Hhvehcnt ), 
                            unlist( SynPop..$VehType[ HasVeh.Hh ] ) ), sum )[Ig,Dt,Dg,Vt[1:2]]
    Tab.4d[ is.na( Tab.4d ) ] <- 0
    Hh_$AutoEvDvmt.CoIgDtDg[ co, , , ] <<- Tab.4d[ , , , "Auto" ]
    Hh_$LtTruckEvDvmt.CoIgDtDg[ co, , , ] <<- Tab.4d[ , , ,"LtTruck" ]
    #Hh_$AutoEvDvmt.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "Auto" ]
    #Hh_$LtTruckEvDvmt.CoIgDtDg[ co, , , ] <- Tab.4d[ , , ,"LtTruck" ]
    
    # Tabulate HC-vehicle DVMT
    Tab.4d <- tapply( unlist( SynPop..$HcVehDvmt[ HasVeh.Hh] ),
                      list( rep( SynPop..$IncGrp, SynPop..$Hhvehcnt ), 
                            rep( SynPop..$DevType, SynPop..$Hhvehcnt ),
                            rep( DenGroup., SynPop..$Hhvehcnt ),				     
                            unlist( SynPop..$VehType[ HasVeh.Hh] ) ), sum )[Ig,Dt,Dg,Vt[1:2]]
    Tab.4d[ is.na( Tab.4d ) ] <- 0
    Hh_$AutoHcDvmt.CoIgDtDg[ co, , , ] <<- Tab.4d[ , , , "Auto" ]
    Hh_$LtTruckHcDvmt.CoIgDtDg[ co, , , ] <<- Tab.4d[ , , ,"LtTruck" ]
    #Hh_$AutoHcDvmt.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "Auto" ]
    #Hh_$LtTruckHcDvmt.CoIgDtDg[ co, , , ] <- Tab.4d[ , , ,"LtTruck" ]
    
    # Tabulate vehicle fuel (gas vehicle and PHEV)
    HcVehDvmt. <- unlist( SynPop..$HcVehDvmt[ HasVeh.Hh ] )
    HcVehDvmt.[ is.na( HcVehDvmt. ) ] <- 0
    VehMpg. <- unlist( SynPop..$VehMpg[ HasVeh.Hh ] )
    VehMpg.[ is.na( VehMpg. ) ] <- 0
    VehFuel. <- HcVehDvmt. / VehMpg.
    VehFuel.[ is.nan( VehFuel. ) ] <- 0
    Tab.4d <- tapply( VehFuel., 
                      list( rep( SynPop..$IncGrp, SynPop..$Hhvehcnt ),
                            rep( SynPop..$DevType, SynPop..$Hhvehcnt), 
                            rep( DenGroup., SynPop..$Hhvehcnt ),
                            unlist( SynPop..$VehType[ HasVeh.Hh ] ) ), 
                      sum )[Ig,Dt,Dg,Vt[1:2]]
    Tab.4d[ is.na( Tab.4d ) ] <- 0
    Hh_$AutoFuel.CoIgDtDg[ co, , , ] <<- Tab.4d[ , , , "Auto" ]
    Hh_$LtTruckFuel.CoIgDtDg[ co, , , ] <<- Tab.4d[ , , ,"LtTruck" ]
    #Hh_$AutoFuel.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "Auto" ]
    #Hh_$LtTruckFuel.CoIgDtDg[ co, , , ] <- Tab.4d[ , , ,"LtTruck" ]
    
    # Tabulate vehicle power (PHEV and EV) by income group, development type, and density group
    EvVehDvmt. <- unlist( SynPop..$EvVehDvmt[ HasVeh.Hh ] )
    EvVehDvmt.[ is.na( EvVehDvmt. ) ] <- 0
    VehMpkwh. <- unlist( SynPop..$VehMpkwh[ HasVeh.Hh ] )
    VehPower. <- EvVehDvmt. / VehMpkwh.
    VehPower.[ is.na( VehMpkwh. ) ] <- 0
    Tab.4d <- tapply( VehPower., 
                      list( rep( SynPop..$IncGrp, SynPop..$Hhvehcnt ),
                            rep( SynPop..$DevType, SynPop..$Hhvehcnt), 
                            rep( DenGroup., SynPop..$Hhvehcnt ),
                            unlist( SynPop..$VehType[ HasVeh.Hh ] ) ), 
                      sum )[Ig,Dt,Dg,Vt[1:2]]
    Tab.4d[ is.na( Tab.4d ) ] <- 0
    Hh_$AutoPower.CoIgDtDg[ co, , , ] <<- Tab.4d[ , , , "Auto" ]
    Hh_$LtTruckPower.CoIgDtDg[ co, , , ] <<- Tab.4d[ , , ,"LtTruck" ]
    #Hh_$AutoPower.CoIgDtDg[ co, , , ] <- Tab.4d[ , , , "Auto" ]
    #Hh_$LtTruckPower.CoIgDtDg[ co, , , ] <- Tab.4d[ , , ,"LtTruck" ]
    
    # Tabulate automobile and light truck age distributions
    Tab.2d <- table( unlist( SynPop..$VehAge[ HasVeh.Hh ] ), 
                     unlist( SynPop..$VehType[ HasVeh.Hh ] ) )
    Hh_$AutoAge.CoAg[ co, rownames( Tab.2d ) ] <<- Tab.2d[ , "Auto" ]          
    Hh_$LtTruckAge.CoAg[ co, rownames( Tab.2d ) ] <<- Tab.2d[ , "LtTruck" ]              
    #Hh_$AutoAge.CoAg[ co, ] <- Tab.2d[ , "Auto" ]          
    #Hh_$LtTruckAge.CoAg[ co, ] <- Tab.2d[ , "LtTruck" ]              
    
    # Tabulate number of vehicles by powertrain
    NumPowertrain.DtDgPt <- array( 0, dim=c( length(Dt), length(Dg), length(Pt) ),
                                   dimnames=list( Dt, Dg, Pt ) ) 
    Tab.3d <- table( rep( SynPop..$DevType[ HasVeh.Hh], SynPop..$Hhvehcnt[ HasVeh.Hh] ), 
                     rep( DenGroup.[ HasVeh.Hh], SynPop..$Hhvehcnt[ HasVeh.Hh] ),
                     unlist( SynPop..$Powertrain[ HasVeh.Hh] ) )
    NumPowertrain.DtDgPt[ dimnames( Tab.3d )[[1]], dimnames( Tab.3d )[[2]], dimnames( Tab.3d )[[3]] ] <- Tab.3d 
    Hh_$NumPowertrain.CoDtDgPt[ co, , , ] <<- NumPowertrain.DtDgPt
    #Hh_$NumPowertrain.CoDtDgPt[ co, , , ] <- Tab.3d
    
    # Tabulate number of vehicles by county, vehicle type, vehicle age, and powertrain
    NumPowertrain.VtAgPt <- array( 0, dim=c( length(Vt), length(Ag), length(Pt) ),
                                   dimnames=list( Vt, Ag, Pt ) ) 
    Tab.3d <- table( unlist( SynPop..$VehType[HasVeh.Hh] ), 
                     unlist( SynPop..$VehAge[HasVeh.Hh] ), 
                     unlist( SynPop..$Powertrain[HasVeh.Hh] ) )
    NumPowertrain.VtAgPt[ dimnames( Tab.3d )[[1]], dimnames( Tab.3d )[[2]], dimnames( Tab.3d )[[3]] ] <- Tab.3d 
    Hh_$NumPowertrain.CoVtAgPt[ co, , , ] <<- NumPowertrain.VtAgPt
    #Hh_$NumPowertrain.CoVtAgPt[ co, , , ] <- Tab.3d
    
    # Tabulate number of vehicles by vehicle type and powertrain
    NumPowertrain.VtPt <- array( 0, dim=c( length(Vt), length(Pt) ), dimnames=list( Vt, Pt ) )
    Tab.2d <- table( unlist( SynPop..$VehType[ HasVeh.Hh ] ), 
                     unlist( SynPop..$Powertrain[ HasVeh.Hh ] ) )
    NumPowertrain.VtPt[ dimnames( Tab.2d )[[1]], dimnames( Tab.2d )[[2]] ] <- Tab.2d
    Hh_$NumPowertrain.CoVtPt[ co, , ] <<- NumPowertrain.VtPt
    #Hh_$NumPowertrain.CoVtPt[ co, , ] <- NumPowertrain.VtPt
    
    # Tabulate VMT powered by hydrocarbon fuels by vehicle type and powertrain
    HcDvmt.VtPt <- array( 0, dim=c( length(Vt), length(Pt) ), dimnames=list( Vt, Pt ) )
    Tab.2d <- tapply( unlist( SynPop..$HcVehDvmt[ HasVeh.Hh ] ), 
                      list( unlist( SynPop..$VehType[ HasVeh.Hh ] ), 
                            unlist( SynPop..$Powertrain[ HasVeh.Hh ] ) ), 
                      sum ) 
    HcDvmt.VtPt[ dimnames( Tab.2d )[[1]], dimnames( Tab.2d )[[2]] ] <- Tab.2d
    Hh_$HcDvmt.CoVtPt[ co, , ] <<- HcDvmt.VtPt
    #Hh_$HcDvmt.CoVtPt[ co, , ] <- HcDvmt.VtPt
    
    # Tabulate VMT powered by electricity by vehicle type and powertrain 
    EvDvmt.VtPt <- array( 0, dim=c( length(Vt), length(Pt) ), dimnames=list( Vt, Pt ) )
    Tab.2d <- tapply( unlist( SynPop..$EvVehDvmt[ HasVeh.Hh ] ), 
                      list( unlist( SynPop..$VehType[ HasVeh.Hh ] ), 
                            unlist( SynPop..$Powertrain[ HasVeh.Hh ] ) ), 
                      sum ) 
    EvDvmt.VtPt[ dimnames( Tab.2d )[[1]], dimnames( Tab.2d )[[2]] ] <- Tab.2d
    Hh_$EvDvmt.CoVtPt[ co, , ] <<- EvDvmt.VtPt
    #Hh_$EvDvmt.CoVtPt[ co, , ] <- EvDvmt.VtPt
    
    # Tabulate fuel consumed by vehicle type and powertrain 
    Fuel.VtPt <- array( 0, dim=c( length(Vt), length(Pt) ), dimnames=list( Vt, Pt ) )
    Tab.2d <- tapply( unlist( SynPop..$HcVehDvmt[ HasVeh.Hh ] ) / unlist( SynPop..$VehMpg[ HasVeh.Hh ] ), 
                      list( unlist( SynPop..$VehType[ HasVeh.Hh ] ), 
                            unlist( SynPop..$Powertrain[ HasVeh.Hh ] ) ), 
                      sum )
    Tab.2d[ is.na( Tab.2d ) ] <- 0 
    Fuel.VtPt[ dimnames( Tab.2d )[[1]], dimnames( Tab.2d )[[2]] ] <- Tab.2d
    Hh_$Fuel.CoVtPt[ co, , ] <<- Fuel.VtPt
    #Hh_$Fuel.CoVtPt[ co, , ] <- Fuel.VtPt
    
    # Tabulate the VMT powered by hydrocarbon fuels by powertrain
    HcDvmt.DtDgPt <- array( 0, dim=c( length(Dt), length(Dg), length(Pt) ),
                            dimnames=list( Dt, Dg, Pt ) ) 
    HcVehDvmt. <- unlist( SynPop..$HcVehDvmt[ HasVeh.Hh ] )
    HcVehDvmt.[ is.na( HcVehDvmt. ) ] <- 0
    Tab.3d <- tapply( HcVehDvmt., 
                      list( rep( SynPop..$DevType, SynPop..$Hhvehcnt ),
                            rep( DenGroup., SynPop..$Hhvehcnt ), 
                            unlist( SynPop..$Powertrain[ HasVeh.Hh ] ) ), 
                      sum )
    HcDvmt.DtDgPt[ dimnames( Tab.3d )[[1]], dimnames( Tab.3d )[[2]], dimnames( Tab.3d )[[3]] ] <- Tab.3d
    HcDvmt.DtDgPt[ is.na( HcDvmt.DtDgPt ) ] <- 0
    Hh_$HcDvmt.CoDtDgPt[ co, , , ] <<- HcDvmt.DtDgPt
    #Hh_$HcDvmt.CoDtDgPt[ co, , , ] <- HcDvmt.DtDgPt
    
    # Tabulate the VMT powered by electricity by powertrain
    EvDvmt.DtDgPt <- array( 0, dim=c( length(Dt), length(Dg), length(Pt) ),
                            dimnames=list( Dt, Dg, Pt ) ) 
    EvVehDvmt. <- unlist( SynPop..$EvVehDvmt[ HasVeh.Hh ] )
    EvVehDvmt.[ is.na( EvVehDvmt. ) ] <- 0
    Tab.3d <- tapply( EvVehDvmt., 
                      list( rep( SynPop..$DevType, SynPop..$Hhvehcnt ),
                            rep( DenGroup., SynPop..$Hhvehcnt ),
                            unlist( SynPop..$Powertrain[ HasVeh.Hh ] ) ), 
                      sum )
    EvDvmt.DtDgPt[ dimnames( Tab.3d )[[1]], dimnames( Tab.3d )[[2]], dimnames( Tab.3d )[[3]] ] <- Tab.3d
    EvDvmt.DtDgPt[ is.na( EvDvmt.DtDgPt ) ] <- 0
    Hh_$EvDvmt.CoDtDgPt[ co, , , ] <<- EvDvmt.DtDgPt
    #Hh_$EvDvmt.CoDtDgPt[ co, , , ] <- EvDvmt.DtDgPt
    
    # Tabulate the gallons of hydrocarbon fuels consumed by powertrain
    Fuel.DtDgPt <- array( 0, dim=c( length(Dt), length(Dg), length(Pt) ),
                          dimnames=list( Dt, Dg, Pt ) )
    HcVehDvmt. <- unlist( SynPop..$HcVehDvmt[ HasVeh.Hh ] )
    HcVehDvmt.[ is.na( HcVehDvmt. ) ] <- 0
    VehMpg. <- unlist( SynPop..$VehMpg[ HasVeh.Hh ] )
    VehMpg.[ is.na( VehMpg. ) ] <- 0
    VehFuel. <- HcVehDvmt. / VehMpg.
    VehFuel.[ is.nan( VehFuel. ) ] <- 0
    Tab.3d <- tapply( VehFuel., 
                      list( rep( SynPop..$DevType, SynPop..$Hhvehcnt ),
                            rep( DenGroup., SynPop..$Hhvehcnt ),
                            unlist( SynPop..$Powertrain[ HasVeh.Hh ] ) ), 
                      sum )
    Fuel.DtDgPt[ dimnames( Tab.3d )[[1]], dimnames( Tab.3d )[[2]], dimnames( Tab.3d )[[3]] ] <- Tab.3d
    Fuel.DtDgPt[ is.na( Fuel.DtDgPt ) ] <- 0 			
    Hh_$Fuel.CoDtDgPt[ co, , , ] <<- Fuel.DtDgPt
    #Hh_$Fuel.CoDtDgPt[ co, , , ] <- Fuel.DtDgPt
    
    # Tabulate the auto and light truck hydrocarbon CO2e emissions by vehicle age
    VehType. <- unlist( SynPop..$VehType )
    VehAge. <- unlist( SynPop..$VehAge )
    HcVehDvmt. <- unlist( SynPop..$HcVehDvmt )
    HcVehCo2e. <- Hh_$AveFuelCo2e.[ VehType. ] * ( HcVehDvmt. / unlist( SynPop..$VehMpg ) )
    HcVehDvmt.2d <- tapply( HcVehDvmt., list( VehAge., VehType. ), function(x) sum(x, na.rm=TRUE) )
    HcVehDvmt.AgVt <- array( 0, dim=c(length(Ag),length(Vt)), dimnames=list(Ag,Vt) )
    HcVehDvmt.AgVt[ dimnames( HcVehDvmt.2d )[[1]], dimnames( HcVehDvmt.2d )[[2]] ] <- HcVehDvmt.2d
    HcVehDvmt.AgVt[is.na(HcVehDvmt.AgVt)] <- 0
    HcVehCo2e.2d <- tapply( HcVehCo2e., list( VehAge., VehType. ), function(x) sum(x, na.rm=TRUE) )
    HcVehCo2e.AgVt <- array( 0, dim=c(length(Ag),length(Vt)), dimnames=list(Ag,Vt) )
    HcVehCo2e.AgVt[ dimnames( HcVehCo2e.2d )[[1]], dimnames( HcVehCo2e.2d )[[2]] ] <- HcVehCo2e.2d
    HcVehCo2e.AgVt[is.na(HcVehCo2e.AgVt)] <- 0
    Hh_$HcVehDvmt.CoAgVt[ co, , ] <<- HcVehDvmt.AgVt
    Hh_$HcVehCo2e.CoAgVt[ co, , ] <<- HcVehCo2e.AgVt
    #Hh_$HcVehDvmt.CoAgVt[ co, , ] <- HcVehDvmt.AgVt
    #Hh_$HcVehCo2e.CoAgVt[ co, , ] <- HcVehCo2e.AgVt
    
    # Tabulate the auto and light truck electric vehicle CO2e emissions by vehicle age
    EvVehDvmt. <- unlist( SynPop..$EvVehDvmt )
    EvVehCo2e. <- Hh_$AveElectricCo2e.Co[co] * ( EvVehDvmt. / unlist( SynPop..$VehMpkwh ) )
    EvVehDvmt.2d <- tapply( EvVehDvmt., list( VehAge., VehType. ), function(x) sum(x, na.rm=TRUE) )
    EvVehDvmt.AgVt <- array( 0, dim=c(length(Ag),length(Vt)), dimnames=list(Ag,Vt) )
    EvVehDvmt.AgVt[ dimnames( EvVehDvmt.2d )[[1]], dimnames( EvVehDvmt.2d )[[2]] ] <- EvVehDvmt.2d
    EvVehDvmt.AgVt[is.na(EvVehDvmt.AgVt)] <- 0
    EvVehCo2e.2d <- tapply( EvVehCo2e., list( VehAge., VehType. ), function(x) sum(x, na.rm=TRUE) )
    EvVehCo2e.AgVt <- array( 0, dim=c(length(Ag),length(Vt)), dimnames=list(Ag,Vt) )
    EvVehCo2e.AgVt[ dimnames( EvVehCo2e.2d )[[1]], dimnames( EvVehCo2e.2d )[[2]] ] <- EvVehCo2e.2d
    EvVehCo2e.AgVt[is.na(EvVehCo2e.AgVt)] <- 0
    Hh_$EvVehDvmt.CoAgVt[ co, , ] <<- EvVehDvmt.AgVt
    Hh_$EvVehCo2e.CoAgVt[ co, , ] <<- EvVehCo2e.AgVt
    #Hh_$EvVehDvmt.CoAgVt[ co, , ] <- EvVehDvmt.AgVt
    #Hh_$EvVehCo2e.CoAgVt[ co, , ] <- EvVehCo2e.AgVt
    
    # Close local function
  } ) 
  
  gc()
  
  # Close loop through counties 	
  }
  
  #Save the results
  #================
  
  Filename <- paste( OutputYearDir, "/", "Hh_.RData", sep="" )
  save( Hh_, file=Filename )
  Filename <- paste( OutputYearDir, "/", "Inputs_.RData", sep="" )
  save( Inputs_, file=Filename )
  Filename <- paste( OutputYearDir, "/", "Model_.RData", sep="" )
  save( Model_, file=Filename )
  Filename <- paste( OutputYearDir, "/", "Metropolitan_.RData", sep="" )
  save( Metropolitan_, file=Filename )
  
  #End the loop through years
  #==========================
  
  print( Sys.time() )
  
}
rm( Hh_ )


