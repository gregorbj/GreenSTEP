#calc_summary_measures.r
#Brian Gregor
#12/18/2016

#Purpose
#-------
#This script calculates a number of statewide and county level measures from one or more GreenSTEP scenarios.  The results are put into a table that is saved as a CSV formatted text file where the rows correspond to measures and the columns correspond to scenarios and years.  The statewide measures to be calculated are specified in a text file whose format is described below.  This enables the output measures to be revised without revising the code in this script.  New measures can be added as long as the data needed to calculate the measures is contained in the standard objects saved as part of a GreenSTEP run.  Standard R expressions are used to specify the calculations.  In a similar fashion, an input file specifies the calculation of county-level measures.  However, because counties add another dimension to the table, the results are flattened out in the final output table so that the rows correspond to the combination of a measure and county.

#Revisions
#---------
#2/4/2016: The "calcAreaMeasures" function was revised to enable the specification of temporary variable. These variables are calculated and added to the "Summary_" list just like any other performance measures. However, they are removed from list before the area performance measure table is created. The names of temporary variables must be prefixed by "Temp_", "temp_", or "TEMP_".
#12/18/2016: Script modified to work with the GreenSTEP model

#Define function to calculate the areawide summary measures for a scenario
#-------------------------------------------------------------------------
#Function Name:
#calcAreaMeasures
#Function Arguments:
#ScenarioPath - Path name of the directory for the scenario to be analyzed
#Year - Year to summarize
#CalcSpecFile - The path name of a text file in comma separated values format containing the specifications of the measures, calculations to be completed, and units of the measure.  Each line has 3 entries.  The first line of the file is the header and has the following entries: Measure, Calculation, Units.  Each successive line has entries for the name of the measure, the formula for calculating the measure from GreenSTEP inputs and outputs that are contained in any of three R objects stored in the GreenSTEP_ outputs directory for a scenario and year, and documentation of the units of the measure.
#Function Return Value:
#The function returns a named list.  The first component of the list (Measures) is a named vector containing the values computed for each measure.  The names are the measure names.  The second component of the list (Units) is a named vector containing the units descriptions of each measure.  The names are the measure names.

calcStateMeasures <- function(Year) {
  # Load the data
  Path <- paste0("scenario", "/outputs/", "Year", Year)
  Objs. <-
    c("Hh_", "CommServ_", "Metropolitan_", "Model_", "Inputs_")
  Files. <- paste(Objs., ".RData", sep = "")
  for (File in Files.) {
    load(paste(Path, File, sep = "/"))
  }
  
  # Attach the data
  attach(Hh_)
  attach(CommServ_)
  attach(Metropolitan_)
  attach(Model_)
  attach(Inputs_)
  
  # Load the measures calculation specification
  CalcSpecs.. <-
    read.csv("scripts/state_measures_spec.csv", as.is = TRUE)
  
  # Iterate through specifications and do the calculations
  Summary_ <- list()
  for (i in 1:nrow(CalcSpecs..)) {
    MeasureName <- CalcSpecs..[i, "Measure"]
    CalcSpec <- CalcSpecs..[i, "Calculation"]
    Summary_[[MeasureName]] <- try(eval(parse(text = CalcSpec)))
  }
  
  # Remove temporary measures
  #i.e. measures whose names start with Temp_, temp_, or TEMP_
  MeasureNames_ <- strsplit(names(Summary_), "_")
  IsTempMeasure. <- unlist(lapply(MeasureNames_, function(x) {
    x[1] %in% c("Temp", "temp", "TEMP")
  }))
  Summary_ <- Summary_[!IsTempMeasure.]
  
  # Make into vector and clean out superfluous names
  Summary. <- sapply(Summary_, function(x) {
    Y <- x
    names(Y) <- NULL
    Y
  })
  
  # Make a named vector of the units
  Units. <- CalcSpecs..$Units[!IsTempMeasure.]
  names(Units.) <- CalcSpecs..$Measure[!IsTempMeasure.]
  
  # Function exit procedure
  on.exit(detach(Hh_))
  on.exit(detach(CommServ_), add = TRUE)
  on.exit(detach(Metropolitan_), add = TRUE)
  on.exit(detach(Model_), add = TRUE)
  on.exit(detach(Inputs_), add = TRUE)
  
  # Return the results
  list(Measures = Summary., Units = Units.)
}


#Define function to calculate the county summary measures for a scenario
#-----------------------------------------------------------------------
#Function Name:
#calcCountyMeasures
#Function Arguments:
#ScenarioPath - Path name of the directory for the scenario to be analyzed
#Year - Year to summarize
#CalcSpecFile - The path name of a text file in comma separated values format containing the specifications of the measures, calculations to be completed, and units of the measure.  Each line has 3 entries.  The first line of the file is the header and has the following entries: Measure, Calculation, Units.  Each successive line has entries for the name of the measure, the formula for calculating the measure from GreenSTEP inputs and outputs that are contained in any of three R objects stored in the RSPM outputs directory for a scenario and year, and documentation of the units of the measure.
#Function Return Value:
#The function returns a named list.  The first component of the list (Measures) is a named vector containing the values computed for each measure and district.  The names are a concatenation of the measure and district names (measure.district).  The second component of the list (Units) is a named vector containing the units descriptions of each measure.  The names are a concatenation of the measure and district names (measure.district).

calcCountyMeasures <- function(Year) {
  # Load the data
  Path <- paste0("scenario", "/outputs/", "Year", Year)
  Objs. <-
    c("Hh_", "CommServ_", "Metropolitan_", "Model_", "Inputs_")
  Files. <- paste(Objs., ".RData", sep = "")
  for (File in Files.) {
    load(paste(Path, File, sep = "/"))
  }
  
  # Attach the data
  attach(Hh_)
  attach(CommServ_)
  attach(Metropolitan_)
  attach(Model_)
  attach(Inputs_)
  
  # Load the measures calculation specification
  CalcSpecs.. <-
    read.csv("scripts/county_measures_spec.csv", as.is = TRUE)
  
  # Determine the number of districts
  Counties <- rownames(Model_$CountyGroups..)
  
  # Iterate through specifications and do the calculations
  Summary_ <- list()
  for (i in 1:nrow(CalcSpecs..)) {
    MeasureName <- CalcSpecs..[i, "Measure"]
    CalcSpec <- CalcSpecs..[i, "Calculation"]
    Measure.Co <- eval(parse(text = CalcSpec))
    if (!all(names(Measure.Co) %in% Counties)) {
      ErrMsg <-
        paste("Result for measure",
              MeasureName,
              "does not correspond to counties")
      stop(ErrMsg)
    }
    Summary_[[MeasureName]] <- Measure.Co
  }
  
  # Make into vector concatenating measure and district names
  Summary. <- unlist(Summary_)
  
  # Make a named vector of the units
  Units. <- rep(CalcSpecs..$Units, each = length(Counties))
  names(Units.) <- names(Summary.)
  
  # Function exit procedure
  on.exit(detach(Hh_))
  on.exit(detach(CommServ_), add = TRUE)
  on.exit(detach(Metropolitan_), add = TRUE)
  on.exit(detach(Model_), add = TRUE)
  on.exit(detach(Inputs_), add = TRUE)
  
  # Return the results
  list(Measures = Summary., Units = Units.)
}


#Iterate through run years and calculate summary measures
#========================================================
local({
  StateResults_ <- list()
  CountyResults_ <- list()
  #Calculate summary measures for all years
  for (yr in RunYears) {
    StateResults_[[yr]] <- calcStateMeasures(yr)
    CountyResults_[[yr]] <- calcCountyMeasures(yr)
  }
  #Prepare and save State measures table
  StateResults.. <-
    data.frame(list(Measure = names(StateResults_[[1]]$Measures)))
  StateResults.. <-
    cbind(StateResults.., do.call(cbind, lapply(StateResults_, function(x)
      x$Measures)))
  StateResults..$Units <- StateResults_[[1]]$Units
  write.table(
    StateResults..,
    file = "scenario/outputs/summary_state_measures.csv",
    col.names = TRUE,
    row.names = FALSE,
    sep = ","
  )
  #Prepare and save County measures table
  CountyResults.. <-
    data.frame(list(Measure = names(CountyResults_[[1]]$Measures)))
  CountyResults.. <-
    cbind(CountyResults.., do.call(cbind, lapply(CountyResults_, function(x)
      x$Measures)))
  CountyResults..$Units <- CountyResults_[[1]]$Units
  write.table(
    CountyResults..,
    file = "scenario/outputs/summary_county_measures.csv",
    col.names = TRUE,
    row.names = FALSE,
    sep = ","
  )
})
