#========================
#GreenSTEP_Hh_Synthesis.r
#========================
#Copyright 2009 - 2016, Oregon Department of Transportation
#Licensed under the Apache License, Version 2.0 (the "License"); you may not use
#this file except in compliance with the License. You may obtain a copy of the
#License at
#http://www.apache.org/licenses/LICENSE-2.0
#Unless required by applicable law or agreed to in writing, software distributed
#under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
#CONDITIONS OF ANY KIND, either express or implied.  See the License for the
#specific language governing permissions and limitations under the License.
#Version: 3.5
#Date: 12/5/16

#Description
#===========
#This script takes population projections by age cohort, county and year and
#makes synthetic households that are used in the GreenSTEP model. The attributes
#of the synthetic households include the number of persons by age category. It
#then adds other household attributes including household income, development
#type (metropolitan, town, rural), population density of the neighborhood where
#the household is located, urban character of the neighborhood, driver age
#population, and whether the household has only elderly persons. The synthetic
#household files are saved in the SynHsld folder. Synthetic households will not
#be created if they already exist.

#Create synthetic households for each forecast year and county
#=============================================================
local({
  # Create a directory to store the created households if one does not exist
  HsldDir <- paste(ModelDir, "SynHsld", sep = "/")
  if (!file.exists(HsldDir))
    dir.create(HsldDir)
  # Run rest of script if synthetic households have not already been created
  # Check is based on whether directory has been created
  YearDir <- paste(HsldDir, "/Year", yr, sep = "")
  if (file.exists(YearDir)) {
    print(
      paste(
        "The directory '",
        YearDir,
        "' already exists. Stopping synthetic household generation for ",
        yr,
        ".",
        sep = ""
      )
    )
  } else {
    # Create directory to store results
    dir.create(YearDir)
    
    #Set a random seed to make run replicable
    #========================================
    #If the UsedSavedRandomSeed input parameter is TRUE a saved random seed will be retrieved
    #Otherwise a new random seed will be set and saved
    RandomSeedFile <-
      paste(ModelDir, "/RandomSeedValue", yr, ".RData", sep = "")
    if (UseSavedRandomSeed & file.exists(RandomSeedFile)) {
      RandomSeedValue <- assignLoad(RandomSeedFile)
    } else {
      RandomSeedValue <- sample(1:1000000, 1)
      save(RandomSeedValue, file = RandomSeedFile)
    }
    #Set random seed for model run
    set.seed(RandomSeedValue)
    
    #Create synthetic households for each county
    #===========================================
    # Load the population file for the year and put in order
    PopFileName <-
      paste(ModelDir,
            "/pop_forecasts/",
            "pop_by_age_",
            yr,
            ".csv",
            sep = "")
    Pop..CoAp <- read.csv(PopFileName, row.names = 1)[Co, Ap]
    # Create a list to hold the results where each component of the list
    # is a matrix of households (rows) with number of people by age group (columns) for a county
    Hsld_Co.HhAp <- list()
    # Create the synthetic households for each county
    for (co in Co) {
      # Create households for the county
      Hsld_Co.HhAp[[co]] <-
        createHhByAge(unlist(Pop..CoAp[co,]), HtProb.HtAp)[[1]]
    }
    # Save the county synthetic population outputs
    Filename <- paste(YearDir, "/Hsld_Co.HhAp.RData", sep = "")
    save(Hsld_Co.HhAp, file = Filename, compress = TRUE)
    

    #Exit else
  }
  
  #Exit local function
})
