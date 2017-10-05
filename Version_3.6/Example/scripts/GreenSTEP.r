#===========
#GreenSTEP.r
#===========
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
#This is the main script for running the GreenSTEP Model. This script sets up
#run parameters and calls modules which implement various portions of the
#GreenSTEP model. These modules include:
#1) The GreenSTEP_Inputs.r module loads all of the model objects and data needed
#to run the GreenSTEP model.
#2) The "GreenSTEP_Hh_Synthesis.r" module generates synthetic households for
#each county and year from population forecasts of persons by age group. The
#synthetic households have characteristics of numbers of persons by each of six
#age groups, household income, development type, density,. These are saved as
#RData files in a folder named 'SynHsld' in the the model folder. This module is
#run only if it has not been run before because the same populations should be
#used for all scenarios to reduce stochastic effects on the results.
#3) The GreenSTEP_Sim.r module performs all of the household microsimulation
#calculations for determining vehicle ownership, household travel and vehicle
#characteristics, use, and emissions.
#4) The GreenSTEP_Sim_Outputs.r module computes summary output tables from the
#GreenSTEP_Sim results.

#Read in parameters that are unique to the model run
#===================================================
source("run_parameters.txt")

#Identify directory locations for model, inputs, etc.
#====================================================
# Make a list to store the directory references
Dir_ <- list()
# Directory references are made with respect to the run directory
Dir_$RunDir <- getwd()
# The scenario inputs directory location
Dir_$InputDir <- "scenario/inputs"
# The scenario outputs directory location
Dir_$OutputDir <- "scenario/outputs"
# Directory containing model objects and inputs
Dir_$ModelDir <- "model"
# Directory where model scripts are located
Dir_$ScriptDir <- "scripts"
attach(Dir_)

#Define function to load an RData object to an object name
#=========================================================
assignLoad <- function(filename) {
  load(filename)
  get(ls()[ls() != "filename"])
}

#Run the GreenSTEP_Inputs.r script
#=================================
#The GreenSTEP_Inputs.r script loads all of the data objects needed to run the
#model.
source(paste(ScriptDir, "/GreenSTEP_Inputs.r", sep = ""))


#Run the GreenSTEP_Hh_Synthesis.r script if necessary
#====================================================
for (yr in RunYears) {
  HsldFile <- paste(ModelDir, "/Hsld", yr, ".RData", sep = "")
  if (!file.exists(HsldFile)) {
    source(paste0(ScriptDir, "/GreenSTEP_Hh_Synthesis.r"))
  }
}
rm(HsldFile)

#Run the GreenSTEP_Sim.r script
#==============================
if (!OnlySynthesizeHh) {
  source(paste(ScriptDir, "/GreenSTEP_Sim.r", sep = ""))
}

#Run the GreenSTEP_Sim_Outputs.r script
#======================================
if (!OnlySynthesizeHh) {
  source(paste(ScriptDir, "/GreenSTEP_Sim_Outputs.r", sep = ""))
}

#Run the summary measures script
#===============================
# if (!OnlySynthesizeHh) {
#   source(paste(ScriptDir, "/calc_summary_measures.r", sep=""))
# }
  

