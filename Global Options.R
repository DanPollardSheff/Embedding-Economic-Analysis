#    Embedding RCT Health Economic Analysis using the Sheffield Type 2 Diabetes Treatment Model - version 3
#    Copyright (C) 2023  Pollard, Pidd, Breeze, Brennan, Thomas

#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    any later version.

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

#    You should have received a copy of the GNU General Public License along
#    with this program; if not, write to the Free Software Foundation, Inc.,
#    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

#    Contact person: Dan Pollard, Email: d.j.pollard@sheffield.ac.uk, 
#    Address: Regent Court, 30 Regent Court, Sheffield, United Kingdom, S1 4DA


#List of the global variables
glo_vars <- c("n","run_psa","psa_count", "disc_rate_costs", "disc_rate_QALYs", "disc_rate_LYs",
              "Population", "treatment", "Results_output", "KMmethod", "Number of cores",
              "Treatment effect duration", "HbA1c Scenario")

##get the length of glo_vars, for checking below
length(glo_vars)

#create the global varaibles matrix to store the global varaibles
GlobalVars <- matrix(data=NA, nrow = length(glo_vars), ncol = 2)
#update the row names to be the list of global variables above
rownames(GlobalVars) <- glo_vars
colnames(GlobalVars) <- c("Value", "Description")

#drop the glo_vars variable
rm(glo_vars)

GlobalVars["n", "Value"] <- 5000
GlobalVars["n", "Description"] <- "The number of patients to run through the model"

GlobalVars["run_psa", "Value"] <- T
GlobalVars["run_psa", "Description"] <- "T = run psa, F = run deterministic"

GlobalVars["psa_count", "Value"] <- 2000
#Stop the model here if there is a discrepancy in the model mode and declared number 
#of PSA runs
if(is.numeric(GlobalVars["psa_count", "Value"])==T&GlobalVars["run_psa", "Value"]==F|
   is.na(GlobalVars["psa_count", "Value"])==T&GlobalVars["run_psa", "Value"]==T){
  stop("discrepancy between the psa mode and the number of psa runs")  
}
GlobalVars["psa_count", "Description"] <- "number of PSA runs. set to NA for deterministic runs"

GlobalVars["disc_rate_costs", "Value"] <- 0.035
GlobalVars["disc_rate_costs", "Description"] <- "discount rate for costs, writen as a numeric number"

GlobalVars["disc_rate_QALYs", "Value"] <- 0.035
GlobalVars["disc_rate_QALYs", "Description"] <- "discount rate for QALYs, writen as a numeric number"

GlobalVars["disc_rate_LYs", "Value"] <- 0.035
GlobalVars["disc_rate_LYs", "Description"] <- "discount rate for QALYs, writen as a numeric number"

GlobalVars["treatment", "Value"] <- "baseline"
GlobalVars["treatment", "Description"] <- "text term for the treatment. Cross reference that this is called correctly
in your code for creating an interventions effect matrix (should be in R/Interventions.R)"


GlobalVars["Results_output", "Value"] <- "Summary"
GlobalVars["Results_output", "Description"] <- "text term to produce different output types. Default is summary for PSA,
set to Patient Level to get the patient matrix. If not specfied, the determinisitc model will return a detailed year by year summary"

#Checks, Time_to_Event and Detailed modes are only run in a deterministic model
if(GlobalVars["run_psa", "Value"]==T&GlobalVars["Results_output", "Value"]=="Time_to_Event"|
   GlobalVars["run_psa", "Value"]==T&GlobalVars["Results_output", "Value"]=="Detailed"){
  stop("model outputs are not compatible with outputs from a PSA")  
}


GlobalVars["KMmethod", "Value"] <- "Events"
GlobalVars["KMmethod", "Description"] <- "Text term to indicate the method for calculating a Kaplan-Meier curve from
the simulation. Options are Events or Probabilities. Events uses the events in the model, probabilities uses the estimated probabilities.
Default is Events."

GlobalVars["Number of cores", "Value"] <- 100
GlobalVars["Number of cores", "Description"] <- "Number to declare the maximum number of cores to use when processing PSA results. Use this to decrease memory usage."

GlobalVars["Treatment effect duration", "Value"] <- 3
GlobalVars["Treatment effect duration", "Description"] <- "Number to declare how long the intervention effect will last."

GlobalVars["HbA1c Scenario", "Value"] <- "Base Case"
GlobalVars["HbA1c Scenario", "Description"] <- "This is a control varaible to determine 
which values are used for the HbA1c subroup value is used for the one year HbA1c effect when direct 
biomedical effects are used. Options are:
PopulationWhite, PopulationEthnicMinority, CompleteCase, EducationAttenders, 
BaselineA1cAbove47.5, RecruitedBeforeFeb2020. Default is the main trial result
so if none of these options are correctly specified the model will default to 
this value"
