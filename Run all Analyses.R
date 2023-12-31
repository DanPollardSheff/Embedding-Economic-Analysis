#    Embedding RCT Health Economic Analysis using the Sheffield Type 2 Diabetes Treatment Model - version 3
#    Copyright (C) 2023   Pollard, Pidd, Breeze, Brennan, Thomas

#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

#    You should have received a copy of the GNU General Public License along
#    with this program; if not, write to the Free Software Foundation, Inc.,
#    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

#    Contact person: Dan Pollard, Email: d.j.pollard@sheffield.ac.uk, 
#    Address: Regent Court, 30 Regent Court, Sheffield, United Kingdom, S1 4DA


library(MASS)
library(VGAM)
library(doParallel)
library(parallel)
library(dplyr)
set.seed(429)

#Create the Global options matrix
source("Global Options.R")
#Load in population and data files
source("all_model_files.R")


####build the population
population <-read.csv("Populations/POPULATION.csv")
population_clean <- build_population(population, PopulationVariables, GlobalVars)
#Generate an array of common random numbers for each patient for each event for each year
random_numbers <- generate_random(length(population_clean[,"ID"])) 

#Base Case analysis - Trial effects and default treatment effect duration
set.seed(1)
start.time <- Sys.time()

results1 <- run_model(population_clean, 
                            parameter, 
                            50, 
                            "Embedding_TrialEffect_All", 
                            GlobalVars,
                            random_numbers,
                            LifeTables)
end.time <- Sys.time()
end.time - start.time

write.csv(results1, "Results/basecase_embedding.csv")
set.seed(1)
results2 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "baseline", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)

write.csv(results2, "Results/basecase_control.csv")

###set discount rate to 1.5%
GlobalVars["disc_rate_costs", "Value"] <- 0.015
GlobalVars["disc_rate_QALYs", "Value"] <- 0.015
GlobalVars["disc_rate_LYs", "Value"] <- 0.015


#Run models
set.seed(1)
start.time <- Sys.time()

results1 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "Embedding_TrialEffect_All", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)
end.time <- Sys.time()
end.time - start.time

write.csv(results1, "Results/15%disc_embedding.csv")
set.seed(1)
results2 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "baseline", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)

write.csv(results2, "Results/15%disc_control.csv")


#Reset back to 3.5%
GlobalVars["disc_rate_costs", "Value"] <- 0.035
GlobalVars["disc_rate_QALYs", "Value"] <- 0.035
GlobalVars["disc_rate_LYs", "Value"] <- 0.035

####Scenario: No statistically insignificant outcomes
#Run models
set.seed(1)
start.time <- Sys.time()

results1 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "Embedding_TrialEffect_PriandSS", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)
end.time <- Sys.time()
end.time - start.time
write.csv(results1, "Results/StatSig_Outcomes_embedding.csv")

#Use control arm from base case

####Scenario: One year step wedge only
#Run models
set.seed(1)
start.time <- Sys.time()

results1 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "Embedding_TrialEffect_All_1yr", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)
end.time <- Sys.time()
end.time - start.time
write.csv(results1, "Results/1year_trial_outcomes_embedding.csv")

#Use base case for control, only 1 year of data for control patients in study

#Scenario: Uptake of SSME & Meta Analysis 
set.seed(1)
start.time <- Sys.time()

results1 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "Embedding_MetaAnalysis_All", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)
end.time <- Sys.time()
end.time - start.time

write.csv(results1, "Results/MA_embedding.csv")
set.seed(1)
results2 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "Control_MetaAnalysis_all", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)

write.csv(results2, "Results/MA_control.csv")

#Scenario: Uptake of SSME from study, only 1 year data & Meta Analysis
set.seed(1)
start.time <- Sys.time()

results1 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "Embedding_MetaAnalysis_1yr", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)
end.time <- Sys.time()
end.time - start.time

write.csv(results1, "Results/MA_1yr_embedding.csv")

#No control, as this only has the 1 year of data for SSME uptake due to study
#design

#Scenario: Base Case & 5 year treatment effect
#Change the Global option to a 5 year duration
GlobalVars["Treatment effect duration", "Value"] <- 5

set.seed(1)
start.time <- Sys.time()

results1 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "Embedding_TrialEffect_All", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)
end.time <- Sys.time()
end.time - start.time

write.csv(results1, "Results/5yrduration_embedding.csv")

#Scenario: 10 year treatment effect duration
#Change the global option to a 10 year duration
GlobalVars["Treatment effect duration", "Value"] <- 10

set.seed(1)
start.time <- Sys.time()

results1 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "Embedding_TrialEffect_All", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)
end.time <- Sys.time()
end.time - start.time

write.csv(results1, "Results/10yrduration_embedding.csv")

#Scenario: Meta Analysis & SSME 15 year duration (10 years full benefit)
#Change the global otpion to a 15 year duration
GlobalVars["Treatment effect duration", "Value"] <- 15

set.seed(1)
start.time <- Sys.time()

results1 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "Embedding_MetaAnalysis_All", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)
end.time <- Sys.time()
end.time - start.time

write.csv(results1, "Results/MA_15yr_embedding_Det.csv")
set.seed(1)
results2 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "Control_MetaAnalysis_all", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)

write.csv(results2, "Results/MA_15yr_control_det.csv")

#Scenario: Meta Analysis & SSME lifetime duration
#Change the GLobal option to a lifetime duration
GlobalVars["Treatment effect duration", "Value"] <- 101

set.seed(1)
start.time <- Sys.time()

results1 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "Embedding_MetaAnalysis_All", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)
end.time <- Sys.time()
end.time - start.time

write.csv(results1, "Results/MA_lifeitme_embedding.csv")
set.seed(1)
results2 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "Control_MetaAnalysis_all", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)

write.csv(results2, "Results/MA_lifetime_control.csv")