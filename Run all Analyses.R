#    Embedding RCT Health Economic Analysis using the Sheffield Type 2 Diabetes Treatment Model - version 3
#    Copyright (C) 2023   Pollard, Pidd, Breeze, Brennan, Thomas

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

#Change the treatment effect duration back to the base case value
GlobalVars["Treatment effect duration", "Value"] <- 3

#Scenario - White population subgroup

##Create a true false variable for whether the population is correct
inpop <- population_clean[,"AFRO"]==0&population_clean[,"INDIAN"]==0
#Subgroup the population
population_clean_white <- population_clean[inpop,]
#Change the number of patients
GlobalVars["n","Value"] <- length(population_clean_white[,"ID"])
#Set the A1c scenario
GlobalVars["HbA1c Scenario", "Value"] <- "PopulationWhite"

set.seed(1)
start.time <- Sys.time()

results1 <- run_model(population_clean_white, 
                      parameter, 
                      50, 
                      "Embedding_TrialEffect_All", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)
end.time <- Sys.time()
end.time - start.time

write.csv(results1, "Results/Whitesubgroup_embedding.csv")
set.seed(1)
results2 <- run_model(population_clean_white, 
                      parameter, 
                      50, 
                      "baseline", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)

write.csv(results2, "Results/Whitesubgroup_control.csv")

rm(population_clean_white,inpop)

#Scenario - ethnic minority subgroup

##Create a true false variable for whether the population is correct
inpop <- population_clean[,"AFRO"]==1|population_clean[,"INDIAN"]==1
#Subgroup the population
population_clean_ethmin <- population_clean[inpop,]
#Change the number of patients
GlobalVars["n","Value"] <- length(population_clean_ethmin[,"ID"])
#Set the A1c scenario
GlobalVars["HbA1c Scenario", "Value"] <- "PopulationEthnicMinority"

set.seed(1)
start.time <- Sys.time()

results1 <- run_model(population_clean_ethmin, 
                      parameter, 
                      50, 
                      "Embedding_TrialEffect_All", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)
end.time <- Sys.time()
end.time - start.time

write.csv(results1, "Results/EthnicMinoritysubgroup_embedding.csv")
set.seed(1)
results2 <- run_model(population_clean_ethmin, 
                      parameter, 
                      50, 
                      "baseline", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)

write.csv(results2, "Results/EthnicMinoritysubgroup_control.csv")

rm(population_clean_ethmin,inpop)
#Reset the population back to the default value
GlobalVars["n","Value"] <- length(population_clean[,"ID"])

#Complete Case Subgroup
GlobalVars["HbA1c Scenario", "Value"] <- "CompleteCase"


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

write.csv(results1, "Results/completecase_embedding.csv")

#Education Attenders Subgroup
GlobalVars["HbA1c Scenario", "Value"] <- "EducationAttenders"


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

write.csv(results1, "Results/educattenders_embedding.csv")

#Recruited before Feb 2020
GlobalVars["HbA1c Scenario", "Value"] <- "RecruitedBeforeFeb2020"


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

write.csv(results1, "Results/precovid_embedding.csv")

#Baseline HbA1c above 6.5%
inpop <- population_clean[,"HBA"] >= 6.5
population_clean_A1c6.5above <- population_clean[inpop,]
GlobalVars["n",  "Value"] <- length( population_clean_A1c6.5above[,"ID"])
GlobalVars["HbA1c Scenario", "Value"] <- "BaselineA1cAbove47.5"

set.seed(1)
start.time <- Sys.time()

results1 <- run_model(population_clean_A1c6.5above, 
                      parameter, 
                      50, 
                      "Embedding_TrialEffect_All", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)
end.time <- Sys.time()
end.time - start.time

write.csv(results1, "Results/blA1c_over47.5_embedding.csv")
set.seed(1)
results2 <- run_model(population_clean_A1c6.5above, 
                      parameter, 
                      50, 
                      "baseline", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)

write.csv(results2, "Results/blA1c_over47.5_control.csv")

rm(population_clean_A1c6.5above, inpop)

##Education type subgroup analysis
##Desmond
GlobalVars["HbA1c Scenario", "Value"] <- "DESMOND"

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
write.csv(results1, "Results/Embedding_DesmondSubgroup.csv")

#Diabetes 2gether / Diabetes 4ward
GlobalVars["HbA1c Scenario", "Value"] <- "Diabetes 2gether"

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

write.csv(results1, "Results/Embedding_Diabetes2getherSubgroup.csv")

#Spotlight
GlobalVars["HbA1c Scenario", "Value"] <- "Diabetes 2gether"

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

write.csv(results1, "Results/Embedding_SpotlightSubgroup.csv")



#Xpert Health
GlobalVars["HbA1c Scenario", "Value"] <- "Xpert Health"

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
write.csv(results1, "Results/Embedding_XpertHealthSubgroup.csv")
