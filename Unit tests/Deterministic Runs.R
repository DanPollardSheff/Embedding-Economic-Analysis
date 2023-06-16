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

write.csv(results1, "Results/basecase_embedding_det.csv")
set.seed(1)
results2 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "baseline", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)

write.csv(results2, "Results/basecase_control_det.csv")

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

#write.csv(results1, "Results/15%disc_embedding_det.csv")
set.seed(1)
results2 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "baseline", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)

#write.csv(results2, "Results/15%disc_control_det.csv")


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
#write.csv(results1, "Results/StatSig_Outcomes_embedding_det.csv")

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
#write.csv(results1, "Results/1year_trial_outcomes_embedding_det.csv")

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

#write.csv(results1, "Results/MA_embedding_det.csv")
set.seed(1)
results2 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "Control_MetaAnalysis_all", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)

#write.csv(results2, "Results/MA_control_det.csv")

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

#write.csv(results1, "Results/MA_1yr_embedding_det.csv")

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

#write.csv(results1, "Results/5yrduration_embedding_det.csv")

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

#write.csv(results1, "Results/10yrduration_embedding_det.csv")

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

#write.csv(results1, "Results/MA_15yr_embedding_det.csv")
set.seed(1)
results2 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "Control_MetaAnalysis_all", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)

#write.csv(results2, "Results/MA_15yr_control.csv")

#Scenario: Meta Analysis & SSME lifetime duration
#Change the GLobal option to a lifetime duration
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

#write.csv(results1, "Results/MA_lifeitme_embedding_det.csv")
set.seed(1)
results2 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "Control_MetaAnalysis_all", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)

#write.csv(results2, "Results/MA_lifetime_control_det.csv")