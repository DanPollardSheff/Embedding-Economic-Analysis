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
population <-read.csv("Populations/POPULATION1.csv")
population_ <- build_population(population, PopulationVariables, GlobalVars)
GlobalVars_ <- GlobalVars
parameters_ <- parameter[1,]
endtime_ <- 50
alive_ <- population_[,"ID"] > 0
year_ <- 0

treatment_ <- "Embedding_TrialEffect_All"

attend_se_ <- initialise_intervention_dt_attendse(length(population_[,"ID"]),
                                                  treatment_,
                                                  parameters_)


population2 <- Intervention_costs_Embedding(treatment_,
                                            parameters_,
                                            population_,
                                            attend_se_,
                                            year_,
                                            alive_)
population2[,"YearCOST"]

GlobalVars_["Treatment effect duration","Value"] <- 3

population2 <- Intervention_costs_Embedding(treatment_,
                                            parameters_,
                                            population_,
                                            attend_se_,
                                            year_,
                                            alive_)
population2[,"YearCOST"]

GlobalVars_["Treatment effect duration","Value"] <- 5

population2 <- Intervention_costs_Embedding(treatment_,
                                            parameters_,
                                            population_,
                                            attend_se_,
                                            year_,
                                            alive_)
population2[,"YearCOST"]

treatment_ <- "Embedding_TrialEffect_All_1yr"

GlobalVars_["Treatment effect duration","Value"] <- 10
population2 <- Intervention_costs_Embedding(treatment_,
                                            parameters_,
                                            population_,
                                            attend_se_,
                                            year_,
                                            alive_)
population2[,"YearCOST"]

GlobalVars_["Treatment effect duration","Value"] <- 3
population2 <- Intervention_costs_Embedding(treatment_,
                                            parameters_,
                                            population_,
                                            attend_se_,
                                            year_,
                                            alive_)
population2[,"YearCOST"]

GlobalVars_["Treatment effect duration","Value"] <- 5
population2 <- Intervention_costs_Embedding(treatment_,
                                            parameters_,
                                            population_,
                                            attend_se_,
                                            year_,
                                            alive_)
population2[,"YearCOST"]

treatment_ <- "Embedding_TrialEffect_PriandSS"
GlobalVars_["Treatment effect duration","Value"] <- 10

population2 <- Intervention_costs_Embedding(treatment_,
                                            parameters_,
                                            population_,
                                            attend_se_,
                                            year_,
                                            alive_)
population2[,"YearCOST"]

GlobalVars_["Treatment effect duration","Value"] <- 3
population2 <- Intervention_costs_Embedding(treatment_,
                                            parameters_,
                                            population_,
                                            attend_se_,
                                            year_,
                                            alive_)
population2[,"YearCOST"]

GlobalVars_["Treatment effect duration","Value"] <- 5
population2 <- Intervention_costs_Embedding(treatment_,
                                            parameters_,
                                            population_,
                                            attend_se_,
                                            year_,
                                            alive_)
population2[,"YearCOST"]

###SE attendance

treatment_ <- "Embedding_MetaAnalysis_All"
GlobalVars_["Treatment effect duration","Value"] <- 101


population2 <- Intervention_costs_Embedding(treatment_,
                                            parameters_,
                                            population_,
                                            attend_se_,
                                            year_,
                                            alive_)
population2[,"YearCOST"]

GlobalVars_["Treatment effect duration","Value"] <- 15

population2 <- Intervention_costs_Embedding(treatment_,
                                            parameters_,
                                            population_,
                                            attend_se_,
                                            year_,
                                            alive_)
population2[,"YearCOST"]


GlobalVars_["Treatment effect duration","Value"] <- 10

population2 <- Intervention_costs_Embedding(treatment_,
                                            parameters_,
                                            population_,
                                            attend_se_,
                                            year_,
                                            alive_)
population2[,"YearCOST"]


treatment_ <- "Control_MetaAnalysis_all"
GlobalVars_["Treatment effect duration","Value"] <- 101

population2 <- Intervention_costs_Embedding(treatment_,
                                            parameters_,
                                            population_,
                                            attend_se_,
                                            year_,
                                            alive_)
population2[,"YearCOST"]

GlobalVars_["Treatment effect duration","Value"] <- 15

population2 <- Intervention_costs_Embedding(treatment_,
                                            parameters_,
                                            population_,
                                            attend_se_,
                                            year_,
                                            alive_)
population2[,"YearCOST"]

GlobalVars_["Treatment effect duration","Value"] <- 10

population2 <- Intervention_costs_Embedding(treatment_,
                                            parameters_,
                                            population_,
                                            attend_se_,
                                            year_,
                                            alive_)
population2[,"YearCOST"]


treatment_ <- "Embedding_MetaAnalysis_1yr"
GlobalVars_["Treatment effect duration","Value"] <- 101

population2 <- Intervention_costs_Embedding(treatment_,
                                            parameters_,
                                            population_,
                                            attend_se_,
                                            year_,
                                            alive_)
population2[,"YearCOST"]

GlobalVars_["Treatment effect duration","Value"] <- 15

population2 <- Intervention_costs_Embedding(treatment_,
                                            parameters_,
                                            population_,
                                            attend_se_,
                                            year_,
                                            alive_)
population2[,"YearCOST"]

GlobalVars_["Treatment effect duration","Value"] <- 10

population2 <- Intervention_costs_Embedding(treatment_,
                                            parameters_,
                                            population_,
                                            attend_se_,
                                            year_,
                                            alive_)
population2[,"YearCOST"]