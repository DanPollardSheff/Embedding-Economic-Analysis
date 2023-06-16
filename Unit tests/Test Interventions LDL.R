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
parameters_ <- parameter[2,]
endtime_ <- 50
treatment_ <- "Embedding_TrialEffect_All"

LDL1 <- initialise_intervention_dt_LDL(length(population_[,"ID"]),
                                       treatment_,
                                       parameters_,
                                       endtime_,
                                       GlobalVars_)
LDL1

GlobalVars_["Treatment effect duration","Value"] <- 3

LDL2 <- initialise_intervention_dt_LDL(length(population_[,"ID"]),
                                       treatment_,
                                       parameters_,
                                       endtime_,
                                       GlobalVars_)
LDL2

GlobalVars_["Treatment effect duration","Value"] <- 5

LDL3 <- initialise_intervention_dt_LDL(length(population_[,"ID"]),
                                       treatment_,
                                       parameters_,
                                       endtime_,
                                       GlobalVars_)
LDL3

treatment_ <- "Embedding_TrialEffect_All_1yr"

GlobalVars_["Treatment effect duration","Value"] <- 10
LDL4 <- initialise_intervention_dt_LDL(length(population_[,"ID"]),
                                       treatment_,
                                       parameters_,
                                       endtime_,
                                       GlobalVars_)
LDL4

GlobalVars_["Treatment effect duration","Value"] <- 3
LDL5 <- initialise_intervention_dt_LDL(length(population_[,"ID"]),
                                       treatment_,
                                       parameters_,
                                       endtime_,
                                       GlobalVars_)
LDL5

GlobalVars_["Treatment effect duration","Value"] <- 5
LDL6 <- initialise_intervention_dt_LDL(length(population_[,"ID"]),
                                       treatment_,
                                       parameters_,
                                       endtime_,
                                       GlobalVars_)
LDL6

treatment_ <- "Embedding_TrialEffect_PriandSS"
GlobalVars_["Treatment effect duration","Value"] <- 10

LDL7 <- initialise_intervention_dt_LDL(length(population_[,"ID"]),
                                       treatment_,
                                       parameters_,
                                       endtime_,
                                       GlobalVars_)
LDL7

GlobalVars_["Treatment effect duration","Value"] <- 3
LDL8 <- initialise_intervention_dt_LDL(length(population_[,"ID"]),
                                       treatment_,
                                       parameters_,
                                       endtime_,
                                       GlobalVars_)
LDL8

GlobalVars_["Treatment effect duration","Value"] <- 5
LDL9 <- initialise_intervention_dt_LDL(length(population_[,"ID"]),
                                       treatment_,
                                       parameters_,
                                       endtime_,
                                       GlobalVars_)
LDL9

###SE attendance

treatment_ <- "Embedding_MetaAnalysis_All"
GlobalVars_["Treatment effect duration","Value"] <- 101

attend_se <- initialise_intervention_dt_attendse (length(population_[,"ID"]), 
                                                  treatment_,
                                                  parameters_)

LDL10 <- initialise_intervention_dt_LDL(length(population_[,"ID"]),
                                        treatment_,
                                        parameters_,
                                        endtime_,
                                        GlobalVars_,
                                        attend_se)
LDL10

GlobalVars_["Treatment effect duration","Value"] <- 15

LDL11 <- initialise_intervention_dt_LDL(length(population_[,"ID"]),
                                        treatment_,
                                        parameters_,
                                        endtime_,
                                        GlobalVars_,
                                        attend_se)
LDL11

GlobalVars_["Treatment effect duration","Value"] <- 10

LDL12 <- initialise_intervention_dt_LDL(length(population_[,"ID"]),
                                        treatment_,
                                        parameters_,
                                        endtime_,
                                        GlobalVars_,
                                        attend_se)
LDL12

treatment_ <- "Control_MetaAnalysis_all"
GlobalVars_["Treatment effect duration","Value"] <- 101

attend_se <- initialise_intervention_dt_attendse (length(population_[,"ID"]), 
                                                  treatment_,
                                                  parameters_)

LDL13 <- initialise_intervention_dt_LDL(length(population_[,"ID"]),
                                        treatment_,
                                        parameters_,
                                        endtime_,
                                        GlobalVars_,
                                        attend_se)
LDL13

GlobalVars_["Treatment effect duration","Value"] <- 15

LDL14 <- initialise_intervention_dt_LDL(length(population_[,"ID"]),
                                        treatment_,
                                        parameters_,
                                        endtime_,
                                        GlobalVars_,
                                        attend_se)
LDL14

GlobalVars_["Treatment effect duration","Value"] <- 10

LDL15 <- initialise_intervention_dt_LDL(length(population_[,"ID"]),
                                        treatment_,
                                        parameters_,
                                        endtime_,
                                        GlobalVars_,
                                        attend_se)
LDL15


treatment_ <- "Embedding_MetaAnalysis_1yr"
GlobalVars_["Treatment effect duration","Value"] <- 101

attend_se <- initialise_intervention_dt_attendse (length(population_[,"ID"]), 
                                                  treatment_,
                                                  parameters_)

LDL16 <- initialise_intervention_dt_LDL(length(population_[,"ID"]),
                                        treatment_,
                                        parameters_,
                                        endtime_,
                                        GlobalVars_,
                                        attend_se)
LDL16

GlobalVars_["Treatment effect duration","Value"] <- 15

LDL17 <- initialise_intervention_dt_LDL(length(population_[,"ID"]),
                                        treatment_,
                                        parameters_,
                                        endtime_,
                                        GlobalVars_,
                                        attend_se)
LDL17

GlobalVars_["Treatment effect duration","Value"] <- 10

LDL18 <- initialise_intervention_dt_LDL(length(population_[,"ID"]),
                                        treatment_,
                                        parameters_,
                                        endtime_,
                                        GlobalVars_,
                                        attend_se)
LDL18