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
treatment_ <- "Embedding_TrialEffect_All"

HBA1 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                        treatment_,
                                        parameters_,
                                        endtime_,
                                        GlobalVars_)
HBA1

GlobalVars_["Treatment effect duration","Value"] <- 3

HBA2 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_)
HBA2

GlobalVars_["Treatment effect duration","Value"] <- 5

HBA3 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_)
HBA3

treatment_ <- "Embedding_TrialEffect_All_1yr"

GlobalVars_["Treatment effect duration","Value"] <- 10
HBA4 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_)
HBA4

GlobalVars_["Treatment effect duration","Value"] <- 3
HBA5 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_)
HBA5

GlobalVars_["Treatment effect duration","Value"] <- 5
HBA6 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_)
HBA6

treatment_ <- "Embedding_TrialEffect_PriandSS"
GlobalVars_["Treatment effect duration","Value"] <- 10

HBA7 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_)
HBA7

GlobalVars_["Treatment effect duration","Value"] <- 3
HBA8 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_)
HBA8

GlobalVars_["Treatment effect duration","Value"] <- 5
HBA9 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_)
HBA9

###SE attendance

treatment_ <- "Embedding_MetaAnalysis_All"
GlobalVars_["Treatment effect duration","Value"] <- 101

attend_se <- initialise_intervention_dt_attendse (length(population_[,"ID"]), 
                                                  treatment_,
                                                  parameters_)

HBA10 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_,
                                         attend_se)
HBA10

GlobalVars_["Treatment effect duration","Value"] <- 15

HBA11 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se)
HBA11

GlobalVars_["Treatment effect duration","Value"] <- 10

HBA12 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se)
HBA12

treatment_ <- "Control_MetaAnalysis_all"
GlobalVars_["Treatment effect duration","Value"] <- 101

attend_se <- initialise_intervention_dt_attendse (length(population_[,"ID"]), 
                                                  treatment_,
                                                  parameters_)

HBA13 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se)
HBA13

GlobalVars_["Treatment effect duration","Value"] <- 15

HBA14 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se)
HBA14

GlobalVars_["Treatment effect duration","Value"] <- 10

HBA15 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se)
HBA15


treatment_ <- "Embedding_MetaAnalysis_1yr"
GlobalVars_["Treatment effect duration","Value"] <- 101

attend_se <- initialise_intervention_dt_attendse (length(population_[,"ID"]), 
                                                  treatment_,
                                                  parameters_)

HBA16 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se)
HBA16

GlobalVars_["Treatment effect duration","Value"] <- 15

HBA17 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se)
HBA17

GlobalVars_["Treatment effect duration","Value"] <- 10

HBA18 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se)
HBA18