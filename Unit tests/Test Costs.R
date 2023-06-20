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