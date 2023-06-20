#    Embedding RCT Health Economic Analysis using the Sheffield Type 2 Diabetes Treatment Model - version 3
#    Copyright (C) 2023  Pollard,Pidd,Breeze,Brennan

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
parameters_ <- parameter[2,]
endtime_ <- 50
treatment_ <- "Embedding_TrialEffect_All"

HDL1 <- initialise_intervention_dt_HDL(length(population_[,"ID"]),
                                       treatment_,
                                       parameters_,
                                       endtime_,
                                       GlobalVars_)
HDL1

GlobalVars_["Treatment effect duration","Value"] <- 3

HDL2 <- initialise_intervention_dt_HDL(length(population_[,"ID"]),
                                       treatment_,
                                       parameters_,
                                       endtime_,
                                       GlobalVars_)
HDL2

GlobalVars_["Treatment effect duration","Value"] <- 5

HDL3 <- initialise_intervention_dt_HDL(length(population_[,"ID"]),
                                       treatment_,
                                       parameters_,
                                       endtime_,
                                       GlobalVars_)
HDL3

treatment_ <- "Embedding_TrialEffect_All_1yr"

GlobalVars_["Treatment effect duration","Value"] <- 10
HDL4 <- initialise_intervention_dt_HDL(length(population_[,"ID"]),
                                       treatment_,
                                       parameters_,
                                       endtime_,
                                       GlobalVars_)
HDL4

GlobalVars_["Treatment effect duration","Value"] <- 3
HDL5 <- initialise_intervention_dt_HDL(length(population_[,"ID"]),
                                       treatment_,
                                       parameters_,
                                       endtime_,
                                       GlobalVars_)
HDL5

GlobalVars_["Treatment effect duration","Value"] <- 5
HDL6 <- initialise_intervention_dt_HDL(length(population_[,"ID"]),
                                       treatment_,
                                       parameters_,
                                       endtime_,
                                       GlobalVars_)
HDL6

treatment_ <- "Embedding_TrialEffect_PriandSS"
GlobalVars_["Treatment effect duration","Value"] <- 10

HDL7 <- initialise_intervention_dt_HDL(length(population_[,"ID"]),
                                       treatment_,
                                       parameters_,
                                       endtime_,
                                       GlobalVars_)
HDL7

GlobalVars_["Treatment effect duration","Value"] <- 3
HDL8 <- initialise_intervention_dt_HDL(length(population_[,"ID"]),
                                       treatment_,
                                       parameters_,
                                       endtime_,
                                       GlobalVars_)
HDL8

GlobalVars_["Treatment effect duration","Value"] <- 5
HDL9 <- initialise_intervention_dt_HDL(length(population_[,"ID"]),
                                       treatment_,
                                       parameters_,
                                       endtime_,
                                       GlobalVars_)
HDL9

###SE attendance

treatment_ <- "Embedding_MetaAnalysis_All"
GlobalVars_["Treatment effect duration","Value"] <- 101

attend_se <- initialise_intervention_dt_attendse (length(population_[,"ID"]), 
                                                  treatment_,
                                                  parameters_)

HDL10 <- initialise_intervention_dt_HDL(length(population_[,"ID"]),
                                        treatment_,
                                        parameters_,
                                        endtime_,
                                        GlobalVars_,
                                        attend_se)
HDL10

GlobalVars_["Treatment effect duration","Value"] <- 15

HDL11 <- initialise_intervention_dt_HDL(length(population_[,"ID"]),
                                        treatment_,
                                        parameters_,
                                        endtime_,
                                        GlobalVars_,
                                        attend_se)
HDL11

GlobalVars_["Treatment effect duration","Value"] <- 10

HDL12 <- initialise_intervention_dt_HDL(length(population_[,"ID"]),
                                        treatment_,
                                        parameters_,
                                        endtime_,
                                        GlobalVars_,
                                        attend_se)
HDL12

treatment_ <- "Control_MetaAnalysis_all"
GlobalVars_["Treatment effect duration","Value"] <- 101

attend_se <- initialise_intervention_dt_attendse (length(population_[,"ID"]), 
                                                  treatment_,
                                                  parameters_)

HDL13 <- initialise_intervention_dt_HDL(length(population_[,"ID"]),
                                        treatment_,
                                        parameters_,
                                        endtime_,
                                        GlobalVars_,
                                        attend_se)
HDL13

GlobalVars_["Treatment effect duration","Value"] <- 15

HDL14 <- initialise_intervention_dt_HDL(length(population_[,"ID"]),
                                        treatment_,
                                        parameters_,
                                        endtime_,
                                        GlobalVars_,
                                        attend_se)
HDL14

GlobalVars_["Treatment effect duration","Value"] <- 10

HDL15 <- initialise_intervention_dt_HDL(length(population_[,"ID"]),
                                        treatment_,
                                        parameters_,
                                        endtime_,
                                        GlobalVars_,
                                        attend_se)
HDL15


treatment_ <- "Embedding_MetaAnalysis_1yr"
GlobalVars_["Treatment effect duration","Value"] <- 101

attend_se <- initialise_intervention_dt_attendse (length(population_[,"ID"]), 
                                                  treatment_,
                                                  parameters_)

HDL16 <- initialise_intervention_dt_HDL(length(population_[,"ID"]),
                                        treatment_,
                                        parameters_,
                                        endtime_,
                                        GlobalVars_,
                                        attend_se)
HDL16

GlobalVars_["Treatment effect duration","Value"] <- 15

HDL17 <- initialise_intervention_dt_HDL(length(population_[,"ID"]),
                                        treatment_,
                                        parameters_,
                                        endtime_,
                                        GlobalVars_,
                                        attend_se)
HDL17

GlobalVars_["Treatment effect duration","Value"] <- 10

HDL18 <- initialise_intervention_dt_HDL(length(population_[,"ID"]),
                                        treatment_,
                                        parameters_,
                                        endtime_,
                                        GlobalVars_,
                                        attend_se)
HDL18