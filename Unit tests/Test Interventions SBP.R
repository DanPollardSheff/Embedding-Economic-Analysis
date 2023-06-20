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
treatment_ <- "Embedding_TrialEffect_All"

SBP1 <- initialise_intervention_dt_SBP(length(population_[,"ID"]),
                                        treatment_,
                                        parameters_,
                                        endtime_,
                                        GlobalVars_)
SBP1

GlobalVars_["Treatment effect duration","Value"] <- 3

SBP2 <- initialise_intervention_dt_SBP(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_)
SBP2

GlobalVars_["Treatment effect duration","Value"] <- 5

SBP3 <- initialise_intervention_dt_SBP(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_)
SBP3

treatment_ <- "Embedding_TrialEffect_All_1yr"

GlobalVars_["Treatment effect duration","Value"] <- 10
SBP4 <- initialise_intervention_dt_SBP(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_)
SBP4

GlobalVars_["Treatment effect duration","Value"] <- 3
SBP5 <- initialise_intervention_dt_SBP(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_)
SBP5

GlobalVars_["Treatment effect duration","Value"] <- 5
SBP6 <- initialise_intervention_dt_SBP(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_)
SBP6

treatment_ <- "Embedding_TrialEffect_PriandSS"
GlobalVars_["Treatment effect duration","Value"] <- 10

SBP7 <- initialise_intervention_dt_SBP(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_)
SBP7

GlobalVars_["Treatment effect duration","Value"] <- 3
SBP8 <- initialise_intervention_dt_SBP(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_)
SBP8

GlobalVars_["Treatment effect duration","Value"] <- 5
SBP9 <- initialise_intervention_dt_SBP(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_)
SBP9

###SE attendance

treatment_ <- "Embedding_MetaAnalysis_All"
GlobalVars_["Treatment effect duration","Value"] <- 101

attend_se <- initialise_intervention_dt_attendse (length(population_[,"ID"]), 
                                                  treatment_,
                                                  parameters_)

SBP10 <- initialise_intervention_dt_SBP(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_,
                                         attend_se)
SBP10

GlobalVars_["Treatment effect duration","Value"] <- 15

SBP11 <- initialise_intervention_dt_SBP(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se)
SBP11

GlobalVars_["Treatment effect duration","Value"] <- 10

SBP12 <- initialise_intervention_dt_SBP(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se)
SBP12

treatment_ <- "Control_MetaAnalysis_all"
GlobalVars_["Treatment effect duration","Value"] <- 101

attend_se <- initialise_intervention_dt_attendse (length(population_[,"ID"]), 
                                                  treatment_,
                                                  parameters_)

SBP13 <- initialise_intervention_dt_SBP(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se)
SBP13

GlobalVars_["Treatment effect duration","Value"] <- 15

SBP14 <- initialise_intervention_dt_SBP(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se)
SBP14

GlobalVars_["Treatment effect duration","Value"] <- 10

SBP15 <- initialise_intervention_dt_SBP(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se)
SBP15


treatment_ <- "Embedding_MetaAnalysis_1yr"
GlobalVars_["Treatment effect duration","Value"] <- 101

attend_se <- initialise_intervention_dt_attendse (length(population_[,"ID"]), 
                                                  treatment_,
                                                  parameters_)

SBP16 <- initialise_intervention_dt_SBP(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se)
SBP16

GlobalVars_["Treatment effect duration","Value"] <- 15

SBP17 <- initialise_intervention_dt_SBP(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se)
SBP17

GlobalVars_["Treatment effect duration","Value"] <- 10

SBP18 <- initialise_intervention_dt_SBP(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se)
SBP18