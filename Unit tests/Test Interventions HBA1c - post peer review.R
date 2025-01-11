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
population_ <- population_clean
#Generate an array of common random numbers for each patient for each event for each year
random_numbers <- generate_random(length(population_clean[,"ID"])) 

#Select the deterministic model runs

GlobalVars_ <- GlobalVars
parameters_ <- parameter[1,]
endtime_ <- 50
treatment_ <- "Embedding_TrialEffect_All"

##Note, unlike original validations the durations are not tested, as this code has not been changed - only the one year HbA1c.



#Base Case analysis - Trial effects and default treatment effect duration
#Intervention
HBA1 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_)
HBA1

#Control
treatment_ <- "baseline"
HBA2 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_)
HBA2



####Scenario: No statistically insignificant outcomes
#Run models
treatment_ <- "Embedding_TrialEffect_PriandSS"
HBA3 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_)
HBA3

####Scenario: One year step wedge only
treatment_ <- "Embedding_TrialEffect_All_1yr"

HBA4 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_)
HBA4


#Scenario: Uptake of SSME & Meta Analysis 

treatment_ <- "Embedding_MetaAnalysis_All"
attend_se_e <- initialise_intervention_dt_attendse (length(population_[,"ID"]), 
                                                  treatment_,
                                                  parameters_)

HBA5 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_,
                                         attend_se_e)
HBA5

treatment_ <- "Control_MetaAnalysis_all"

attend_se_c <- initialise_intervention_dt_attendse (length(population_[,"ID"]), 
                                                  treatment_,
                                                  parameters_)

HBA6 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_,
                                         attend_se_c)
HBA6

#Scenario: Uptake of SSME from study, only 1 year data & Meta Analysis
treatment_ <- "Embedding_MetaAnalysis_1yr"
attend_se_e <- initialise_intervention_dt_attendse (length(population_[,"ID"]), 
                                                    treatment_,
                                                    parameters_)

HBA7 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_,
                                         attend_se_e)
HBA7

##Subgroups
GlobalVars["HbA1c Scenario", "Value"] <- "PopulationWhite"
treatment_ <- "Embedding_TrialEffect_All"

HBA8 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_,
                                         attend_se_e)
HBA8

GlobalVars_["HbA1c Scenario", "Value"] <- "PopulationWhite"
treatment_ <- "Embedding_TrialEffect_All"

HBA9 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_,
                                         attend_se_e)
HBA9

GlobalVars_["HbA1c Scenario", "Value"] <- "PopulationEthnicMinority"
HBA10 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                         treatment_,
                                         parameters_,
                                         endtime_,
                                         GlobalVars_,
                                         attend_se_e)
HBA10

GlobalVars_["HbA1c Scenario", "Value"] <- "CompleteCase"
HBA10 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se_e)
HBA10

GlobalVars_["HbA1c Scenario", "Value"] <- "EducationAttenders"
HBA11 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se_e)
HBA11

GlobalVars_["HbA1c Scenario", "Value"] <- "BaselineA1cAbove47.5"
HBA12 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se_e)
HBA12

GlobalVars_["HbA1c Scenario", "Value"] <- "RecruitedBeforeFeb2020"
HBA13 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se_e)
HBA13

GlobalVars_["HbA1c Scenario", "Value"] <- "DESMOND"
HBA13 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se_e)
HBA13

GlobalVars_["HbA1c Scenario", "Value"] <- "Diabetes 2gether"
HBA14 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se_e)
HBA14

GlobalVars_["HbA1c Scenario", "Value"] <- "Spotlight"
HBA14 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se_e)
HBA14

GlobalVars_["HbA1c Scenario", "Value"] <- "Xpert Health"
HBA14 <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),
                                          treatment_,
                                          parameters_,
                                          endtime_,
                                          GlobalVars_,
                                          attend_se_e)
HBA14