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

#Load in population and data files
source("all_model_files.R")
#Set ACM probs to 0
LifeTables[,"MortRate_nextyear"] <- 0
#Create the Global options matrix
source("Global Options.R")
#Rest the patient number to 200000
GlobalVars["n","Value"] <- 200000
#Make sure PSA is off
GlobalVars["run_psa", "Value"] <- F
#Produce detailed results
GlobalVars["Results_output", "Value"] <- "Detailed"
####build the population
population <-read.csv("Populations/POPULATION_UKPDS.csv")
population_clean <- build_population(population, PopulationVariables, GlobalVars)
#Generate an array of common random numbers for each patient for each event for each year
random_numbers <- generate_random(length(population_clean[,"ID"])) 

#create new generate results function. UKPDS 90 reports proportion of people who
#are alive who have PVD, ATFIB and MMALB history

#rebuild test simulations
set.seed(1)
results <- run_model(population_clean, 
                     parameter, 
                     50, 
                     "bl", 
                     GlobalVars,
                     random_numbers,
                     LifeTables)

write.csv(results, "Results/nointerventionUKPDS90.csv")

results2 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "test", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)
write.csv(results2, "Results/standardinterventionUKPDS90.csv")