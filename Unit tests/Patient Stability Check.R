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
population <-read.csv("Populations/POPULATION.csv")
population_clean <- build_population(population, PopulationVariables, GlobalVars)
#Generate an array of common random numbers for each patient for each event for each year
random_numbers <- generate_random(length(population_clean[,"ID"])) 

GlobalVars["Results_output", "Value"] <- "Patient Level"
GlobalVars["run_psa", "Value"] <- F

#rebuild test simulations
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

results2 <- run_model(population_clean, 
                      parameter, 
                      50, 
                      "baseline", 
                      GlobalVars,
                      random_numbers,
                      LifeTables)

write.csv(results2, "Results/basecase_control.csv")

stability_res <- 

library(ggplot2)
#Cost stability graph, start at patient 1000 as it is highly unlikely that fewer patients
#can be run

CostGraph <- ggplot(stability_res[1000:length(stability_res$ID),], aes(x=ID))+
  geom_line(aes(y = P2_WMASCost, colour ="red") )+
  geom_line(aes(y = P3_WMASCost, colour ="yellow"), linetype = 2)+
  geom_line(aes(y = P2_SWASCost, colour ="blue"),linetype = 3)+
  geom_line(aes(y = P2_LASCost, colour ="purple"), linetype = 4)+
  ylab("Per patient cost (£)")+
  xlab("Number of patients")+
  scale_color_identity(name="",
                       breaks = c("red", "yellow", "blue", "purple"),
                       labels = c("Phase 2 WMAS", "Phase 3 WMAS", "Phase 2 SWAST", "Phase 2 LAS"),
                       guide = 'legend')


CostGraph
ggsave("Results/StabilityCostGraph.png", plot = CostGraph)

QALYGraph <- ggplot(stability_res[1000:length(stability_res$ID),], aes(x=ID))+
  geom_line(aes(y = P2_WMASQALY, colour ="red") )+
  geom_line(aes(y = P3_WMASQALY, colour ="yellow"), linetype = 2)+
  geom_line(aes(y = P2_SWASQALY, colour ="blue"),linetype = 3)+
  geom_line(aes(y = P2_LASQALY, colour ="purple"), linetype = 4)+
  ylim(12.2,13)+
  ylab("Quality Adjusted Life Years")+
  xlab("Number of patients")+
  scale_color_identity(name="",
                       breaks = c("red", "yellow", "blue", "purple"),
                       labels = c("Phase 2 WMAS", "Phase 3 WMAS", "Phase 2 SWAST", "Phase 2 LAS"),
                       guide = 'legend')

QALYGraph
ggsave("Results/StabilityQALYGraph.png", plot = QALYGraph)
