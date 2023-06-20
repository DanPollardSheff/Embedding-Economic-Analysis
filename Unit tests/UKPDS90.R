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
set.seed(429)

source("all_model_files.R")
source("Global Options.R")


#Produce the parameters matrix
parameters <- parameter[1,]
#Build the population
UKPDS90 <- read.csv("X:/ScHARR/PR_Amy_Ahern_WLM/General/GLoW modelling/Model/symmetrical-octo-tribble-master Versions/Model Rebuild/symmetrical-octo-tribble/Unit tests/UKPDS90standardperson.csv")
UKPDS90_person <- build_population(UKPDS90,PopulationVariables,GlobalVars)
alive <- UKPDS90_person[,"ID"]>1
#Produce the deterministic trajectories
#Only need the first two years
HbA1c_UKPDStraj<-UKPDS_90_contrisk_A1c(UKPDS90_person, parameters, 2)

if(round(HbA1c_UKPDStraj[1,3],2)!= 6.94){
  stop("HbA1c trajectory does not match the UKPDS 90 example in year 1")
}
if(round(HbA1c_UKPDStraj[1,4],2) != 7.29){
  stop("HbA1c tracjectory does not match the UKPDS 90 example in year 2")
}

#round smoking coefficients to the ones in the supplementary material
parameters[,"SMO_UKPDS90_CONS"] <- round(parameters[,"SMO_UKPDS90_CONS"],3)
parameters[,"SMO_UKPDS90_FEMALE"] <- round(parameters[,"SMO_UKPDS90_FEMALE"],2)
parameters[,"SMO_UKPDS90_SMOlastyear"] <- round(parameters[,"SMO_UKPDS90_SMOlastyear"],3)
parameters[,"SMO_UKPDS90_SMODiag"] <- round(parameters[,"SMO_UKPDS90_SMODiag"],3)
parameters[,"SMO_UKPDS90_DIABDUR"] <- round(parameters[,"SMO_UKPDS90_DIABDUR"],3)
parameters[,"SMO_UKPDS90_AgeDiag"] <- round(parameters[,"SMO_UKPDS90_AgeDiag"],3)

#Do maths in UKPDS90 appendix
prob_smo1 <- 1/(1+exp(-(0.016-0.05*61+2.018+5.535-1.574*log(4+1))))
prob_smo2 <- 1/(1+exp(-(0.016-0.05*61+2.018+5.535-1.574*log(4+2))))


#Produce an error if it doesn't match the appendix
if(round(UKPDS_90_smo(UKPDS90_person,parameters,alive)[1],3) != round(prob_smo1,3)){
  stop("smoking probability does not match the UKPDS 90 example")
}

UKPDS90_person[,"AGE"] <- UKPDS90_person[,"AGE"]+1
UKPDS90_person[,"DIAB_DUR"] <- UKPDS90_person[,"DIAB_DUR"]+1

#Produce an error if it doesn't match the appendix
if(round(UKPDS_90_smo(UKPDS90_person,parameters,alive)[1],3) != round(prob_smo2,3)){
  stop("smoking probability does not match the UKPDS 90 example")
}

#reset age and diabetes duration back to normal
UKPDS90_person[,"AGE"] <- UKPDS90_person[,"AGE"]-1
UKPDS90_person[,"DIAB_DUR"] <- UKPDS90_person[,"DIAB_DUR"]-1

#Round ATFIB coefficents to the same number of decimal places as quoted in the supp mat
parameters[,"ATFIB_CONS"] <- round(parameters[,"ATFIB_CONS"],3)
parameters[,"ATFIB_AGEDAIG"] <- round(parameters[,"ATFIB_AGEDAIG"],3)
parameters[,"ATFIB_BMI"] <- round(parameters[,"ATFIB_BMI"],3)

#Check ATFIB probabilities
if(round(UKPDS_90_ATFIB(UKPDS90_person,parameters,alive)[1],4) != 0.0029){
  stop("ATFIB probability does not match the UKPDS 90 example")
}

#Check eGFR
#Round coefficients to the same level as used in the supplementary material
parameters[,"eGFR_cu60_CONS"] <- round(parameters[,"eGFR_cu60_CONS"],1)
parameters[,"eGFR_cu60_Female"] <- round(parameters[,"eGFR_cu60_Female"],3)
parameters[,"eGFR_cu60_AFRO"] <- round(parameters[,"eGFR_cu60_AFRO"],3)
parameters[,"eGFR_cu60_INDIAN"] <- round(parameters[,"eGFR_cu60_INDIAN"],3)
parameters[,"eGFR_cu60_eGFR"] <- round(parameters[,"eGFR_cu60_eGFR"],3)
parameters[,"eGFR_cu60_firsteGFR"] <- round(parameters[,"eGFR_cu60_firsteGFR"],3)
parameters[,"eGFR_cu60_DIABDUR"] <- round(parameters[,"eGFR_cu60_DIABDUR"],3)
parameters[,"eGFR_cu60_Shape"] <- round(parameters[,"eGFR_cu60_Shape"],3)

parameters[,"eGFR_co60_CONS"] <- round(parameters[,"eGFR_co60_CONS"],3)
parameters[,"eGFR_co60_Female"] <- round(parameters[,"eGFR_co60_Female"],3)
parameters[,"eGFR_co60_AFRO"] <- round(parameters[,"eGFR_co60_AFRO"],3)
parameters[,"eGFR_co60_INDIAN"] <- round(parameters[,"eGFR_co60_INDIAN"],3)
parameters[,"eGFR_co60_eGFR"] <- round(parameters[,"eGFR_co60_eGFR"],3)
parameters[,"eGFR_co60_firsteGFR"] <- round(parameters[,"eGFR_co60_firsteGFR"],3)
parameters[,"eGFR_co60_DIABDUR"] <- round(parameters[,"eGFR_co60_DIABDUR"],3)
parameters[,"eGFR_co60_Shape"] <- round(parameters[,"eGFR_co60_Shape"],3)

#Produce a vector of trues, the length of the population matrix
Under60 <- rep(T,times = length(UKPDS90_person[,"ID"]))
#Produce a vector of false, the length of the population matrix
Over60 <- rep(F, times =length(UKPDS90_person[,"ID"]))

#redo thier maths
FV_egfru60 <- 26.1 +2.162 -(3.280*log(4+1))+0.567*90+0.138*90
pnorm1 <- pnorm((0-FV_egfru60)/9.452)
pnorm2 <- pnorm((60-FV_egfru60)/9.452)
dnorm1 <- dnorm((0-FV_egfru60)/9.452)
dnorm2 <- dnorm((60-FV_egfru60)/9.452)

egfru60 <- FV_egfru60-9.452*((dnorm1-dnorm2)/(pnorm1-pnorm2))

FV_egfro60 <- 23.970+3.419-3.013*log(4+1)+0.406*90+0.297*90
pnorm3 <- pnorm((60-FV_egfro60)/12.575)
dnorm3 <- dnorm((60-FV_egfro60)/12.575)
eGFRo60 <- FV_egfro60+12.575*(dnorm3/(1-pnorm3))


if(round(UKPDS_90_eGFR(UKPDS90_person,parameters,Under60)[1],2) != round(egfru60,2) &
   round(UKPDS_90_eGFR(UKPDS90_person,parameters,Over60)[1],2) != round(eGFRo60,2)){
  stop("errors in both eGFR above and below 60")
}else if(round(UKPDS_90_eGFR(UKPDS90_person,parameters,Under60)[1],2) != round(egfru60,2)){
  stop("errors in eGFR under 60 only")
}else if(round(UKPDS_90_eGFR(UKPDS90_person,parameters,Over60)[1],2) != round(eGFRo60,2)){
  stop("errors in eGFR over 60 only")
}
