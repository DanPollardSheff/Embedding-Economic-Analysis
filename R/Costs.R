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


##'@param population_ is the population matrix
##'@param parameters_ is a single row of the parameters matrix
##'@param year_ is the current year of the simulation
##'@param alive_ is a vector of TRUES and FLASES to indicate that a patient is alive or not
##'@param GlobalVars_ is the global variables matrix
##'@param treatment_ is a text string indicating the treatment
##'@param attend_se_ is a matrix indicating whether a person had  
##'@return population_ returns the population matrix


calculate_costs <- function(population_, parameters_, year_, alive_, GlobalVars_,treatment_,attend_se_) {
  
  population_[,"DMCOST"][alive_] <- population_[,"MET"][alive_]*parameters_[,"COST_MET"] + 
    population_[,"MET2"][alive_] * parameters_[,"COST_MET2"] + 
    population_[,"INSU"][alive_] * parameters_[,"COST_INSU"]
  
  population_[, "CVDCOST"][alive_] <- 
    population_[, "IHD_E"][alive_] * parameters_[,"COST_IHD_E"] + 
    population_[, "IHD_H"][alive_] * parameters_[,"COST_IHD_H"]  + 
    population_[, "MI_E"][alive_] * parameters_[,"COST_MI_E"] + 
    population_[, "MI_H"][alive_] * parameters_[,"COST_MI_H"] + 
    population_[, "MI2_E"][alive_] * (parameters_[,"COST_MI_E"] - parameters_[,"COST_MI_H"]) + 
    #only add on the additional costs of a 2nd MI, and do not apply two sets of history costs
    population_[, "STRO_E"][alive_] * parameters_[,"COST_STRO_E"] + 
    population_[, "STRO_H"][alive_] * parameters_[,"COST_STRO_H"] +
    population_[, "STRO2_E"][alive_] * (parameters_[,"COST_STRO_E"]-parameters_[,"COST_STRO_H"]) + 
    #only add on the additional costs of a 2nd stroke, and do not apply two sets of history costs
    population_[, "CHF_E"][alive_] * parameters_[,"COST_CHF_E"]+
    population_[, "CHF_H"][alive_] * parameters_[,"COST_CHF_H"]+
    population_[,"ATFIB_E"][alive_]*0+ 
    population_[,"ATFIB_H"][alive_]*0+# note no cost was identified, but code will work if a parameter is added here
    #Adjusting for mortality
    #UKPDS OM v2 does not attribute mortality to events
    #So if they die in a year they have an event, costs due to death are applied
    ifelse(population_[,"IHD_E"][alive_]==1&is.na(population_[,"F_ALLCAUSE"][alive_]==F), #apply lower costs if the CHF was fatal
           (parameters_[,"COST_FIHD"]-parameters_[,"COST_IHD_E"]),
           0)+
    ifelse(population_[,"MI_E"][alive_]==1&is.na(population_[,"F_ALLCAUSE"][alive_]==F), #apply lower costs if the MI was fatal
           (parameters_[,"COST_FMI"]-parameters_[,"COST_MI_E"]),
           0)+
    ifelse(population_[,"MI2_E"][alive_]==1&is.na(population_[,"F_ALLCAUSE"][alive_]==F), #apply lower costs if the 2nd MI was fatal
           (parameters_[,"COST_FMI"]-parameters_[,"COST_MI_E"]),
           0)+
    ifelse(population_[,"STRO_E"][alive_]==1&is.na(population_[,"F_ALLCAUSE"][alive_]==F), #apply lower costs if the Stroke was fatal
           (parameters_[,"COST_FSTRO"]-parameters_[,"COST_STRO_E"]),
           0)+
    ifelse(population_[,"STRO2_E"][alive_]==1&is.na(population_[,"F_ALLCAUSE"][alive_]==F), #apply lower costs if the 2nd Stroke was fatal
           (parameters_[,"COST_FSTRO"]-parameters_[,"COST_STRO_E"]),
           0)
  
population_[, "NEPHCOST"][alive_] <- 
    population_[, "RENAL_E"][alive_] * parameters_[,"COST_RENAL_E"]+
    population_[, "RENAL_H"][alive_] * parameters_[,"COST_RENAL_H"]+
    population_[, "MMALB_E"][alive_]*0+ # note no cost was identified, but code will work if a parameter is added here
    population_[, "MMALB_H"][alive_]*0  # note no cost was identified, but code will work if a parameter is added here

population_[, "RETCOST"][alive_] <- 
    population_[, "BLIND_E"][alive_] * parameters_[,"COST_BLIND_E"] + 
    population_[, "BLIND_H"][alive_] * parameters_[,"COST_BLIND_H"]
  
population_[, "NEUCOST"][alive_] <- 
    population_[, "AMP_E"][alive_] * parameters_[,"COST_AMP_E"] +
    population_[, "AMP_H"][alive_] * parameters_[,"COST_AMP_H"]+
    population_[, "AMP2_E"][alive_] * parameters_[,"COST_AMP_E"]+
    population_[, "AMP2_H"][alive_] * (parameters_[,"COST_AMP_H"])+ #Cost for two amputations
    population_[, "ULCER_E"][alive_] * parameters_[,"COST_ULCER"]+
    population_[, "ULCER_H"][alive_] * parameters_[,"COST_ULCER"]
  
population_[, "CANCOST"][alive_] <- 
    population_[, "CANB_E"][alive_] * parameters_[,"COST_CANB"] + 
    population_[, "CANC_E"][alive_] * parameters_[,"COST_CANC"] 
  
population_[, "OSTCOST"][alive_] <- 
    population_[, "OST_E"][alive_] * parameters_[,"COST_OST"]+
    population_[, "OST_H"][alive_] * parameters_[,"COST_OST"]
  
population_[, "DEPCOST"][alive_] <- 
    population_[, "DEP_E"][alive_]* parameters_[,"COST_DEP"]+
    population_[, "DEP_H"][alive_]* parameters_[,"COST_DEP"]
  
  ##GP
population_[, "GP"][alive_] <- 0 #set to 0 for now, but call new GP function here
  ##Other costs
population_[, "OTHCOST"][alive_] <- 
    population_[, "GP"][alive_] * parameters_[,"COST_GP"] + 
    population_[,"PVD_E"][alive_]*0+
    population_[,"PVD_H"][alive_]*0# note no cost was identified, but code will work if a parameter is added here

##add in undiscounted costs
population_[, "YearCOST"][alive_] <-population_[, "DMCOST"][alive_]+
  population_[, "CVDCOST"][alive_]+population_[, "NEPHCOST"][alive_]+
  population_[, "RETCOST"][alive_]+population_[, "NEUCOST"][alive_]+
  population_[, "CANCOST"][alive_]+population_[, "OSTCOST"][alive_]+
  population_[, "DEPCOST"][alive_]+population_[, "OTHCOST"][alive_]

##Add in intervention costs, Embedder costs + any SE courses attended
population_ <- Intervention_costs_Embedding(treatment_,
                                                    parameters_,
                                                    population_,
                                                    attend_se_,
                                                    year_,
                                                    alive_)

#store cumulative costs
population_[, "COST"][alive_] <- population_[, "COST"][alive_]+
  population_[, "YearCOST"][alive_]
#Store cumulative discounted costs
population_[, "DiscCOST"][alive_] <- population_[, "DiscCOST"][alive_]+
  (population_[, "YearCOST"][alive_]/((1+as.numeric(GlobalVars_["disc_rate_costs","Value"]))^year_))
  
 
  return(population_)
}

##'@param population_ is the population matrix
##'@param parameter_ is a single row of the parameters matrix
##'@param year_ is the current year of the simulation
##'@param alive_ is a vector of TRUES and FLASES to indicate that a patient is alive or not
##'@param attend_se_ is a matrix indicating whether each patient in the simulation 
##'attends an SE course in each year
##'@return population_ returns the population matrix

Intervention_costs_Embedding <- function(treatment_,
                                         parameter_, 
                                         population_, 
                                         attend_se_, 
                                         year_,
                                         alive_){
  
  
  #add in embedding costs for the first two years, if it is an embedding arm
  if(treatment_ == "Embedding_MetaAnalysis_All"|
     treatment_ == "Embedding_MetaAnalysis_PriandSS"|
     treatment_ == "Embedding_MetaAnalysis_1yr"|
     treatment_ == "Embedding_TrialEffect_All_1yr"|
     treatment_ == "Embedding_TrialEffect_PriandSS_1yr"|
     treatment_ == "Embedding_TrialEffect_All_2yr" | 
     treatment_ == "Embedding_TrialEffect_PriandSS_2yr"|
     treatment_ == "Embedding_TrialEffect_All"|
     treatment_ == "Embedding_TrialEffect_PriandSS"){
    if(year_ ==0|year_==1){
      population_[,"YearCOST"][alive_] <- population_[,"YearCOST"][alive_] +
        parameter_[, "COST_Embedding"]
    }}
  
  #add in SE costs
  #work out who is in the population
  inpop <- attend_se_[,1] %in% population_[,"ID"][alive_]
  attend_se <- subset(attend_se_, inpop)
  
  #SE costs can be incurred in the first or second year of the simulation
  if (year_ == 0){
    population_[,"YearCOST"][alive_] <- population_[,"YearCOST"][alive_] +
      attend_se[,2]*parameter_[, "COST_SE"]
  }else if (year_ == 1){
    population_[,"YearCOST"][alive_] <- population_[,"YearCOST"][alive_] +
      attend_se[,3]*parameter_[, "COST_SE"]
  }
  return(population_)
}