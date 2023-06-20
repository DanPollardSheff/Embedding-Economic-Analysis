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



#'@param population_ is the population matrix
#'@param HBA1c_underlying_ is the underling trajectory of HbA1C for each patient
#'@param BMI_underlying_ is the underling trajectory of BMI for each patient
#'@param SBP_underlying_ is the underling trajectory of systolic blood pressure 
#'for each patient
#'@param HDL_underlying_ is the underlying trajectory of HDL cholesterol for each 
#'patient
#'@param LDL_underlying_ is the underlying trajectory of LDL cholesterol for each
#'patient 
#'@param HEARTR_underlying_ is the underlying trajectory of heart rate for each
#'patient
#'@param WBC_underlying_ is the underlying trajectory of white blood cell count
#'for each patient
#'@param HAEM_underlying_ is the underlying trajectory of haemoglobin for each 
#'patient
#'@param HbA1c_INTV_ is any treatment related changes to HbA1c for each patient
#'in each simulated year
#'@param BMI_INTV_ is any treatment related changes to HbA1c for each patient
#'in each simulated year
#'@param SBP_INTV_ is any treatment related changes to systolic blood pressure 
#'for each patient in each simulated year
#'@param HDL_INTV_ is any treatment related changes to HDL cholesterol for each 
#'patient in each simulated year
#'@param LDL_INTV_ is any treatment related changes to LDL cholesterol for each 
#'patient in each simulated year
#'@param year_ is the current simulation year
#'@return population_ is a revised population matrix

update_history <- function(population_,
                           HBA1c_underlying_,
                           BMI_underlying_,
                           SBP_underlying_,    
                           HDL_underlying_,
                           LDL_underlying_, 
                           HEARTR_underlying_,
                           WBC_underlying_,
                           HAEM_underlying_,
                           HBA1c_INTV_,
                           BMI_INTV_,
                           SBP_INTV_,
                           HDL_INTV_,
                           LDL_INTV_,
                           year_){
  #Update histories if there is an event
  population_[,"MI_H"] <- ifelse(population_[,"MI_E"]==1,1,population_[,"MI_H"])
  population_[,"MI2_H"] <- ifelse(population_[,"MI2_E"]==1,1,population_[,"MI2_H"])
  population_[,"AMP_H"] <- ifelse(population_[,"AMP_E"]==1,1,population_[,"AMP_H"])
  population_[,"AMP2_H"] <- ifelse(population_[,"AMP2_E"]==1,1,population_[,"AMP2_H"])
  population_[,"BLIND_H"] <- ifelse(population_[,"BLIND_E"]==1,1,population_[,"BLIND_H"])
  population_[,"ULCER_H"] <- ifelse(population_[,"ULCER_E"]==1,1,population_[,"ULCER_H"])
  population_[,"RENAL_H"] <- ifelse(population_[,"RENAL_E"]==1,1,population_[,"RENAL_H"])
  population_[,"CHF_H"] <- ifelse(population_[,"CHF_E"]==1,1,population_[,"CHF_H"] )
  population_[,"IHD_H"] <- ifelse(population_[,"IHD_E"]==1,1,population_[,"IHD_H"])
  population_[,"STRO_H"] <- ifelse(population_[,"STRO_E"]==1,1,population_[,"STRO_H"])
  population_[,"STRO2_H"] <- ifelse(population_[,"STRO2_E"]==1,1,population_[,"STRO2_H"])
  population_[,"PVD_H"] <- ifelse(population_[,"PVD_E"]==1,1,population_[,"PVD_H"])
  population_[,"MMALB_H"] <- ifelse(population_[,"MMALB_E"]==1,1,population_[,"MMALB_H"])
  population_[,"ATFIB_H"] <- ifelse(population_[,"ATFIB_E"]==1,1,population_[,"ATFIB_H"])
  population_[,"CANB_H"] <- ifelse(population_[,"CANB_E"]==1,1,population_[,"CANB_H"])
  population_[,"CANC_H"] <- ifelse(population_[,"CANC_E"]==1,1,population_[,"CANC_H"])
  population_[,"DEP_H"] <- ifelse(population_[,"DEP_E"]==1,1,population_[,"DEP_H"])
  population_[,"OST_H"] <- ifelse(population_[,"OST_E"]==1,1,population_[,"OST_H"])
  
  #Set all events back to 0
  population_[,"MI_E"] <- 0
  population_[,"MI2_E"] <- 0
  population_[,"AMP_E"] <- 0
  population_[,"AMP2_E"] <- 0
  population_[,"BLIND_E"] <- 0
  population_[,"ULCER_E"] <- 0
  population_[,"RENAL_E"] <- 0
  population_[,"CHF_E"] <- 0
  population_[,"IHD_E"] <- 0
  population_[,"STRO_E"] <- 0
  population_[,"STRO2_E"] <- 0
  population_[,"PVD_E"] <- 0
  population_[,"MMALB_E"] <- 0
  population_[,"ATFIB_E"] <- 0
  population_[,"CANB_E"] <- 0
  population_[,"CANC_E"] <- 0
  population_[,"DEP_E"] <- 0
  population_[,"OST_E"] <- 0
  
  #Reset this years costs and QALYs back to 0
  population_[,"EQ5D"] <- 0
  population_[, "YearCOST"] <- 0
  
  #work out who survived this year
  alive <- is.na(population_[,"F_ALLCAUSE"])
  
  #Update risk factors
  #Look at year + 3, year starts at 0, column 1 is the IS, column 2 are the 
  #baseline factors, column3 onwards are the future years values
  population_[,"HBA"][alive] <- HBA1c_underlying_[,year_+3][alive]+
    HBA1c_INTV_[,year_+2][alive]
  population_[,"BMI"][alive] <- BMI_underlying_[,year_+3][alive]+
    BMI_INTV_[,year_+2][alive]
  population_[,"SBP"][alive] <- SBP_underlying_[,year_+3][alive]+
    SBP_INTV_[,year_+2][alive]
  population_[,"HDL"][alive] <- HDL_underlying_[,year_+3][alive]+
    HDL_INTV_[,year_+2][alive]
  population_[,"LDL"][alive] <- LDL_underlying_[,year_+3][alive]+
    LDL_INTV_[,year_+2][alive]
  population_[,"HEART_R"][alive] <- HEARTR_underlying_[,year_+3][alive]
  population_[,"WBC"][alive] <- WBC_underlying_[,year_+3][alive]
  population_[,"HAEM"][alive] <- HAEM_underlying_[,year_+3][alive]
  
  #update the binary varaibles
  population_[,"BMI_U_18_5"] <- ifelse(population_[,"BMI"]<18.5,1,0)
  population_[,"BMI_O_E_25"] <- ifelse(population_[,"BMI"]>=25,1,0)
  population_[,"LDL_O_35"] <- ifelse(population_[,"LDL"]>3.5, population_[,"LDL"],0)
  
  #remove unnecessary variables
  rm(alive)
  #produce the population matrix
  return(population_)
}

#'@param population_ is the population matrix
#'@param parameters_, is a single row of the parameters matrix
#'@param alive_ is a vector of true and falses indicating whether or not someone
#'is alive at the start of the year
#'@return population_ is a revised population matrix

update_patchars <- function(population_,parameters_,alive_){
  
  #Age the population
  population_[,"AGE"][alive_] <- population_[,"AGE"][alive_]+1
  population_[,"DIAB_DUR"][alive_] <- population_[,"DIAB_DUR"][alive_]+1
  
  #store life expectancy
  alive_thisyear <- is.na(population_[,"F_ALLCAUSE"][alive_])
  #Give people who survived the year a full year of life expectancy, and those 
  #who died half a year
  population_[,"YearsLived"][alive_] <- population_[,"YearsLived"][alive_] +
    ifelse(alive_thisyear,1,0.5)
  
  #Reset pSMO to 0, this is only set every 3 years
  population_[,"p_SMO"] <- NA
  
  #set the menopause characteristics
  population_[, "MEN"][alive_] <- ifelse(population_[,"AGE"][alive_]>51 & population_[,"MALE"][alive_]==0,1,0)
  
  #Update therapy lines
  #Source: Bennett H, et al. Assessment of unmet clinical need in type 2 diabetic 
  #patients on conventional therapy in the UK. Diabetes Ther. 2014;5(2):567-578.
  #Table 1
  HbA1c_esc_mono_to_dual <- 8.48
  HbA1c_esc_dual_to_ins_Bennett  <- 9.78
  HbA1c_esc_dual_to_triple_Bennett  <- 8.71
  #Call in the percentage of people on insulin for third line therapy
  percent_ins_trip <- parameters_[,"third_line_percent_ins"]
  HbA1c_esc_dual_to_third <- 9.78*percent_ins_trip+8.71*(1-percent_ins_trip)
    
  #update treatment lines
  #From 1st line to 2nd line
  population_["MET"][alive_] < ifelse(population_[,"MET"][alive_]==1 & population_[,"HBA"][alive_] >= HbA1c_esc_mono_to_dual,
                             0, 
                             population_[,"MET"])
  population_["MET2"][alive_] < ifelse(population_[,"MET"][alive_]==1 & population_[,"HBA"][alive_] >= HbA1c_esc_mono_to_dual,
                              1, 
                              population_[,"MET2"][alive_])
  
  #From 2nd line to 3rd line
  population_["MET2"] < ifelse(population_[,"MET2"][alive_]==1 & population_[,"HBA"][alive_] >= HbA1c_esc_dual_to_third,
                              0, 
                              population_[,"MET2"][alive_])
  population_["INSU"] < ifelse(population_[,"MET2"][alive_]==1 & population_[,"HBA"][alive_] >= HbA1c_esc_dual_to_third,
                              1, 
                              population_[,"INSU"][alive_])
  
  return(population_)
}
