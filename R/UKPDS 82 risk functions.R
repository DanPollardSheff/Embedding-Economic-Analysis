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



#### Macrovascular risk functions ##############################################

#' @param population is the population matrix returned by the 
#' build_population_parrallel_premodel function
#' @param parameters is the matrix coef that is returned by the set_parameters 
#' function. This matrix contains all parameters used to predict the shape 
#' parameter of the Weibull time to event function 
#' @param rho is the scale parameter of the Weibull time to event function. 
#' This is called UKPDS2_rho_CHF and is put into the global environment by the 
#' set_parameters function
#' @param treatment is a character string that indicates which treatment should 
#' be used. Note, this is only relevant for treatments that directly impact CVD 
#' risks
#' @param alive_ is a TRUE/FALSE vector indicating whether the person is alive this year
#' @return pCHF is the vector of probabilities of having a first CHF for all
#' patients this year

First_CHF_UKPDS_82 <- function(
    population_,
    parameters_,
    rho_,
    treatment_,
    alive_){
  #estimate the fitted value from the UKPDS OMv2 from the regression
  #N.B. Data relevant data transformations 
  #eGFR values are divided by 10 in UKPDS 82
  #SBP values are divided by 10 in UKPDS 82
  #eGFR values are divided by 10 in UKPDS 82
  #Heart Rate values are divided by 10 in UKPDS 82
  #LDL cholesterol values are multiplied by 10 in UKPDS 82
  #HDL cholesterol values are multiplied by 10 in UKPDS 82
  Fitted_value <-
    parameters_[,"Diab_1stCHF_Lambda"]+
    parameters_[,"Diab_1stCHF_AGE_DIAG"]*(population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
    parameters_[,"Diab_1stCHF_ATFIB"]*population_[,"ATFIB_E"][alive_]+
    parameters_[,"Diab_1stCHF_ATFIB"]*population_[,"ATFIB_H"][alive_]+
    parameters_[,"Diab_1stCHF_BMI"]*population_[,"BMI"][alive_]+
    parameters_[,"Diab_1stCHF_eGFR_lessthan_60"]*(population_[,"eGFR_U_60"][alive_]/10)+
    parameters_[,"Diab_1stCHF_LDL"]*(population_[,"LDL"][alive_]*10)+
    parameters_[,"Diab_1stCHF_micro_macroalbumuria"]*population_[,"MMALB_E"][alive_]+
    parameters_[,"Diab_1stCHF_micro_macroalbumuria"]*population_[,"MMALB_H"][alive_]+
    parameters_[,"Diab_1stCHF_PVD"]*population_[,"PVD_E"][alive_]+
    parameters_[,"Diab_1stCHF_PVD"]*population_[,"PVD_H"][alive_]+
    parameters_[,"Diab_1stCHF_AMP_HIST"]*population_[,"AMP_H"][alive_]+
    parameters_[,"Diab_1stCHF_ULCER_HIST"]*population_[,"ULCER_H"][alive_]
  
  #calculate the probability of someone's first CHF
  Haz_this_year <- exp(Fitted_value)*(population_[,"DIAB_DUR"][alive_]^rho_)
  Haz_next_year <- exp(Fitted_value)*((population_[,"DIAB_DUR"][alive_]+1)^rho_)
  pCHF <-  1 - exp (Haz_this_year - Haz_next_year)
  
  #set the probability of first CHF back to 0 for people with CHF 
  #history
  pCHF <- ifelse(population_[,"CHF_H"][alive_]==1,0,pCHF)
  
  #remove the temporary variables in this code
  rm(Fitted_value, Haz_this_year, Haz_next_year)
  
  #return the probability of having a CHF
  return(pCHF)
}

#' @param population is the population matrix returned by the 
#' build_population_parrallel_premodel function
#' @param parameters is the matrix coef that is returned by the set_parameters 
#' function. This matrix contains all parameters used to predict the shape 
#' parameter of the Weibull time to event function 
#' @param rho is the scale parameter of the Weibull time to event function.
#' This is called UKPDS2_rho_IHD and is put into the global enviroment by the 
#' set_parameters function
#' @param treatment is a character string that indicates which treatment should 
#' be used. Note, this is only relevant for treatments that directly impact CVD 
#' risks
#' @param alive_ is a TRUE/FALSE vector indicating whether the person is alive this year
#' @return pIHD is the vector of probabiltieis of having a first CHF for all
#' patients this year

First_IHD_UKPDS_82 <- function(
    population_,
    parameters_,
    rho_,
    treatment_,
    alive_){
  #estimate the fitted value from the UKPDS OMv2 from the regression
  #N.B. Data relevant data transformations 
  #eGFR values are divided by 10 in UKPDS 82
  #SBP values are divided by 10 in UKPDS 82
  #eGFR values are divided by 10 in UKPDS 82
  #Heart Rate values are divided by 10 in UKPDS 82
  #LDL cholesterol values are multiplied by 10 in UKPDS 82
  #HDL cholesterol values are multiplied by 10 in UKPDS 82
  Fitted_value <-
    parameters_[,"Diab_1stIHD_Lambda"]+
    parameters_[,"Diab_1stIHD_AGE_DIAG"]*(population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
    parameters_[,"Diab_1stIHD_FEMALE"]*population_[,"FEMALE"][alive_]+
    parameters_[,"Diab_1stIHD_eGFR"]*(population_[,"eGFR"][alive_]/10)+
    parameters_[,"Diab_1stIHD_HDL"]*(population_[,"HDL"][alive_]*10)+
    parameters_[,"Diab_1stIHD_LDL"]*(population_[,"LDL"][alive_]*10)+
    parameters_[,"Diab_1stIHD_PVD"]*population_[,"PVD_E"][alive_]+
    parameters_[,"Diab_1stIHD_PVD"]*population_[,"PVD_H"][alive_]+
    parameters_[,"Diab_1stIHD_SBP"]*(population_[,"SBP"][alive_]/10)+
    parameters_[,"Diab_1stIHD_AMP_HIST"]*population_[,"AMP_H"][alive_]+
    parameters_[,"Diab_1stIHD_CHF_HIST"]*population_[,"CHF_H"][alive_]
    
  
  #calculate the probability of someone's first IHD
  Haz_this_year <- exp(Fitted_value)*(population_[,"DIAB_DUR"][alive_]^rho_)
  Haz_next_year <- exp(Fitted_value)*((population_[,"DIAB_DUR"][alive_]+1)^rho_)
  pIHD <-  1 - exp (Haz_this_year - Haz_next_year)
  
  #set the probability of first IHD back to 0 for people with IHD 
  #history
  pIHD <- ifelse(population_[,"IHD_H"][alive_]==1,0,pIHD)
  
  #remove the temporary variables in this code
  rm(Fitted_value, Haz_this_year, Haz_next_year)
  
  #return the probability of having a IHD
  return(pIHD)
}

#' @param population is the population matrix returned by the 
#' build_population_parrallel_premodel function
#' @param parameters is the matrix coef that is returned by the set_parameters 
#' function. This matrix contains all parameters used to predict the shape 
#' parameter of the Exponential time to event function 
#' @param treatment is a character string that indicates which treatment should 
#' be used. Note, this is only relevant for treatments that directly impact CVD 
#' risks
#' @param alive_ is a TRUE/FALSE vector indicating whether the person is alive this year
#' @return pMI is the vector of probabilities of having a first MI for all
#' patients this year


First_MI_Male_UKPDS_82 <- function(
    population_,
    parameters_,
    treatment_,
    alive_){
  #estimate the fitted value from the UKPDS OMv2 from the regression
  #N.B. Data relevant data transformations 
  #eGFR values are divided by 10 in UKPDS 82
  #SBP values are divided by 10 in UKPDS 82
  #eGFR values are divided by 10 in UKPDS 82
  #Heart Rate values are divided by 10 in UKPDS 82
  #LDL cholesterol values are multiplied by 10 in UKPDS 82
  #HDL cholesterol values are multiplied by 10 in UKPDS 82
  Fitted_value <-
    parameters_[,"Diab_1stMI_Male_Lambda"]+
    parameters_[,"Diab_1stMI_Male_AFRO"]*population_[,"AFRO"][alive_]+
    parameters_[,"Diab_1stMI_Male_AGE_DIAG"]*(population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
    parameters_[,"Diab_1stMI_Male_INDIAN"]*population_[,"INDIAN"][alive_]+
    parameters_[,"Diab_1stMI_Male_HbA1c"]*population_[,"HBA"][alive_]+
    parameters_[,"Diab_1stMI_Male_HDL"]*(population_[,"HDL"][alive_]*10)+
    parameters_[,"Diab_1stMI_Male_LDL"]*(population_[,"LDL"][alive_]*10)+
    parameters_[,"Diab_1stMI_Male_micro_macroalbumuria"]*population_[,"MMALB_E"][alive_]+
    parameters_[,"Diab_1stMI_Male_micro_macroalbumuria"]*population_[,"MMALB_H"][alive_]+
    parameters_[,"Diab_1stMI_Male_PVD"]*population_[,"PVD_E"][alive_]+
    parameters_[,"Diab_1stMI_Male_PVD"]*population_[,"PVD_H"][alive_]+
    parameters_[,"Diab_1stMI_Male_SBP"]*(population_[,"SBP"][alive_]/10)+
    parameters_[,"Diab_1stMI_Male_SMOKER"]*population_[,"SMO"][alive_]+
    parameters_[,"Diab_1stMI_Male_WBC"]*population_[,"WBC"][alive_]+
    parameters_[,"Diab_1stMI_Male_AMP_HIST"]*population_[,"AMP_H"][alive_]+
    parameters_[,"Diab_1stMI_Male_CHF_HIST"]*population_[,"CHF_H"][alive_]+
    parameters_[,"Diab_1stMI_Male_IHD_HIST"]*population_[,"IHD_H"][alive_]+
    parameters_[,"Diab_1stMI_Male_STROKE_HIST"]*population_[,"STRO_H"][alive_]
  
  
  #calculate the probability of someone's first MI
  Haz_this_year <- exp(Fitted_value)*(population_[,"DIAB_DUR"][alive_])
  Haz_next_year <- exp(Fitted_value)*(population_[,"DIAB_DUR"][alive_]+1)
  pMI <-  1 - exp (Haz_this_year - Haz_next_year)
  
  #set the probability of first MI back to 0 for people with MI 
  #history
  pMI <- ifelse(population_[,"MI_H"][alive_]==1,0,pMI)
  
  #remove the temporary variables in this code
  rm(Fitted_value, Haz_this_year, Haz_next_year)
  
  #return the probability of having a MI
  return(pMI)
}

#' @param population is the population matrix returned by the 
#' build_population_parrallel_premodel function
#' @param parameters is the matrix coef that is returned by the set_parameters 
#' function. This matrix contains all parameters used to predict the shape 
#' parameter of the Weibull time to event function 
#' @param rho is the scale parameter of the Weibull time to event function
#' This is called UKPDS2_rho_MI1_F and is put into the global enviroment by the 
#' set_parameters function
#' @param treatment is a character string that indicates which treatment should 
#' be used. Note, this is only relevant for treatments that directly impact CVD 
#' risks
#' @param alive_ is a TRUE/FALSE vector indicating whether the person is alive this year
#' @return pMI is the vector of probabilities of having a first MI for all
#' patients this year


First_MI_Female_UKPDS_82 <- function(
    population_,
    parameters_,
    rho_,
    treatment_,
    alive_){
  #estimate the fitted value from the UKPDS OMv2 from the regression
  #N.B. Data relevant data transformations 
  #eGFR values are divided by 10 in UKPDS 82
  #SBP values are divided by 10 in UKPDS 82
  #eGFR values are divided by 10 in UKPDS 82
  #Heart Rate values are divided by 10 in UKPDS 82
  #LDL cholesterol values are multiplied by 10 in UKPDS 82
  #HDL cholesterol values are multiplied by 10 in UKPDS 82
  Fitted_value <-
    parameters_[,"Diab_1stMI_Female_Lambda"]+
    parameters_[,"Diab_1stMI_Female_AFRO"]*population_[,"AFRO"][alive_]+
    parameters_[,"Diab_1stMI_Female_AGE_DIAG"]*(population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
    parameters_[,"Diab_1stMI_Female_eGFR_lessthan_60"]*(population_[,"eGFR_U_60"][alive_]/10)+
    parameters_[,"Diab_1stMI_Female_HbA1c"]*population_[,"HBA"][alive_]+
    parameters_[,"Diab_1stMI_Female_LDL_greaterthan_35"]*(population_[,"LDL_O_35"][alive_]*10)+
    parameters_[,"Diab_1stMI_Female_micro_macroalbumuria"]*population_[,"MMALB_E"][alive_]+
    parameters_[,"Diab_1stMI_Female_micro_macroalbumuria"]*population_[,"MMALB_H"][alive_]+
    parameters_[,"Diab_1stMI_Female_PVD"]*population_[,"PVD_E"][alive_]+
    parameters_[,"Diab_1stMI_Female_PVD"]*population_[,"PVD_H"][alive_]+
    parameters_[,"Diab_1stMI_Female_SBP"]*(population_[,"SBP"][alive_]/10)+
    parameters_[,"Diab_1stMI_Female_SMOKER"]*population_[,"SMO"][alive_]+
    parameters_[,"Diab_1stMI_Female_WBC"]*population_[,"WBC"][alive_]+
    parameters_[,"Diab_1stMI_Female_CHF_HIST"]*population_[,"CHF_H"][alive_]+
    parameters_[,"Diab_1stMI_Female_IHD_HIST"]*population_[,"IHD_H"][alive_]
    
  #calculate the probability of someone's first CHF
  Haz_this_year <- exp(Fitted_value)*(population_[,"DIAB_DUR"][alive_]^rho_)
  Haz_next_year <- exp(Fitted_value)*((population_[,"DIAB_DUR"][alive_]+1)^rho_)
  pMI <-  1 - exp (Haz_this_year - Haz_next_year)
  
  #set the probability of first MI back to 0 for people with MI 
  #history
  pMI <- ifelse(population_[,"MI_H"][alive_]==1,0,pMI)
  
  #remove the temporary variables in this code
  rm(Fitted_value, Haz_this_year, Haz_next_year)
  
  #return the probability of having a MI
  return(pMI)
}

#' @param population is the population matrix returned by the 
#' build_population_parrallel_premodel function
#' @param parameters is the matrix coef that is returned by the set_parameters 
#' function. This matrix contains all parameters used to predict the shape 
#' parameter of the Weibull time to event function 
#' @param rho_f is the scale parameter of the Weibull time to event function for women
#' @param treatment is a character string that indicates which treatment should 
#' be used. Note, this is only relevant for treatments that directly impact CVD 
#' risks
#' @param alive_ is a TRUE/FALSE vector indicating whether the person is alive this year
#' @return pMI is the vector of probabilities of having a first MI for all
#' patients this year

First_MI_UKPDS_82 <- function(
    population_,
    parameters_,
    rho_f_,
    treatment_,
    alive_){
  #Produce a TRUE false vector indicating whether a patient is a woman
  female <- population_[,"MALE"][alive_]==0
  
  #if a women apply the First_MI_Female_UKPDS_82 function
  #if not apply the First_MI_Male_UKPDS_82
  
  pMI_F <- First_MI_Female_UKPDS_82(
    population_,
    parameters_,
    rho_f_,
    treatment_,
    alive_
  )
  
  pMI_M <- First_MI_Male_UKPDS_82(
    population_,
    parameters_,
    treatment_,
    alive_
  )
  
  pMI <- ifelse(female, 
                pMI_F,
                pMI_M)
  
  #reset pMI to 0 for patients who have already had 1 MI
  #Note this line shouldn't do anything as it is already applied in both the
  #First_MI_Female_UKPDS_82 & First_MI_Male_UKPDS_82 functions
  pMI <- ifelse(population_[,"MI_H"][alive_]==1, 0, pMI)
  
  #remove the temporary variables
  rm(pMI_F, pMI_M, female)
  
  #return the probability of having a first MI
  return(pMI)
}


#' @param population is the population matrix returned by the 
#' build_population_parrallel_premodel function
#' @param parameters is the matrix coef that is returned by the set_parameters 
#' function. This matrix contains all parameters used to predict the shape 
#' parameter of the Exponential time to event function 
#' @param treatment is a character string that indicates which treatment should 
#' be used. Note, this is only relevant for treatments that directly impact CVD 
#' risks
#' @param alive_ is a TRUE/FALSE vector indicating whether the person is alive this year
#' @return pMI2 is the vector of probabilities of having a second MI for all
#' patients this year

Second_MI_UKPDS_82 <- function(
    population_,
    parameters_,
    treatment_,
    alive_){
  #estimate the fitted value from the UKPDS OMv2 from the regression
  #N.B. Data relevant data transformations 
  #eGFR values are divided by 10 in UKPDS 82
  #SBP values are divided by 10 in UKPDS 82
  #eGFR values are divided by 10 in UKPDS 82
  #Heart Rate values are divided by 10 in UKPDS 82
  #LDL cholesterol values are multiplied by 10 in UKPDS 82
  #HDL cholesterol values are multiplied by 10 in UKPDS 82
  Fitted_value <-
    parameters_[,"Diab_2ndMI_Lambda"]+
    parameters_[,"Diab_2ndMI_LDL"]*(population_[,"LDL"][alive_]*10)+
    parameters_[,"Diab_2ndMI_micro_macroalbumuria"]*population_[,"MMALB_E"][alive_]+
    parameters_[,"Diab_2ndMI_micro_macroalbumuria"]*population_[,"MMALB_H"][alive_]
  
  #calculate the probability of someone's first CHF
  Haz_this_year <- exp(Fitted_value)*(population_[,"DIAB_DUR"][alive_])
  Haz_next_year <- exp(Fitted_value)*(population_[,"DIAB_DUR"][alive_]+1)
  pMI2 <-  1 - exp (Haz_this_year - Haz_next_year)
  
  #set the probability of 2nd MI back to 0 for people without a first MI or who
  #already have a 2nd MI 
  pMI2 <- ifelse(population_[,"MI_H"][alive_]==0|population_[,"MI2_H"][alive_]==1,0,pMI2)
  
  #return the probability of having a CHF
  return(pMI2)
}


#' @param population is the population matrix returned by the 
#' build_population_parrallel_premodel function
#' @param parameters is the matrix coef that is returned by the set_parameters 
#' function. This matrix contains all parameters used to predict the shape 
#' parameter of the Weibull time to event function 
#' @param rho is the scale parameter of the Weibull time to event function
#' This is called UKPDS2_rho_STRO1 and is put into the global enviroment by the 
#' set_parameters function
#' @param treatment is a character string that indicates which treatment should 
#' be used. Note, this is only relevant for treatments that directly impact CVD 
#' risks
#' @param alive_ is a TRUE/FALSE vector indicating whether the person is alive this year
#' @return pMI is the vector of probabilities of having a first MI for all
#' patients this year


First_Stroke_UKPDS_82 <- function(
    population_,
    parameters_,
    rho_,
    treatment_,
    alive_){
  #estimate the fitted value from the UKPDS OMv2 from the regression
  #N.B. Data relevant data transformations 
  #eGFR values are divided by 10 in UKPDS 82
  #SBP values are divided by 10 in UKPDS 82
  #eGFR values are divided by 10 in UKPDS 82
  #Heart Rate values are divided by 10 in UKPDS 82
  #LDL cholesterol values are multiplied by 10 in UKPDS 82
  #HDL cholesterol values are multiplied by 10 in UKPDS 82
  Fitted_value <-
    parameters_[,"Diab_1stStroke_Lambda"]+
    parameters_[,"Diab_1stStroke_AGE_DIAG"]*(population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
    parameters_[,"Diab_1stStroke_FEMALE"]*population_[,"FEMALE"][alive_]+
    parameters_[,"Diab_1stStroke_ATFIB"]*population_[,"ATFIB_E"][alive_]+
    parameters_[,"Diab_1stStroke_ATFIB"]*population_[,"ATFIB_H"][alive_]+
    parameters_[,"Diab_1stStroke_eGFR_lessthan_60"]*(population_[,"eGFR_U_60"][alive_]/10)+
    parameters_[,"Diab_1stStroke_HbA1c"]*population_[,"HBA"][alive_]+
    parameters_[,"Diab_1stStroke_LDL"]*(population_[,"LDL"][alive_]*10)+
    parameters_[,"Diab_1stStroke_micro_macroalbumuria"]*population_[,"MMALB_E"][alive_]+
    parameters_[,"Diab_1stStroke_micro_macroalbumuria"]*population_[,"MMALB_H"][alive_]+
    parameters_[,"Diab_1stStroke_SBP"]*(population_[,"SBP"][alive_]/10)+
    parameters_[,"Diab_1stStroke_WBC"]*population_[,"WBC"][alive_]+
    parameters_[,"Diab_1stStroke_AMP_HIST"]*population_[,"AMP_H"][alive_]+
    parameters_[,"Diab_1stStroke_IHD_HIST"]*population_[,"IHD_H"][alive_]
  
  #calculate the probability of someone's first CHF
  Haz_this_year <- exp(Fitted_value)*(population_[,"DIAB_DUR"][alive_]^rho_)
  Haz_next_year <- exp(Fitted_value)*((population_[,"DIAB_DUR"][alive_]+1)^rho_)
  pSTRO <-  1 - exp (Haz_this_year - Haz_next_year)
  
  #set the probability of first CHF back to 0 for people with CHF 
  #history
  pSTRO <- ifelse(population_[,"STRO_H"][alive_]==1,0,pSTRO)
  
  #return the probability of having a CHF
  return(pSTRO)
}

#' @param population is the population matrix returned by the 
#' build_population_parrallel_premodel function
#' @param parameters is the matrix coef that is returned by the set_parameters 
#' function. This matrix contains all parameters used to predict the shape 
#' parameter of the Weibull time to event function 
#' @param rho is the scale parameter of the Weibull time to event function
#' This is called UKPDS2_rho_STRO2 and is put into the global enviroment by the 
#' set_parameters function
#' @param treatment is a character string that indicates which treatment should 
#' be used. Note, this is only relevant for treatments that directly impact CVD 
#' risks
#' @param alive_ is a TRUE/FALSE vector indicating whether the person is alive this year
#' @return pMI is the vector of probabilities of having a first MI for all
#' patients this year


Second_Stroke_UKPDS_82 <- function(
    population_,
    parameters_,
    rho_,
    treatment_, 
    alive_){
  #estimate the fitted value from the UKPDS OMv2 from the regression
  #N.B. Data relevant data transformations 
  #eGFR values are divided by 10 in UKPDS 82
  #SBP values are divided by 10 in UKPDS 82
  #eGFR values are divided by 10 in UKPDS 82
  #Heart Rate values are divided by 10 in UKPDS 82
  #LDL cholesterol values are multiplied by 10 in UKPDS 82
  #HDL cholesterol values are multiplied by 10 in UKPDS 82
  Fitted_value <-
    parameters_[,"Diab_2ndStroke_Lambda"]+
    parameters_[,"Diab_2ndStroke_age_diag"]*(population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
    parameters_[,"Diab_2ndStroke_micro_macroalbumuria"]*population_[,"MMALB_E"][alive_]+
    parameters_[,"Diab_2ndStroke_micro_macroalbumuria"]*population_[,"MMALB_H"][alive_]+
    parameters_[,"Diab_2ndStroke_Smoking"]*population_[,"SMO"][alive_]
  
  #calculate the probability of someone's first CHF
  Haz_this_year <- exp(Fitted_value)*(population_[,"DIAB_DUR"][alive_]^rho_)
  Haz_next_year <- exp(Fitted_value)*((population_[,"DIAB_DUR"][alive_]+1)^rho_)
  pSTRO2 <-  1 - exp (Haz_this_year - Haz_next_year)
  
  #set the probability of first second stroke back to 0 for people who already have 
  #had a 2nd stroke or people without a first stroke
  pSTRO2 <- ifelse(population_[,"STRO_H"][alive_]==0|population_[,"STRO2_H"][alive_]==1,0,pSTRO2)
  
  #return the probability of having a 2nd Stroke
  return(pSTRO2)
}

#### Microvascular risk functions###############################################
#' @param population is the population matrix returned by the 
#' build_population_parrallel_premodel function
#' @param parameters is the matrix coef that is returned by the set_parameters 
#' function. This matrix contains all parameters used to predict the shape 
#' parameter of the Exponential time to event function 
#' @param treatment is a character string that indicates which treatment should 
#' be used. Note, this is only relevant for treatments that directly impact CVD 
#' risks
#' @param alive_ is a TRUE/FALSE vector indicating whether the person is alive this year
#' @return pBLIND is the vector of probabilities of becoming blind for all
#' patients this year

Blindness_UKPDS_82 <- function(
    population_,
    parameters_,
    treatment_,
    alive_){
  #estimate the fitted value from the UKPDS OMv2 from the regression
  #N.B. Data relevant data transformations 
  #eGFR values are divided by 10 in UKPDS 82
  #SBP values are divided by 10 in UKPDS 82
  #eGFR values are divided by 10 in UKPDS 82
  #Heart Rate values are divided by 10 in UKPDS 82
  #LDL cholesterol values are multiplied by 10 in UKPDS 82
  #HDL cholesterol values are multiplied by 10 in UKPDS 82
  Fitted_value <-
    parameters_[,"Diab_blindness_lambda"]+
    parameters_[,"Diab_blindness_age_diagnosed"]*(population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
    parameters_[,"Diab_blindness_HbA1c"]*population_[,"HBA"][alive_]+
    parameters_[,"Diab_blindness_heart_rate"]*(population_[,"HEART_R"][alive_]/10)+
    parameters_[,"Diab_blindness_systolic_blood_pressure"]*(population_[,"SBP"][alive_]/10)+
    parameters_[,"Diab_blindness_WBC"]*population_[,"WBC"][alive_]+
    parameters_[,"Diab_blindness_CHF_HIST"]*population_[,"CHF_H"][alive_]+
    parameters_[,"Diab_blindness_IHD_HIST"]*population_[,"IHD_H"][alive_]
  
  #calculate the probability of someone's first CHF
  Haz_this_year <- exp(Fitted_value)*(population_[,"DIAB_DUR"][alive_])
  Haz_next_year <- exp(Fitted_value)*(population_[,"DIAB_DUR"][alive_]+1)
  pBLIND <-  1 - exp (Haz_this_year - Haz_next_year)
  
  #set the probability of blindness back to 0 for people who are blind 
  pBLIND <- ifelse(population_[,"BLIND_H"][alive_]==1,0,pBLIND)
  
  #return the probability of having a CHF
  return(pBLIND)
}

#' @param population is the population matrix returned by the 
#' build_population_parrallel_premodel function
#' @param parameters is the matrix coef that is returned by the set_parameters 
#' function. This matrix contains all parameters used to predict the fitted value 
#' from the logistic regression 
#' @param treatment is a character string that indicates which treatment should 
#' be used. Note, this is only relevant for treatments that directly impact CVD 
#' risks
#' @param alive_ is a TRUE/FALSE vector indicating whether the person is alive this year
#' @return pULCER is the vector of probabilities of becoming blind for all
#' patients this year

Ulcer_UKPDS_82 <- function(
    population_,
    parameters_,
    treatment_,
    alive_){
  #estimate the fitted value from the UKPDS OMv2 from the regression
  #N.B. Data relevant data transformations 
  #eGFR values are divided by 10 in UKPDS 82
  #SBP values are divided by 10 in UKPDS 82
  #eGFR values are divided by 10 in UKPDS 82
  #Heart Rate values are divided by 10 in UKPDS 82
  #LDL cholesterol values are multiplied by 10 in UKPDS 82
  #HDL cholesterol values are multiplied by 10 in UKPDS 82
  Fitted_value <-
    parameters_[,"Diab_ulcer_lambda"]+
    parameters_[,"Diab_ulcer_age_diagnosed"]*(population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
    parameters_[,"Diab_ulcer_female"]*population_[,"FEMALE"][alive_]+
    parameters_[,"Diab_ulcer_BMI"]*population_[,"BMI"][alive_]+
    parameters_[,"Diab_ulcer_HbA1c"]*population_[,"HBA"][alive_]+
    parameters_[,"Diab_ulcer_PVD"]*population_[,"PVD_E"][alive_]+
    parameters_[,"Diab_ulcer_PVD"]*population_[,"PVD_H"][alive_]
  
  #calculate the probability of someone's first CHF
  pULCER <- exp(Fitted_value) / (1+exp(Fitted_value))
  
  #set the probability of ulcer back to 0 for people who are have a history of
  #ulcers
  pULCER <- ifelse(population_[,"ULCER_H"][alive_]==1,0,pULCER)
  
  #return the probability of having a CHF
  return(pULCER)
}


#' @param population is the population matrix returned by the 
#' build_population_parrallel_premodel function
#' @param parameters is the matrix coef that is returned by the set_parameters 
#' function. This matrix contains all parameters used to predict the shape 
#' parameter of the Weibull time to event function 
#' @param rho is the scale parameter of the Weibull time to event function
#' This is called UKPDS2_rho_amp1_noulcer and is put into the global enviroment 
#' by the set_parameters function
#' @param treatment is a character string that indicates which treatment should 
#' be used. Note, this is only relevant for treatments that directly impact CVD 
#' risks
#' @param alive_ is a TRUE/FALSE vector indicating whether the person is alive this year
#' @return pAMP is the vector of probabilities of having a first amputation for 
#' all patients this year

First_Amputation_noUlcer_UKPDS_82 <- function(
    population_,
    parameters_,
    rho_,
    treatment_,
    alive_){
  #estimate the fitted value from the UKPDS OMv2 from the regression
  #N.B. Data relevant data transformations 
  #eGFR values are divided by 10 in UKPDS 82
  #SBP values are divided by 10 in UKPDS 82
  #eGFR values are divided by 10 in UKPDS 82
  #Heart Rate values are divided by 10 in UKPDS 82
  #LDL cholesterol values are multiplied by 10 in UKPDS 82
  #HDL cholesterol values are multiplied by 10 in UKPDS 82
  Fitted_value <-
    parameters_[,"Diab_amp1_no_ulcer_lambda"]+
    parameters_[,"Diab_amp1_no_ulcer_agediagnosed"]*(population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
    parameters_[,"Diab_amp1_no_ulcer_female"]*population_[,"FEMALE"][alive_]+
    parameters_[,"Diab_amp1_no_ulcer_ATFIB"]*population_[,"ATFIB_E"][alive_]+
    parameters_[,"Diab_amp1_no_ulcer_ATFIB"]*population_[,"ATFIB_H"][alive_]+
    parameters_[,"Diab_amp1_no_ulcer_HbA1c"]*population_[,"HBA"][alive_]+
    parameters_[,"Diab_amp1_no_ulcer_HDL"]*(population_[,"HDL"][alive_]*10)+
    parameters_[,"Diab_amp1_no_ulcer_heartrate"]*(population_[,"HEART_R"][alive_]/10)+
    parameters_[,"Diab_amp1_no_ulcer_micro_macroalbumuria"]*population_[,"MMALB_E"][alive_]+
    parameters_[,"Diab_amp1_no_ulcer_micro_macroalbumuria"]*population_[,"MMALB_H"][alive_]+
    parameters_[,"Diab_amp1_no_ulcer_PVD"]*population_[,"PVD_E"][alive_]+
    parameters_[,"Diab_amp1_no_ulcer_PVD"]*population_[,"PVD_H"][alive_]+
    parameters_[,"Diab_amp1_no_ulcer_SBP"]*(population_[,"SBP"][alive_]/10)+
    parameters_[,"Diab_amp1_no_ulcer_WBC"]*population_[,"WBC"][alive_]+
    parameters_[,"Diab_amp1_no_ulcer_stroke_hist"]*population_[,"STRO_H"][alive_]
  
  #calculate the probability of someone's first CHF
  Haz_this_year <- exp(Fitted_value)*(population_[,"DIAB_DUR"][alive_]^rho_)
  Haz_next_year <- exp(Fitted_value)*((population_[,"DIAB_DUR"]+1)[alive_]^rho_)
  pAMP<-  1 - exp (Haz_this_year - Haz_next_year)
  
  #set the probability of blindness back to 0 for people who are blind 
  pAMP <- ifelse(population_[,"AMP_H"][alive_]==1,0,pAMP)
  
  #return the probability of having a CHF
  return(pAMP)
}

#' @param population is the population matrix returned by the 
#' build_population_parrallel_premodel function
#' @param parameters is the matrix coef that is returned by the set_parameters 
#' function. This matrix contains all parameters used to predict the shape 
#' parameter of the Weibull time to event function 
#' @param rho is the scale parameter of the Weibull time to event function
#' @param treatment is a character string that indicates which treatment should 
#' be used. Note, this is only relevant for treatments that directly impact CVD 
#' risks
#' @param alive_ is a TRUE/FALSE vector indicating whether the person is alive this year
#' @return pAMP is the vector of probabilities of having a first amputation for 
#' all patients this year

First_Amputation_Ulcer_UKPDS_82 <- function(
    population_,
    parameters_,
    treatment_,
    alive_){
  #estimate the fitted value from the UKPDS OMv2 from the regression
  #N.B. Data relevant data transformations 
  #eGFR values are divided by 10 in UKPDS 82
  #SBP values are divided by 10 in UKPDS 82
  #eGFR values are divided by 10 in UKPDS 82
  #Heart Rate values are divided by 10 in UKPDS 82
  #LDL cholesterol values are multiplied by 10 in UKPDS 82
  #HDL cholesterol values are multiplied by 10 in UKPDS 82
  Fitted_value <-
    parameters_[,"Diab_amp1_prev_ulcer_lambda"]+
    parameters_[,"Diab_amp1_prev_ulcer_age_diagnosed"]*(population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
    parameters_[,"Diab_amp1_prev_ulcer_PVD"]*population_[,"PVD_E"][alive_]+
    parameters_[,"Diab_amp1_prev_ulcer_PVD"]*population_[,"PVD_H"][alive_]
  
  #calculate the probability of someone's first CHF
  Haz_this_year <- exp(Fitted_value)*(population_[,"DIAB_DUR"][alive_])
  Haz_next_year <- exp(Fitted_value)*(population_[,"DIAB_DUR"][alive_]+1)
  pAMP<-  1 - exp (Haz_this_year - Haz_next_year)
  
  #set the probability of blindness back to 0 for people who are blind 
  pAMP <- ifelse(population_[,"AMP_H"][alive_]==1,0,pAMP)
  
  #return the probability of having a CHF
  return(pAMP)
}

#' @param population is the population matrix returned by the 
#' build_population_parrallel_premodel function
#' @param parameters is the matrix coef that is returned by the set_parameters 
#' function. This matrix contains all parameters used to predict the shape 
#' parameter of the exponential time to event function 
#' @param treatment is a character string that indicates which treatment should 
#' be used. Note, this is only relevant for treatments that directly impact CVD 
#' risks
#' @param alive_ is a TRUE/FALSE vector indicating whether the person is alive this year
#' @return pAMP2 is the vector of probabilities of having a second amputation for 
#' all patients this year

First_Amputation_UKPDS_82 <- function(
    population_,
    parameters_,
    rho_first_amp_noulcer_,
    treatment_,
    alive_){
  #Make a logical vector indicating if each patient has an ulcer or not
  #N.B. people who are dead are removed in the First_Amputation_Ulcer_UKPDS_82 
  #and First_Amputation_noUlcer_UKPDS_82 risk functions
  ulcer <- (population_[,"ULCER_H"][alive_]==1|population_[,"ULCER_E"][alive_]==1)
  
  #create temporary varaibles in the population who are alive for their amputation
  #risk with and without ulcer
  pAMP_ulcer <- First_Amputation_Ulcer_UKPDS_82(population_,
                                                parameters_,
                                                treatment_,
                                                alive_)
  
  pAMP_noulcer <- First_Amputation_noUlcer_UKPDS_82(population_,
                                                    parameters_,
                                                    parameters_[,"Diab_amp1_no_ulcer_rho"],
                                                    treatment_,
                                                    alive_)
  #give people with or without ulcers the correct amputaion risk
  pAMP <-  ifelse(ulcer, 
                  pAMP_ulcer,
                  pAMP_noulcer)
  
  #just as a final check, set pAMP to 0 for anyone who has already had 1 amputation
  #note this should do nothing as this is also applied in both the 
  #First_Amputation_Ulcer_UKPDS_82 & First_Amputation_noUlcer_UKPDS_82 functions
  
  pAMP <- ifelse(population_[,"AMP_H"][alive_]==1, 0, pAMP)
  
  #remove temporary variables generated in this code
  rm(ulcer, pAMP_ulcer, pAMP_noulcer)
  
  return(pAMP)
  
}

#' @param population is the population matrix returned by the 
#' build_population_parrallel_premodel function
#' @param parameters is the matrix coef that is returned by the set_parameters 
#' function. This matrix contains all parameters used to predict the shape 
#' parameter of the exponential time to event function 
#' @param treatment is a character string that indicates which treatment should 
#' be used. Note, this is only relevant for treatments that directly impact CVD 
#' risks
#' @param alive_ is a TRUE/FALSE vector indicating whether the person is alive this year
#' @return pAMP2 is the vector of probabilities of having a second amputation for 
#' all patients this year

Second_Amputation_UKPDS_82 <- function(
    population_,
    parameters_,
    treatment_,
    alive_){
  #estimate the fitted value from the UKPDS OMv2 from the regression
  #N.B. Data relevant data transformations 
  #eGFR values are divided by 10 in UKPDS 82
  #SBP values are divided by 10 in UKPDS 82
  #eGFR values are divided by 10 in UKPDS 82
  #Heart Rate values are divided by 10 in UKPDS 82
  #LDL cholesterol values are multiplied by 10 in UKPDS 82
  #HDL cholesterol values are multiplied by 10 in UKPDS 82
  Fitted_value <-
    parameters_[,"Diab_amp2_lambda"]+
    parameters_[,"Diab_amp2_HbA1c"]*population_[,"HBA"][alive_]
  
  #calculate the probability of someone's first CHF
  Haz_this_year <- exp(Fitted_value)*(population_[,"DIAB_DUR"][alive_])
  Haz_next_year <- exp(Fitted_value)*(population_[,"DIAB_DUR"][alive_]+1)
  pAMP2<-  1 - exp (Haz_this_year - Haz_next_year)
  
  #set the probability of 2nd amputation back to 0 for people who
  #haven't had a first amputation or
  #have already had a second amputation 
  pAMP2 <- ifelse(population_[,"AMP_H"][alive_]==0 | population_[,"AMP2_H"][alive_]==1,0,pAMP2)
  
  #return the probability of having a 2nd amputation
  return(pAMP2)
}

#' @param population_ is the population matrix returned by the 
#' build_population_parrallel_premodel function
#' @param parameters_ is the matrix coef that is returned by the set_parameters 
#' function. This matrix contains all parameters used to predict the shape 
#' parameter of the exponential time to event function 
#' @param treatment_ is a character string that indicates which treatment should 
#' be used. Note, this is only relevant for treatments that directly impact CVD 
#' risks
#' @param alive_ is a TRUE/FALSE vector indicating whether the person is alive this year
#' @return pAMP2 is the vector of probabilities of having a second amputation for 
#' all patients this year

Renal_UKPDS_82 <- function(
    population_,
    parameters_,
    treatment_,
    alive_){
  #estimate the fitted value from the UKPDS OMv2 from the regression
  #N.B. Data relevant data transformations 
  #eGFR values are divided by 10 in UKPDS 82
  #SBP values are divided by 10 in UKPDS 82
  #eGFR values are divided by 10 in UKPDS 82
  #Heart Rate values are divided by 10 in UKPDS 82
  #LDL cholesterol values are multiplied by 10 in UKPDS 82
  #HDL cholesterol values are multiplied by 10 in UKPDS 82
  Fitted_value <-
    parameters_[,"Diab_renal_fail_lambda"]+
    parameters_[,"Diab_renal_fail_affro"]*population_[,"AFRO"][alive_]+
    parameters_[,"Diab_renal_fail_AGE_DIAG"]*(population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
    parameters_[,"Diab_renal_fail_FEMALE"]*population_[,"FEMALE"][alive_]+
    parameters_[,"Diab_renal_fail_BMI"]*population_[,"BMI"][alive_]+
    parameters_[,"Diab_renal_fail_EGFR_lessthan_60"]*(population_[,"eGFR_U_60"][alive_]/10)+
    parameters_[,"Diab_renal_fail_EGFR_greaterthan_60"]*(population_[,"eGFR_O_60"][alive_]/10)+
    parameters_[,"Diab_renal_fail_haemoglobin"]*population_[,"HAEM"][alive_]+
    parameters_[,"Diab_renal_fail_LDL"]*(population_[,"LDL"][alive_]*10)+
    parameters_[,"Diab_renal_fail_micro_macroalbumuria"]*population_[,"MMALB_E"][alive_]+
    parameters_[,"Diab_renal_fail_micro_macroalbumuria"]*population_[,"MMALB_H"][alive_]+
    parameters_[,"Diab_renal_fail_SBP"]*(population_[,"SBP"][alive_]/10)+
    parameters_[,"Diab_renal_fail_WBC"]*population_[,"WBC"][alive_]+
    parameters_[,"Diab_renal_fail_AMPHIST"]*population_[,"AMP_H"][alive_]+
    parameters_[,"Diab_renal_fail_BLINDHIST"]*population_[,"BLIND_H"][alive_]
    
  
  #calculate the probability of someone developing renal failure
  Haz_this_year <- exp(Fitted_value)*(population_[,"DIAB_DUR"][alive_])
  Haz_next_year <- exp(Fitted_value)*(population_[,"DIAB_DUR"][alive_]+1)
  pRENAL<-  1 - exp (Haz_this_year - Haz_next_year)
  
  #set the probability of renal failure back to 0 for people who
  #are already in renal failure
  pRENAL <- ifelse(population_[,"RENAL_H"][alive_]==1 ,0, pRENAL)
  
  #return the probability of having a 2nd amputation
  return(pRENAL)
}

##### Death risk functions ####################################################

#' @param population_ is the population matrix returned by the 
#' build_population_parrallel_premodel function
#' @param parameters_ is the matrix coef that is returned by the set_parameters 
#' function. This matrix contains all parameters used to predict the shape 
#' parameter of the exponential time to event function 
#' @param treatment_ is a character string that indicates which treatment should 
#' be used. Note, this is only relevant for treatments that directly impact CVD 
#' risks
#' @param phi_ is the scale parameter for the Gompertz function
#' @param alive_ is a TRUE/FALSE vector indicating whether the person is alive this year
#' @return pDEATH is the vector of probabilities of dying from any cause this 
#' this year


Death_NoEvent_NoHist_UKPDS_82 <- function(
    population_,
    parameters_,
    phi_,
    treatment_,
    alive_){
  
  #estimate the fitted value from the UKPDS OMv2 from the regression
  #N.B. Data relevant data transformations 
  #eGFR values are divided by 10 in UKPDS 82
  #SBP values are divided by 10 in UKPDS 82
  #eGFR values are divided by 10 in UKPDS 82
  #Heart Rate values are divided by 10 in UKPDS 82
  #LDL cholesterol values are multiplied by 10 in UKPDS 82
  #HDL cholesterol values are multiplied by 10 in UKPDS 82
  
  
  Fitted_value <-
    parameters_[,"Diab_Mort_Nohist_Noevent_Lambda"]+
    parameters_[,"Diab_Mort_Nohist_Noevent_Female"]*population_[,"FEMALE"][alive_]+
    parameters_[,"Diab_Mort_Nohist_Noevent_Smoker"]*population_[,"SMO"][alive_]
  
  #Calculate the probability of death
  Haz_this_year <- (1/phi_)*exp(Fitted_value)*(exp(phi_*population_[,"AGE"][alive_])-1)
  Haz_next_year <- (1/phi_)*exp(Fitted_value)*(exp(phi_*(population_[,"AGE"][alive_]+1))-1)
  pDEATH <- 1 - exp(Haz_this_year-Haz_next_year)
  
  #return the probability of death
  return(pDEATH)
}


#' @param population is the population matrix returned by the 
#' build_population_parrallel_premodel function
#' @param parameters is the matrix coef that is returned by the set_parameters 
#' function. This matrix contains all parameters used to predict the shape 
#' parameter of the exponential time to event function 
#' @param phi is the scale parameter of the Gompertz distribution
#' This is called UKPDS2_phi_NE_NH and is put into the global enviroment 
#' by the set_parameters function
#' @param treatment is a character string that indicates which treatment should 
#' be used. Note, this is only relevant for treatments that directly impact CVD 
#' risks
#' @param alive_ is a TRUE/FALSE vector indicating whether the person is alive this year
#' @return pDEATH is the vector of probabilities of dying from any cause this 
#' this year


Death_NoEvent_Hist_UKPDS_82 <- function(
    population_,
    parameters_,
    phi_,
    treatment_,
    alive_){
  
  #estimate the fitted value from the UKPDS OMv2 from the regression
  #N.B. Data relevant data transformations 
  #eGFR values are divided by 10 in UKPDS 82
  #SBP values are divided by 10 in UKPDS 82
  #eGFR values are divided by 10 in UKPDS 82
  #Heart Rate values are divided by 10 in UKPDS 82
  #LDL cholesterol values are multiplied by 10 in UKPDS 82
  #HDL cholesterol values are multiplied by 10 in UKPDS 82
  
  Fitted_value <-
    parameters_[,"Diab_Mort_Hist_NoEvent_Lambda"]+
    parameters_[,"Diab_Mort_Hist_NoEvent_BMI_u_18.5"]*population_[,"BMI_U_18_5"][alive_]+
    parameters_[,"Diab_Mort_Hist_NoEvent_BMI_O_25"]*population_[,"BMI_O_E_25"][alive_]+
    parameters_[,"Diab_Mort_Hist_NoEvent_MMALB"]*population_[,"MMALB_E"][alive_]+
    parameters_[,"Diab_Mort_Hist_NoEvent_MMALB"]*population_[,"MMALB_H"][alive_]+
    parameters_[,"Diab_Mort_Hist_NoEvent_SMOKER"]*population_[,"SMO"][alive_]+
    parameters_[,"Diab_Mort_Hist_NoEvent_WhiteBloodCellCount"]*population_[,"WBC"][alive_]+
    parameters_[,"Diab_Mort_Hist_NoEvent_AMPHIST"]*population_[,"AMP_H"][alive_]+
    parameters_[,"Diab_Mort_Hist_NoEvent_CHFHIST"]*population_[,"CHF_H"][alive_]+
    parameters_[,"Diab_Mort_Hist_NoEvent_RENALHIST"]*population_[,"RENAL_H"][alive_]+
    parameters_[,"Diab_Mort_Hist_NoEvent_STROKEHIST"]*population_[,"STRO_H"][alive_]
  
  #Calculate the probability of death
  Haz_this_year <- (1/phi_)*exp(Fitted_value)*(exp(phi_*population_[,"AGE"][alive_])-1)
  Haz_next_year <- (1/phi_)*exp(Fitted_value)*(exp(phi_*(population_[,"AGE"][alive_]+1))-1)
  pDEATH <- 1 - exp(Haz_this_year-Haz_next_year)
  
  #return the probability of death
  return(pDEATH)
}

#' @param population is the population matrix returned by the 
#' build_population_parrallel_premodel function
#' @param parameters is the matrix coef that is returned by the set_parameters 
#' function. This matrix contains all parameters used to predict the fitted 
#' value from the logistic regression 
#' @param treatment is a character string that indicates which treatment should 
#' be used. Note, this is only relevant for treatments that directly impact CVD 
#' risks
#' @param alive_ is a TRUE/FALSE vector indicating whether the person is alive this year
#' @return pDEATH is the vector of probabilities of dying from any cause this 
#' this year


Death_Event_NoHist_UKPDS_82 <- function(
    population_,
    parameters_,
    treatment_,
    alive_){
  
  #estimate the fitted value from the UKPDS OMv2 from the regression
  #N.B. Data relevant data transformations 
  #eGFR values are divided by 10 in UKPDS 82
  #SBP values are divided by 10 in UKPDS 82
  #eGFR values are divided by 10 in UKPDS 82
  #Heart Rate values are divided by 10 in UKPDS 82
  #LDL cholesterol values are multiplied by 10 in UKPDS 82
  #HDL cholesterol values are multiplied by 10 in UKPDS 82
  
  Fitted_value <-
    parameters_[,"Diab_Mort_NoHist_EventYear_Lambda"]+
    parameters_[,"Diab_Mort_NoHist_EventYear_Indian"]*population_[,"INDIAN"][alive_]+
    parameters_[,"Diab_Mort_NoHist_EventYear_DiabetesDuration"]*population_[,"DIAB_DUR"][alive_]+
    parameters_[,"Diab_Mort_NoHist_EventYear_CurrentAge"]*population_[,"AGE"][alive_]+
    parameters_[,"Diab_Mort_NoHist_EventYear_HeartRate"]*(population_[,"HEART_R"][alive_]/10)+
    parameters_[,"Diab_Mort_NoHist_EventYear_PeripheralVascularDisease"]*population_[,"PVD_E"][alive_]+
    parameters_[,"Diab_Mort_NoHist_EventYear_PeripheralVascularDisease"]*population_[,"PVD_H"][alive_]+
    parameters_[,"Diab_Mort_NoHist_EventYear_Smoker"]*population_[,"SMO"][alive_]+
    parameters_[,"Diab_Mort_NoHist_EventYear_AmputationEvent"]*population_[,"AMP_E"][alive_]+
    parameters_[,"Diab_Mort_NoHist_EventYear_IHDEvent"]*population_[,"IHD_E"][alive_]+
    parameters_[,"Diab_Mort_NoHist_EventYear_MIEvent"]*population_[,"MI_E"][alive_]+
    parameters_[,"Diab_Mort_NoHist_EventYear_RenalFailureEvent"]*population_[,"RENAL_E"][alive_]+
    parameters_[,"Diab_Mort_NoHist_EventYear_StrokeEvent"]*population_[,"STRO_E"][alive_]
  
  #Calculate the probability of survival
  pSURV <- exp(-Fitted_value)/(1+exp(-Fitted_value))
  #Calculate the probability of death
  pDEATH <- 1 - pSURV
  #return the probability of death
  return(pDEATH)
}

#' @param population is the population matrix returned by the 
#' build_population_parrallel_premodel function
#' @param parameters is the matrix coef that is returned by the set_parameters 
#' function. This matrix contains all parameters used to predict the fitted 
#' value from the logistic regression 
#' @param treatment is a character string that indicates which treatment should 
#' be used. Note, this is only relevant for treatments that directly impact CVD 
#' risks
#' @param alive_ is a TRUE/FALSE vector indicating whether the person is alive this year
#' @return pDEATH is the vector of probabilities of dying from any cause this 
#' this year

Death_Event_Hist_UKPDS_82 <- function(
    population_,
    parameters_,
    treatment_,
    alive_){
  
  #estimate the fitted value from the UKPDS OMv2 from the regression
  #N.B. Data relevant data transformations 
  #eGFR values are divided by 10 in UKPDS 82
  #SBP values are divided by 10 in UKPDS 82
  #eGFR values are divided by 10 in UKPDS 82
  #Heart Rate values are divided by 10 in UKPDS 82
  #LDL cholesterol values are multiplied by 10 in UKPDS 82
  #HDL cholesterol values are multiplied by 10 in UKPDS 82
  
  Fitted_value <-
    parameters_[,"Diab_Mort_Hist_EventYear_Lambda"]+
    parameters_[,"Diab_Mort_Hist_EventYear_ATFIB"]*population_[,"ATFIB_E"][alive_]+
    parameters_[,"Diab_Mort_Hist_EventYear_ATFIB"]*population_[,"ATFIB_H"][alive_]+
    parameters_[,"Diab_Mort_Hist_EventYear_CurrentAge"]*population_[,"AGE"][alive_]+
    parameters_[,"Diab_Mort_Hist_EventYear_HDL"]*(population_[,"HDL"][alive_]*10)+
    parameters_[,"Diab_Mort_Hist_EventYear_PVD"]*population_[,"PVD_E"][alive_]+
    parameters_[,"Diab_Mort_Hist_EventYear_PVD"]*population_[,"PVD_H"][alive_]+
    parameters_[,"Diab_Mort_Hist_EventYear_WBC"]*population_[,"WBC"][alive_]+
    parameters_[,"Diab_Mort_Hist_EventYear_AmpEvent"]*population_[,"AMP_E"][alive_]+
    parameters_[,"Diab_Mort_Hist_EventYear_AmpHist"]*population_[,"AMP_H"][alive_]+
    parameters_[,"Diab_Mort_Hist_EventYear_2ndAmpEvent"]*population_[,"AMP2_E"][alive_]+
    parameters_[,"Diab_Mort_Hist_EventYear_IHDEvent"]*population_[,"IHD_E"][alive_]+
    parameters_[,"Diab_Mort_Hist_HistYear_IHDHist"]*population_[,"IHD_H"][alive_]+
    parameters_[,"Diab_Mort_Hist_EventYear_MIEvent"]*population_[,"MI_E"][alive_]+
    parameters_[,"Diab_Mort_Hist_EventYear_MIEvent"]*population_[,"MI2_E"][alive_]+
    parameters_[,"Diab_Mort_Hist_EventYear_MIHist"]*population_[,"MI_H"][alive_]+
    parameters_[,"Diab_Mort_Hist_EventYear_RenalHist"]*population_[,"RENAL_H"][alive_]+
    parameters_[,"Diab_Mort_Hist_EventYear_StrokeEvent"]*population_[,"STRO_E"][alive_]+
    parameters_[,"Diab_Mort_Hist_EventYear_StrokeEvent"]*population_[,"STRO2_E"][alive_]
  
  #Calculate the probability of survival
  pSURV <- exp(-Fitted_value)/(1+exp(-Fitted_value))
  
  #Calculate the probability of death
  pDEATH <- 1 - pSURV
  
  #remove temporary variables
  rm(pSURV, Fitted_value)
  
  #return the probability of death
  return(pDEATH)
}
###############################################################################