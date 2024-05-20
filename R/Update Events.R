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





#' @param population_ is the population matrix returned by the 
#' build_population_parrallel_premodel function
#' @param parameters_ is the row of the parameters matrix that corresponds to this
#' model run
#' @param treatment_ is a character string that indicates which treatment should 
#' be used. Note, this is only relevant for treatments that directly impact CVD 
#' risks
#' @param SOUR_ is the current second order uncertainty (PSA) run
#' @param Year_ is the current simulation year, this is used to record time of death
#' @param alive_ is a vector of TRUE/FALSES indicating whether a patient is alive
#' @param random_numbs_ is an array of common random numbers giving the same random number
#' @param LifeTables_, is a dataframe containing life table information in formate
#' that can easily be matched to the population matrix
#' draws for each patient for each event in each year
#' or not


update_events_UKPDS82 <- function(population_, parameters_, treatment_, Year_, alive_,random_numbs_, LifeTables_){
  
  #Calculate probabilities, note this will be 0 with a CHF history
  p_CHF <- First_CHF_UKPDS_82(population_,parameters_,parameters_[,"Diab_1stCHF_Rho"],treatment_,alive_)
  #Record events and probs
  population_[,"p_CHF"][alive_] <- p_CHF
  population_[,"CHF_E"][alive_] <- ifelse(random_numbs_[,"CHF",Year_+1][alive_]<p_CHF,1,0)
  
  #Calculate probabilities of IHD, note this will be 0 for those with a IHD history
  p_IHD <- First_IHD_UKPDS_82(population_,parameters_,parameters_[,"Diab_1stIHD_Rho"],treatment_, alive_)
  #Record events and probs
  population_[,"p_IHD"][alive_] <- p_IHD
  population_[,"IHD_E"][alive_] <- ifelse(random_numbs_[,"IHD",Year_+1][alive_]<p_IHD,1,0)
  
  #Calculate probabilities of 1st MI, note this will 0 for those with a history of MI
  p_MI <- First_MI_UKPDS_82(population_, parameters_,parameters_[,"Diab_1stMI_Female_Rho"],treatment_, alive_)
  #Record events and probs
  population_[,"p_MI"][alive_] <- p_MI
  population_[,"MI_E"][alive_] <- ifelse(random_numbs_[,"MI1",Year_+1][alive_]<p_MI,1,0)
  
  #Calculate probabilities of 2nd MI
  p_MI2 <- Second_MI_UKPDS_82(population_, parameters_, treatment_, alive_)
  #Record events and probs
  population_[,"p_MI2"][alive_] <- p_MI2
  population_[,"MI2_E"][alive_] <- ifelse(random_numbs_[,"MI2",Year_+1][alive_]<p_MI2,1,0)
  
  #Calculate probabilities of 1st Stroke
  p_STRO <- First_Stroke_UKPDS_82(population_, parameters_, parameters_[,"Diab_1stStroke_Rho"], treatment_, alive_)
  #Record events and probs
  population_[,"p_STRO"][alive_] <- p_STRO
  population_[,"STRO_E"][alive_] <- ifelse(random_numbs_[,"STRO",Year_+1][alive_]<p_STRO,1,0)
  
  #Calculate probabilities of 2nd Stroke
  p_STRO2 <- Second_Stroke_UKPDS_82(population_, parameters_,parameters_[,"Diab_2ndStroke_Rho"], treatment_, alive_)
  #Record events and probs
  population_[,"p_STRO2"][alive_] <- p_STRO2
  population_[,"STRO2_E"][alive_] <- ifelse(random_numbs_[,"STRO2",Year_+1][alive_]<p_STRO2,1,0)
  
  #Calculate probabilities of Blindness
  p_BLIND <- Blindness_UKPDS_82(population_, parameters_, treatment_, alive_)
  #Record events and probs
  population_[,"p_BLIND"][alive_] <- p_BLIND
  population_[,"BLIND_E"][alive_] <- ifelse(random_numbs_[,"BLIND",Year_+1][alive_]<p_BLIND,1,0)
  
  #Calculate probabilities of developing an ulcer
  p_ULCER <- Ulcer_UKPDS_82(population_,parameters_,treatment_, alive_)
  #Record events and probs
  population_[,"p_ULCER"][alive_] <- p_ULCER
  population_[,"ULCER_E"][alive_] <- ifelse(random_numbs_[,"ULCER",Year_+1][alive_]<p_ULCER,1,0)
  
  #Calculate probabilities of the first amputation
  p_AMP <- First_Amputation_UKPDS_82(population_,parameters_,parameters_[,"Diab_amp1_no_ulcer_rho"], treatment, alive_)
  #Record events and probs
  population_[,"p_amp1"][alive_] <- p_AMP
  population_[,"AMP_E"][alive_] <- ifelse(random_numbs_[,"AMP",Year_+1][alive_]<p_AMP,1,0)
  
  #Calculate the probabilties of the 2nd amputation
  p_AMP2 <- Second_Amputation_UKPDS_82(population_,parameters_, treatment_, alive_)
  #Record events and probs
  population_[,"p_amp2"][alive_] <- p_AMP2
  population_[,"AMP2_E"][alive_] <- ifelse(random_numbs_[,"AMP2",Year_+1][alive_]<p_AMP2,1,0)
  
  #Calculate the probabilties of the 2nd amputation
  p_renal <- Renal_UKPDS_82(population_,parameters_, treatment_, alive_)
  #Record events and probs
  population_[,"p_renal"][alive_] <- p_renal
  population_[,"RENAL_E"][alive_] <- ifelse(random_numbs_[,"RENAL",Year_+1][alive_]<p_renal,1,0)
  
  #Events exclude blindness and ulcer in UKPDS82
  event_year <- ifelse(population_[,"RENAL_E"][alive_]==1|
                         population_[,"AMP2_E"][alive_]==1|
                         population_[,"AMP_E"][alive_]==1|
                         population_[,"STRO2_E"][alive_]==1|
                         population_[,"STRO_E"][alive_]==1|
                         population_[,"MI2_E"][alive_]==1|
                       population_[,"MI_E"][alive_]==1|
                       population_[,"IHD_E"][alive_]==1|
                       population_[,"CHF_E"][alive_]==1,
                       T,
                       F)
  #Histories of events include blindness and ulcers in UKPDS 82
  event_hist <- ifelse(population_[,"RENAL_H"][alive_]==1|
                       population_[,"AMP2_H"][alive_]==1|
                       population_[,"AMP_H"][alive_]==1|
                       population_[,"STRO2_H"][alive_]==1|
                       population_[,"STRO_H"][alive_]==1|
                       population_[,"MI2_H"][alive_]==1|
                       population_[,"MI_H"][alive_]==1|
                       population_[,"IHD_H"][alive_]==1|
                       population_[,"CHF_H"][alive_]==1|
                       population_[,"ULCER_H"][alive_]==1|
                       population_[,"BLIND_H"][alive_]==1,
                       T,
                       F)
  
 #calculate the probability of death based on the event status and history
  p_DEATH <- ifelse(event_year==F&event_hist==F,
                   Death_NoEvent_NoHist_UKPDS_82(population_,parameters_,parameters_[,"Diab_Mort_Nohist_Noevent_Phi"],treatment_, alive_),
            ifelse(event_year==T&event_hist==F,
                   Death_Event_NoHist_UKPDS_82(population_,parameters_,treatment,alive_),
            ifelse(event_year==F&event_hist==T,
                   Death_NoEvent_Hist_UKPDS_82(population_,parameters_,parameters_[,"Diab_Mort_Hist_NoEvent_Phi"],treatment_,alive_),
                   Death_Event_Hist_UKPDS_82(population_,parameters_,treatment_,alive_))))
  #get age-gender matched deaths from the life tables
  p_DEATH_LT <- LifeTableMort(population_, LifeTables_, alive_)
  
  #stop the model if the number of people with mortality data does match from 
  #UKPDS and does not match
  if(length(p_DEATH_LT)!=length(p_DEATH)){
    #push everything to the global enviroment
    for (variable in ls()) {
      assign(variable, get(variable), envir = .GlobalEnv)
    }
    #stop the model with an error message
    stop("the number of people with a prob of death does not match from UKPDS82 
         and the lifetables")
  }
  
  #give each person the highest risk of death, their UKPDS82 ACM mortality or
  #current mortality based on the source of the lifetables
  p_DEATH_final <- ifelse(p_DEATH>=p_DEATH_LT,p_DEATH,p_DEATH_LT)
 

  
 #If the patient dies, record FOTH as this year, otherwise set FOTH to NA
 population_[,"p_DEATH"][alive_] <- p_DEATH_final
 population_[,"F_ALLCAUSE"][alive_] <- ifelse(random_numbs_[,"DEATH",Year_+1][alive_]<=p_DEATH_final,Year_,NA)
 
 #Remove temporary variables
 rm(p_DEATH,event_hist,event_year,p_renal,p_AMP2,p_AMP,p_ULCER,p_BLIND,p_STRO2,
    p_STRO,p_MI2,p_MI,p_IHD,p_CHF, p_DEATH_LT)
 
 #return the population
 return(population_)
}

#' @param population_ is the population matrix
#' @param parameters_ is a single row of the parameters matrix
#' @param random_numbs_ is an array of common random numbers giving the same random number
#' @param Year_, is the current year in the simulation
#' @param alive_, is a TRUE/FALSE vector indicating wether the patient is alive

#' @param Year_ is the current simulation year, this is used to record time of death

update_events_UKPDS90 <- function(population_, parameters_, Year_, alive_,random_numbs_){
  #Update smoking status every 3 years (logistic regression on data collected three yearly)
  if((Year_)%%3==0&Year_!=0){
  p_SMO <- UKPDS_90_smo(population_,parameters_,alive_)
  #Record smoking status
  population_[,"SMO"][alive_] <- ifelse(random_numbs_[,"SMO",Year_+1][alive_]<p_SMO,1,0)
  population_[,"p_SMO"][alive_] <- p_SMO
  #remove temporary variables
  rm(p_SMO)
  }
  #Apply the time to event risk equations
  #Apply the yearly event micro/macroalbuminuria
  p_MMALB <- UKPDS_90_MICALB(population_, parameters_,alive_)
  #set the probability to 0, if they already have a history of MMALB
  p_MMALB <- ifelse(population_[,"MMALB_H"][alive_]==1,0,p_MMALB)
  #record MMALB status
  population_[,"MMALB_E"][alive_] <- ifelse(random_numbs_[,"MMALB",Year_+1][alive_]<p_MMALB,1,0)
  population_[,"p_MMALB"][alive_] <- p_MMALB
  
  p_ATFIB <- UKPDS_90_ATFIB(population_,parameters_,alive_)
  #set the probability to 0, if they already have a history of atrial fibrilliation
  p_ATFIB <- ifelse(population_[,"ATFIB_H"][alive_]==1,0,p_ATFIB)
  #Record atrial fibrillation
  population_[,"ATFIB_E"][alive_] <- ifelse(random_numbs_[,"ATFIB",Year_+1][alive_]<p_ATFIB,1,0)
  population_[,"p_ATFIB"][alive_] <- p_ATFIB
  
  p_PVD <- UKPDS_90_PVD(population_, parameters_,alive_)
  #set the probability to 0, if they already have a history of PVD
  p_PVD <- ifelse(population_[,"PVD_H"][alive_]==1,0,p_PVD)
  #record PVD status
  population_[,"PVD_E"][alive_] <- ifelse(random_numbs_[,"PVD",Year_+1][alive_]<p_PVD,1,0)
  population_[,"p_PVD"][alive_] <- p_PVD
  
  ###eGFR
  #eGFR is under 60
  p_eGFRu60 <- UKPDS_90_binrary_peGFRu60(population_, parameters_, alive_)
  eGFRu60 <- ifelse(random_numbs_[,"eGFRu60",Year_+1][alive_]<p_eGFRu60,1,0)
  #predict eGFR
  population_[,"eGFR"][alive_] <- UKPDS_90_eGFR(population_, parameters_,eGFRu60,alive_)
  population_[,"eGFR_U_60"][alive_] <- ifelse(population_[,"eGFR"][alive_]<60,population_[,"eGFR"][alive_],60)
  population_[,"eGFR_O_60"][alive_] <- ifelse(population_[,"eGFR"][alive_]>60,0,population_[,"eGFR"][alive_]-60)
  
  #remove the temporary variables
  rm(p_MMALB, p_ATFIB, p_PVD,p_eGFRu60,eGFRu60)
  
  return(population_)
}

##'@param population_ is the population matrix
##'@param parameter_ is a row of the parameters matrix
##'@param alive_ is a vector of TRUE and FALSES indicating whether a patient 
##'is alive or not
##'@return pOST is the vector of the probabilities of osteoarthritis

update_events_SPHR_osteoarthritis <- function(population_,
                                              parameters_,
                                              Year_,
                                              alive_,
                                              random_numbs_
){
  #get probability of osteoarthritis
  pOST <- Oesto_SPHR(population_,parameters_,alive_)
  #reset this to 0, if the person has a history of osteoarthritis
  pOST <- ifelse(population_[,"OST_H"][alive_]==1,0,pOST)
  #Set a osteoarthritis event to 1, if the person develops osteoarthritis
  population_[,"OST_E"][alive_] <- ifelse(random_numbs_[,"OST",Year_+1][alive_]<
                                            pOST,
                                          1,
                                          0)
  #recor dhte probability of osteoarthritis this year
  population_[,"p_OST"][alive_] <- pOST
  #Remove temporary variables
  rm(pOST)
  #return the population matrix
  return(population_)
  
  
}

##'@param population_ is the population matrix
##'@param parameter_ is a row of the paramters matrix
##'@param alive_ is a vector of TRUE and FALSES indicating whether a patient 
##'is alive or not
##'@param random_numbs_ is the array of common random numbers
##'@param Year_ is the current simulation year
##'@return popoulation is the population matrix

update_events_SPHR_cancer <- function(population_, parameter_,Year_, alive_, random_numbs_){
  #Estimate the probability of developing breast cancer this year
  pBC <- Breast_cancer(population_, parameter_, alive_)
  #Reset this probability to 0 for people with a history of breast cancer
  pBC <- ifelse(population_[,"CANB_H"][alive_]==1,0,pBC)
  #record probability of breast cancer
  population_[,"p_BC"][alive_] <- pBC
  #Update breast cancer events
  population_[,"CANB_E"][alive_] <- ifelse(random_numbs_[,"CANB",Year_+1][alive_]<pBC,1,0)
  
  #Estimate the probability of developing colorectal cancer this year
  pCC <- Colorectal_cancer(population_, parameter_, alive_)
  #Reset this probability to 0 for people with a history of breast cancer
  pCC <- ifelse(population_[,"CANC_H"][alive_]==1,0,pCC)
  #record probability of breast cancer
  population_[,"p_CC"][alive_] <- pCC
  #Update breast cancer events
  population_[,"CANC_E"][alive_] <- ifelse(random_numbs_[,"CANC",Year_+1][alive_]<pCC,1,0)
  
  #remove temporary variables
  rm(pCC,pBC)
  
  #return the population matrix
  return(population_)
}

##'@param population_ is the population matrix
##'@param parameter_ is a row of the parameters matrix
##'@param alive_ is a vector of TRUE and FALSES indicating whether a patient 
##'is alive or not
##'@param random_numbs_ is the array of common random numbers
##'@param Year_ is the current simulation year
##'@return pDEP is the vector of the probabilities of depression

update_events_SPHR_depression <- function(population_,
                                          parameters_,
                                          Year_,
                                          alive_,
                                          random_numbs_
){
  #get probability of depression
  pDEP <- depression_SPHR(population_,parameters_,alive_)
  #reset this to 0, if the person has a history of depression
  pDEP <- ifelse(population_[,"DEP_H"][alive_]==1,0,pDEP)
  #Set a depression event to 1, if the person develops depression
  population_[,"DEP_E"][alive_] <- ifelse(random_numbs_[,"DEP",Year_+1][alive_]<
                                            pDEP,
                                          1,
                                          0)
  #recor dhte probability of depression this year
  population_[,"p_DEP"][alive_] <- pDEP
  #Remove temporary variables
  rm(pDEP)
  #return the population matrix
  return(population_)
}