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


####'This function runs the model for a given set of patients and parameters
####'@param population_, is the population matrix
####'@param parameters_, is the full parameters matrix
####'@param endtime_, is the number of years to run the simulation for
####'@param GlobalVars_, is the matrix giving the global variables
####'@param random_numbs_, is an array of common random numbers giving a random number
####'@param LifeTables_, is a dataframe containing life table information in formate
####'that can easily be matched to the population matrix
####'@param SOUR_, is the current second order uncertainty run
####'draw for each patient in each year for every event in which a random number 
####'is required 
####'@return results, is the results matrix
####'@return psaresults, is a summary set of results to produce in the PSA

run_simulation <- function(population_, parameters_, endtime_, treatment_, GlobalVars_, random_numbs_,LifeTables_, SOUR_){
  
  ##reduce the parameters matrix down to the correct row
  if(GlobalVars_["run_psa","Value"]==F){
    parameters_ <- parameters_[1,]
  }else{
    parameters_ <- parameters_[SOUR_+1,]
  }
  ##Create the underlying UKPDS trajectory matrices
  HBA1c_underlying  <- UKPDS_90_contrisk_A1c(population_,parameters_,endtime_)
  BMI_underlying    <- UKPDS_90_contrisk_BMI(population_,parameters_,endtime_)
  SBP_underlying    <- UKPDS_90_contrisk_SBP(population_,parameters_,endtime_)
  HDL_underlying    <- UKPDS_90_contrisk_HDL(population_,parameters_,endtime_)
  LDL_underlying    <- UKPDS_90_contrisk_LDL(population_,parameters_,endtime_)
  HEARTR_underlying <- UKPDS_90_HEARTR(population_,parameters_,endtime_)
  WBC_underlying    <- UKPDS_90_WBC(population_,parameters_,endtime_)
  HAEM_underlying   <- UKPDS_90_HAEM(population_,parameters_,endtime_)
  
  #Place to add Intervention effects
  attend_se         <- initialise_intervention_dt_attendse(length(population_[,"ID"]), treatment_, parameters_)
  HBA1c_INTV        <- initialise_intervention_dt_HbA1c(length(population_[,"ID"]),treatment_,parameters_,endtime_,GlobalVars_,attend_se)
  BMI_INTV          <- initialise_intervention_dt_BMI(length(population_[,"ID"]),treatment_,parameters_,endtime_,GlobalVars_,attend_se)
  SBP_INTV          <- initialise_intervention_dt_SBP(length(population_[,"ID"]),treatment_,parameters_,endtime_,GlobalVars_,attend_se)
  HDL_INTV          <- initialise_intervention_dt_HDL(length(population_[,"ID"]),treatment_,parameters_,endtime_,GlobalVars_,attend_se)
  LDL_INTV          <- initialise_intervention_dt_LDL(length(population_[,"ID"]),treatment_,parameters_,endtime_,GlobalVars_,attend_se)
  
  
  
  #start year at 0
  year <- 0
  
  #initialise the results matrix
  results <- GenerateResultsMatrix(GlobalVars_, endtime_)
  #initialise a PSA results matrix, if it is a PSA
  if(GlobalVars_["run_psa","Value"]==T){
  psaresults <- matrix(data=NA, nrow = as.numeric(GlobalVars_["psa_count", "Value"]), ncol = 5)  
  colnames(psaresults) <- c("Life Years per Patient", "Undiscounted QALYs per Patient", 
                            "QALYs per Patient", "Undiscounted Costs per Patient",
                            "Discounted Costs per Patient")
  }
  
  #run the model up to the specified end time or so long as at least one person is alive
  while (year < endtime_ & 
         (sum(is.na(population_[,"F_ALLCAUSE"]))) >= 1){
  #Get a logical vector indicating if people are alive, use the fact that FOTH
  #in the population matrix is NA if alive
  alive <- is.na(population_[,"F_ALLCAUSE"])
  #create matrix of ture = dead too for unit tests
  dead <- is.na(population_[,"F_ALLCAUSE"])==F
  
  #Estimate Diabetes Related complications and all cause deaths for this year
  population_ <- update_events_UKPDS82(population_,parameters_, treatment_, year, alive, random_numbs_, LifeTables_)
  #Estimate PVD,ATFIB,MMALB
  population_ <- update_events_UKPDS90(population_,parameters_, year, alive,random_numbs_)
  #Estimate Depression
  population_ <- update_events_SPHR_depression(population_,parameters_,year,alive,random_numbs_)
  #Estimate Osteoarthritis
  population_ <- update_events_SPHR_osteoarthritis(population_,parameters_,year,alive,random_numbs_)
  #Estimate Cancer incidence
  population_ <- update_events_SPHR_cancer(population_, parameters_,year, alive,random_numbs_)
  
  #Stop the model if dead people have events
  if(sum(population_[,"MI_E"][dead])  !=0|
     sum(population_[,"MI2_E"][dead]) != 0|
     sum(population_[,"AMP_E"][dead]) != 0| 
     sum(population_[,"AMP2_E"][dead]) != 0|
     sum(population_[,"BLIND_E"][dead]) != 0|
     sum(population_[,"ULCER_E"][dead]) != 0|
     sum(population_[,"RENAL_E"][dead]) != 0|
     sum(population_[,"CHF_E"][dead]) != 0|
     sum(population_[,"IHD_E"][dead]) != 0|
     sum(population_[,"STRO_E"][dead]) != 0|
     sum(population_[,"STRO2_E"][dead]) != 0|
     sum(population_[,"ATFIB_E"][dead]) != 0|
     sum(population_[,"PVD_E"][dead]) != 0|
     sum(population_[,"MMALB_E"][dead]) != 0|
     sum(population_[,"CANB_E"][dead]) != 0|
     sum(population_[,"CANC_E"][dead]) != 0|
     sum(population_[,"DEP_E"][dead]) != 0|
     sum(population_[,"OST_E"][dead]) != 0){
    #stop the model if the number of people with dead people are recorded as
    #having an event
    #push everything to the global enviroment for bug checking
    for (variable in ls()) {
      assign(variable, get(variable), envir = .GlobalEnv)
    }
    
    stop("dead people are getting events")
  }
  
  ##QALYs
  population_ <- calculate_QALYs(population_, parameters_,  year, alive, GlobalVars_)
  ##Costs
  population_ <- calculate_costs(population_, parameters_, year, alive, GlobalVars_,treatment_,attend_se)
  
  #Record results
  results <- GenerateDetailedresults(results,population_, year, alive, GlobalVars_)
  
  
  #update histories
  population_ <- update_history(population_,
                                HBA1c_underlying,
                                BMI_underlying,
                                SBP_underlying,    
                                HDL_underlying,
                                LDL_underlying, 
                                HEARTR_underlying,
                                WBC_underlying,
                                HAEM_underlying,
                                HBA1c_INTV,
                                BMI_INTV,
                                SBP_INTV,
                                HDL_INTV,
                                LDL_INTV,
                                year)
  
  population_ <- update_patchars(population_, parameters_, alive)
  
  #Unit test
  #Stop the model if there are events still in the population matrix
 if(sum(population_[,"MI_E"])  !=0|
     sum(population_[,"MI2_E"]) != 0|
     sum(population_[,"AMP_E"]) != 0| 
     sum(population_[,"AMP2_E"]) != 0|
     sum(population_[,"BLIND_E"]) != 0|
     sum(population_[,"ULCER_E"]) != 0|
     sum(population_[,"RENAL_E"]) != 0|
     sum(population_[,"CHF_E"]) != 0|
     sum(population_[,"IHD_E"]) != 0|
     sum(population_[,"STRO_E"]) != 0|
     sum(population_[,"STRO2_E"]) != 0){
   
     #push everything to the global enviroment for bug checking
     for (variable in ls()) {
       assign(variable, get(variable), envir = .GlobalEnv)
     }
   #stop the model if the events are not reset to 0
    stop("events are not set to zero before reset")
  }
  
  year <- year + 1
  
  }

  
  #For now return the Detailed results table or the population matrix if the run is deterministic
  if(GlobalVars_["Results_output", "Value"] == "Summary"&
     GlobalVars_["run_psa", "Value"]==T){
    psaresults <- matrix(data=NA,nrow=1,ncol=24)
    #Life Years
    psaresults[,1] <- sum(results["Undiscounted life years accrued",],na.rm=TRUE)/length(population_[,"ID"])
    psaresults[,2] <- sum(results["Discounted life years accrued",],na.rm=TRUE)/length(population_[,"ID"])
    psaresults[,3] <- sum(results["Undiscounted QALYs",],na.rm=TRUE)/length(population_[,"ID"])
    psaresults[,4] <- sum(results["Discounted QALYs",],na.rm=TRUE)/length(population_[,"ID"])
    psaresults[,5] <- sum(results["Undiscounted Costs",],na.rm=TRUE)/length(population_[,"ID"])
    psaresults[,6] <- sum(results["Discounted Costs",],na.rm=TRUE)/length(population_[,"ID"])
    #10 year histories
    psaresults[,7] <- results["1st MI Hist",11]
    psaresults[,8] <- results["2nd MI Hist",11]
    psaresults[,9] <- results["1st Stroke Hist",11]
    psaresults[,10] <- results["2nd Stroke Hist",11]
    psaresults[,11] <- results["CHF Hist",11]
    psaresults[,12] <- results["IHD Hist",11]
    psaresults[,13] <- results["Blindness Hist",11]
    psaresults[,14] <- results["Ulcer Hist",11]
    psaresults[,15] <- results["1st Amputation Hist",11]
    psaresults[,16] <- results["2nd Amputation Hist",11]
    psaresults[,17] <- results["Renal Failure Hist",11]
    psaresults[,18] <- results["PVD Hist",11]
    psaresults[,19] <- results["MMALB Hist",11]
    psaresults[,20] <- results["ATFIB Hist",11]
    psaresults[,21] <- results["Breast Cancer Hist",11]
    psaresults[,22] <- results["Colorectal Cancer Hist",11]
    psaresults[,23] <- results["Depression Hist",11]
    psaresults[,24] <- results["Osteoarthritis Hist",11]
    #delete the original results matrix
    rm(results)
    return(psaresults)
  }else if(GlobalVars_["Results_output","Value"]=="Patient Level"&
     GlobalVars_["run_psa","Value"]==F){#option to produce the patient 
    #charateristics matrix for checking results stability for number of patients
    return(population_)
  }else{#default is the 
  return(results)
  }
}