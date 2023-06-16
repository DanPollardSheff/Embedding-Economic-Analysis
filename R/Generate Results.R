GenerateDetailedresults <- function(results_,population_, year_, alive_, GlobalVars_){
  #Deaths
  Dead <- sum(is.na(population_[,"F_ALLCAUSE"][alive_])==F)
  results_["Death",year_+1] <- Dead
  #remove temporary variables
  rm(Dead)
  
  #alive_
  alive_year_end <- sum(is.na(population_[,"F_ALLCAUSE"][alive_])==T)
  results_["Alive_year_end", year_+1] <- alive_year_end
  #remove temporary variables
  rm(alive_year_end)
  
  #QALYs
  #Assume people who die do so half way through the year_
  results_["Undiscounted QALYs", year_+1] <- sum(population_[,"EQ5D"][alive_])
  results_["Discounted QALYs", year_+1] <- sum(population_[,"EQ5D"][alive_])/
    ((1+as.numeric(GlobalVars_["disc_rate_QALYs", "Value"]))^year_)
  
  #Life years
  results_["Undiscounted life years accrued", year_+1] <- 
    results_["Alive_year_end", year_+1]+0.5*results_["Death",year_+1]
  results_["Discounted life years accrued", year_+1]<-
    results_["Undiscounted life years accrued", year_+1]/
    ((1+as.numeric(GlobalVars_["disc_rate_LYs", "Value"]))^year_)
  
  #Costs
  results_["Undiscounted Costs", year_+1] <- 
    sum(population_[,"YearCOST"][alive_])
  results_["Discounted Costs", year_+1]<-
    sum(population_[,"YearCOST"][alive_])/
    ((1+as.numeric(GlobalVars_["disc_rate_costs", "Value"]))^year_)
  
  #1st MIs
  #Create a variable for MI history as these people are not eligible to have another first MI
  MI_nohist <- population_[,"MI_H"]==0
  MI <- sum(population_[,"MI_E"][alive_&MI_nohist])
  if(MI!=sum(population_[,"MI_E"])){
    stop("people who are ineligible are getting 1st MIs")
  }
  #Record new proportion of people without this event
  results_["1st MI",year_+1] <- MI
  results_["pMI1", year_+1] <- mean(population_[,"p_MI"])
  results_["1st MI Hist",year_+1] <- sum(population_[,"MI_H"])
  #remove temporary varaibles
  rm(MI_nohist,MI)
  
  #2nd MIs
  MI2_eligible <- population_[,"MI_H"]==1&population_[,"MI2_H"]==0
  MI2 <- sum(population_[,"MI2_E"][alive_&MI2_eligible])
  if(MI2!=sum(population_[,"MI2_E"])){
    stop("people who are ineligible are getting 2nd MIs")
  }
  #Record new proportion of people without this event
  results_["2nd MI",year_+1] <- MI2
  results_["pMI2", year_+1] <- mean(population_[,"p_MI2"])
  results_["2nd MI Hist",year_+1] <- sum(population_[,"MI2_H"])
  #remove temporary varaibles
  rm(MI2_eligible,MI2)
  
  #1st Stroke
  STRO_nohist <- population_[,"STRO_H"]==0
  STRO <- sum(population_[,"STRO_E"][alive_&STRO_nohist])
  if(STRO!=sum(population_[,"STRO_E"])){
    stop("people who are ineligible are getting 1st strokes")
  }
  #Record new proportion of people without this event
  results_["1st Stroke",year_+1] <- STRO
  results_["pSTRO1", year_+1] <- mean(population_[,"p_STRO"])
  results_["1st Stroke Hist",year_+1] <- sum(population_[,"STRO_H"])
  #remove temporary varaibles
  rm(STRO_nohist,STRO)
  
  #2nd Stroke
  STRO2_eligible <- population_[,"STRO_H"]==1&population_[,"STRO2_H"]==0
  STRO2 <- sum(population_[,"STRO2_E"][alive_&STRO2_eligible])
  if(STRO2!=sum(population_[,"STRO2_E"])){
    stop("people who are ineligible are getting 2nd strokes")
  }
  #Record new proportion of people without this event
  results_["2nd Stroke",year_+1] <- STRO2
  results_["pSTRO2", year_+1] <- mean(population_[,"p_STRO2"])
  results_["2nd Stroke Hist",year_+1] <- sum(population_[,"STRO2_H"])
  #remove temporary variables
  rm(STRO2_eligible,STRO2)
  
  #CHF
  CHF_nohist <- population_[,"CHF_H"]==0
  CHF <- sum(population_[,"CHF_E"][alive_&CHF_nohist])
  if(CHF!=sum(population_[,"CHF_E"])){
    stop("people who are ineligible are getting CHF")
  }
  #Record new proportion of people without this event
  results_["CHF",year_+1] <- CHF
  results_["pCHF", year_+1] <- mean(population_[,"p_CHF"])
  results_["CHF Hist",year_+1] <- sum(population_[,"CHF_H"])
  #remove temporary variables
  rm(CHF_nohist,CHF)
  
  #IHD
  IHD_nohist <- population_[,"IHD_H"]==0
  IHD <- sum(population_[,"IHD_E"][alive_&IHD_nohist])
  if(IHD!=sum(population_[,"IHD_E"])){
    stop("people who are ineligible are getting IHD")
  }
  #Record new proportion of people without this event
  results_["IHD",year_+1] <- IHD
  results_["pIHD", year_+1] <- mean(population_[,"p_IHD"])
  results_["IHD Hist",year_+1] <- sum(population_[,"IHD_H"])
  #remove temporary varaibles
  rm(IHD_nohist,IHD)
  
  #Blindness
  Blind_nohist <- population_[,"BLIND_H"]==0
  BLIND <- sum(population_[,"BLIND_E"][alive_&Blind_nohist])
  if(BLIND!=sum(population_[,"BLIND_E"])){
    stop("people who are ineligible are becoming blind")
  }
  #Record new proportion of people without this event
  results_["Blindness",year_+1] <- BLIND
  results_["pBLIND", year_+1] <- mean(population_[,"p_BLIND"])
  results_["Blindness Hist",year_+1] <- sum(population_[,"BLIND_H"])
  #remove temporary varaibles
  rm(Blind_nohist,BLIND)
  
  #Ulcer 
  Ulcer_nohist <- population_[,"ULCER_H"]==0
  ULCER <- sum(population_[,"ULCER_E"][alive_&Ulcer_nohist])
  if(ULCER!=sum(population_[,"ULCER_E"])){
    stop("people who are ineligible are becoming foot ulcers")
  }
  #Record new proportion of people without this event
  results_["Ulcer",year_+1] <- ULCER
  results_["pULCER", year_+1] <- mean(population_[,"p_ULCER"])
  results_["Ulcer Hist",year_+1] <- sum(population_[,"ULCER_H"])
  #remove temporary varaibles
  rm(Ulcer_nohist,ULCER)
  
  #1st Amputation 
  AMP_nohist <- population_[,"AMP_H"]==0
  AMP <- sum(population_[,"AMP_E"][alive_&AMP_nohist])
  if(AMP!=sum(population_[,"AMP_E"])){
    stop("people who are ineligible are getting a 1st amputation")
  }
  #Record new proportion of people without this event
  results_["1st Amputation",year_+1] <- AMP
  results_["pAMP1", year_+1] <- mean(population_[,"p_amp1"])
  results_["1st Amputation Hist",year_+1] <- sum(population_[,"AMP_H"])
  #remove temporary variables
  rm(AMP_nohist,AMP)
  
  #2nd Amputation 
  AMP_hist <- population_[,"AMP_H"]==1&population_[,"AMP2_H"]==0
  AMP2 <- sum(population_[,"AMP2_E"][alive_&AMP_hist])
  if(AMP2!=sum(population_[,"AMP2_E"])){
    stop("people who are ineligible are getting a 2nd amputations")
  }
  #Record new proportion of people without this event
  results_["2nd Amputation",year_+1] <- AMP2
  results_["pAMP2", year_+1] <- mean(population_[,"p_amp2"])
  results_["2nd Amputation Hist",year_+1] <- sum(population_[,"AMP2_H"])
  #remove temporary varaibles
  rm(AMP_hist,AMP2)
  
  #Renal Failure
  Renal_nohist <- population_[,"RENAL_H"]==0
  RENAL <- sum(population_[,"RENAL_E"][alive_&Renal_nohist])
  if(RENAL!=sum(population_[,"RENAL_E"])){
    stop("people who are ineligible are getting a renal failure")
  }
  #Record new proportion of people without this event
  results_["Renal Failure",year_+1] <- RENAL
  results_["pRENAL", year_+1] <- mean(population_[,"p_renal"])
  results_["Renal Failure Hist",year_+1] <- sum(population_[,"RENAL_H"])
  #remove temporary variables
  rm(Renal_nohist,RENAL)
  
  #PVD
  PVD_nohist <- population_[,"PVD_H"]==0
  results_["PVD",year_+1] <- sum(population_[,"PVD_E"][alive_&PVD_nohist])
  results_["PVD Hist", year_+1]<- sum(population_[,"PVD_H"])
  results_["p_PVD",year_+1] <- mean(population_[,"p_PVD"])
  
  #ATFIB
  ATFIB_nohist <- population_[,"ATFIB_H"]==0
  results_["ATFIB",year_+1] <- sum(population_[,"ATFIB_E"][alive_&ATFIB_nohist])
  results_["ATFIB Hist",year_+1] <- sum(population_[,"ATFIB_H"])
  results_["p_ATFIB",year_+1] <- mean(population_[,"p_ATFIB"])
  
  #MMALB
  MMALB_nohist <- population_[,"MMALB_H"]==0
  results_["MMALB",year_+1] <- sum(population_[,"MMALB_E"][alive_&MMALB_nohist])
  results_["MMALB Hist",year_+1] <- sum(population_[,"MMALB_H"])
  results_["p_MMALB",year_+1] <- mean(population_[,"p_MMALB"])
  
  #Smoking
  results_["SMO",year_+1] <- sum(population_[,"SMO"][alive_])
  results_["p_SMO",year_+1] <- mean(population_[,"p_SMO"][alive_])
  
  #Breast Cancer
  BCAN_nohist <- population_[,"CANB_H"]==0
  results_["Breast Cancer",year_+1] <- sum(population_[,"CANB_E"][alive_&BCAN_nohist])
  results_["Breast Cancer Hist",year_+1] <- sum(population_[,"CANB_H"])
  results_["p_BC",year_+1] <- mean(population_[,"p_BC"])
  
  #Colorectal Cancer
  CCAN_nohist <- population_[,"CANC_H"]==0
  results_["Colorectal Cancer",year_+1] <- sum(population_[,"CANC_E"][alive_&CCAN_nohist])
  results_["Colorectal Cancer Hist",year_+1] <- sum(population_[,"CANC_H"])
  results_["p_CC",year_+1] <- mean(population_[,"p_CC"])
  
  #Depression
  DEP_nohist <- population_[,"DEP_H"]==0
  results_["Depression",year_+1] <- sum(population_[,"DEP_E"][alive_&DEP_nohist])
  results_["Depression Hist",year_+1] <- sum(population_[,"DEP_H"])
  results_["p_DEP",year_+1] <- mean(population_[,"p_DEP"])
  
  #Osteoarthritis
  OST_nohist <- population_[,"OST_H"]==0
  results_["Osteoarthritis",year_+1] <- sum(population_[,"OST_E"][alive_&OST_nohist])
  results_["Osteoarthritis Hist",year_+1] <- sum(population_[,"OST_H"])
  results_["p_OST",year_+1] <- mean(population_[,"p_OST"])
  
  #record risk factors
  results_["eGFR", year_+1] <- mean(population_[,"eGFR"][alive_])
  results_["HBA", year_+1] <- mean(population_[,"HBA"][alive_])
  results_["BMI", year_+1] <- mean(population_[,"BMI"][alive_])
  results_["SBP", year_+1] <- mean(population_[,"SBP"][alive_])
  results_["HDL", year_+1] <- mean(population_[,"HDL"][alive_])
  results_["LDL", year_+1] <- mean(population_[,"LDL"][alive_])
  results_["HEARTR", year_+1] <- mean(population_[,"HEART_R"][alive_])
  results_["WBC", year_+1] <- mean(population_[,"WBC"][alive_])
  results_["HAEM", year_+1] <- mean(population_[,"HAEM"][alive_])
  
  return(results_)
  
}