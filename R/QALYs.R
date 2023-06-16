
##'@param population_ is the population matrix
##'@param parameters_ is a single row of the parameters matrix
##'@param year_ model year
##'@param alive_ is a vector of TRUEs and FALSEs to indicate whether or not people
##'are alive
##'@param GlobalVars_ is the global parameters matrix
##'@return population_ is the revised population matrix
calculate_QALYs <- function(population_, parameters_,  year_, alive_, GlobalVars_) {
  #Calculate multiplier to adjust for a population with T2D without any complications
  
  #Declare mean BMI, age and proportion female from Hayes et al
  Mean_BMI_Hayes <-28.4 #Mean BMI in the source of our baseline utilities 
  Mean_Age_Hayes <- 65.8
  Mean_pFemale_Hayes <- 4729/(6401+4729)
  
  Util_bl_mult <- parameters_[,"UTIL_BL"] / 
    (0.9454933+0.0256466*(1-Mean_pFemale_Hayes)+
       -0.0002213*Mean_Age_Hayes+
       -0.0000294*(Mean_Age_Hayes^2))
  
  #Calculate utility prior to adjusting for BMI and events 
  population_[,"EQ5D"][alive_] <- (0.9454933 + 
                             0.0256466*population_[,"MALE"][alive_]+
                             -0.0002213 * population_[,"AGE"][alive_] + 
                             -0.0000294 * (population_[,"AGE"][alive_]^2))*Util_bl_mult
  
  #apply the BMI decrements to this
  population_[,"EQ5D"][alive_] <- population_[,"EQ5D"][alive_] + 
    parameters_[,"UTIL_BMI"]*(population_[,"BMI"][alive_]-Mean_BMI_Hayes)
  
  #Ensure that QALYs are not greater than 1 after adjusting for BMI, age and gender
  population_[,"EQ5D"][alive_] <- ifelse(population_[,"EQ5D"][alive_]>1,1,population_[,"EQ5D"][alive_])
  
  #Adjust for model events
  #This is multiplicative, with no other option in line with ISPOR good practice 
  #guidance 
  #If you want decrements you must rewrite this code
  #MI 
  #event years
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"MI_E"][alive_]==1|population_[,"MI2_E"][alive_]==1,
                                                     parameters_[,"UTIL_MI_E"],1)
  #History of a first MI
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"MI_H"][alive_]==1,
                                                     parameters_[,"UTIL_MI_H"],1)
  #History of a second MI
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"MI2_H"][alive_]==1,
                                                     parameters_[,"UTIL_MI_H"],1)
  
  #Stroke
  #event years
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"STRO_E"][alive_]==1|population_[,"STRO2_E"][alive_]==1,
                                                     parameters_[,"UTIL_STRO_E"],1)
  #History of a first MI
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"STRO_H"][alive_]==1,
                                                     parameters_[,"UTIL_STRO_H"],1)
  #History of a second MI
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"STRO2_H"][alive_]==1,
                                                     parameters_[,"UTIL_STRO_H"],1)
  
  #CHF
  #event year
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"CHF_E"][alive_]==1,
                                                     parameters_[,"UTIL_CHF"],1)
  #History of CHF
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"CHF_H"][alive_]==1,
                                                     parameters_[,"UTIL_CHF"],1)
  
  #IHD
  #event year
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"IHD_E"][alive_]==1,
                                                     parameters_[,"UTIL_IHD"],1)
  #History of IHD
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"IHD_H"][alive_]==1,
                                                     parameters_[,"UTIL_IHD"],1)
  
  #Blind
  #event year
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"BLIND_E"][alive_]==1,
                                                                     parameters_[,"UTIL_BLIND"],1)
  #History of blindness
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"BLIND_H"][alive_]==1,
                                                                     parameters_[,"UTIL_BLIND"],1)
  
  #Ulcers
  #event year
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"ULCER_E"][alive_]==1,
                                                                     parameters_[,"UTIL_ULCER"],1)
  #History of Ulcer
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"ULCER_H"][alive_]==1,
                                                                     parameters_[,"UTIL_ULCER"],1)
  
  #Amputations
  #event year
  #event years
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"AMP_E"][alive_]==1|population_[,"AMP2_E"][alive_]==1,
                                                                     parameters_[,"UTIL_AMP"],1)
  #History of a first MI
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"AMP_H"][alive_]==1,
                                                                     parameters_[,"UTIL_AMP"],1)
  #History of a second MI
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"AMP2_H"][alive_]==1,
                                                                     parameters_[,"UTIL_AMP"],1)
  
  #Renal Failure
  #event year
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"RENAL_E"][alive_]==1,
                                                                     parameters_[,"UTIL_RENAL"],1)
  #History of Renal Failure
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"RENAL_H"][alive_]==1,
                                                                     parameters_[,"UTIL_RENAL"],1)
  
  #Micro or Macro albuminurea
  #event year
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"MMALB_E"][alive_]==1,
                                                                     parameters_[,"UTIL_MMALB"],1)
  #History of MMALB
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"MMALB_H"][alive_]==1,
                                                                     parameters_[,"UTIL_MMALB"],1)
  
  #ATFIB
  #event year
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"ATFIB_E"][alive_]==1,
                                                                     parameters_[,"UTIL_ATFIB"],1)
  #History of ATFIB
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"ATFIB_H"][alive_]==1,
                                                                     parameters_[,"UTIL_ATFIB"],1)
  
  #PVD
  #event year
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"PVD_E"][alive_]==1,
                                                                     parameters_[,"UTIL_PVD"],1)
  #History of MMALB
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"PVD_H"][alive_]==1,
                                                                     parameters_[,"UTIL_PVD"],1)
  
  #Breast Cancer
  #event year
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"CANB_E"][alive_]==1,
                                                                     parameters_[,"UTIL_CANB"],1)
  #History of MMALB
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"CANB_H"][alive_]==1,
                                                                     parameters_[,"UTIL_CANB"],1)
  
  #Colorectal Cancer
  #event year
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"CANC_E"][alive_]==1,
                                                                     parameters_[,"UTIL_CANC"],1)
  #History of MMALB
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"CANC_H"][alive_]==1,
                                                                     parameters_[,"UTIL_CANC"],1)
  
  #Depression
  #event year
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"DEP_E"][alive_]==1,
                                                                     parameters_[,"UTIL_DEP"],1)
  #History of MMALB
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"DEP_H"][alive_]==1,
                                                                     parameters_[,"UTIL_DEP"],1)
  
  #Osteoarthritis
  #event year
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"OST_E"][alive_]==1,
                                                                     parameters_[,"UTIL_CANC"],1)
  #History of MMALB
  population_[,"EQ5D"][alive_] <-population_[,"EQ5D"][alive_]*ifelse(population_[,"OST_H"][alive_]==1,
                                                                     parameters_[,"UTIL_CANC"],1)
  
  #Half anyone who died this year QALYs, this assumes they died halfway through the year
  dead_thisyear <- is.na(population_[,"F_ALLCAUSE"][alive_])==F
  population_[,"EQ5D"][alive_] <- population_[,"EQ5D"][alive_]*ifelse(dead_thisyear,
                                                                      0.5,
                                                                      1)
  #record QALYs and discounted QALYs cumulatively
  population_[,"QALY"][alive_] <- population_[,"QALY"][alive_] + 
                                  population_[,"EQ5D"][alive_]
  population_[,"DiscQALY"][alive_] <- population_[,"DiscQALY"][alive_] + 
                                      (population_[,"EQ5D"][alive_]/
                                         ((1+as.numeric(GlobalVars_["disc_rate_QALYs", "Value"]))^year_))
  return(population_)
}