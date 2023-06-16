##'@param population_ is the population matrix
##'@param parameter_ is a row of the parameters matrix
##'@param alive_ is a vector of TRUE and FALSES indicating whether a patient 
##'is alive or not
##'@param random_numbs_ is the array of common random numbers
##'@param Year_ is the current simulation year
##'@return pDEP is the vector of the probabilities of depression
##'

depression_SPHR <- function(population_,
                       parameters_,
                       alive_){
  ### Depression probabilities
  #Calculate the fitted value for the depression equation
  FV <- 
    parameters_[,"DEP_alpha"]+
    parameters_[,"DEP_DM"]+
    parameters_[,"DEP_STRO"]*population_[,"STRO_E"][alive_]+
    parameters_[,"DEP_STRO"]*population_[,"STRO_H"][alive_]  
  
  #Calculate probabilities
  pDEP <- 1 - exp(-exp(FV))
  
  #drop the fitted value
  rm(FV)
  
  #return the probability of depression
  return(pDEP)
}

