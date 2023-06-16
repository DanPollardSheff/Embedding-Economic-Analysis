##'@param population_ is the population matrix
##'@param parameter_ is a row of the parameters matrix
##'@param alive_ is a vector of TRUE and FALSES indicating whether a patient 
##'is alive or not
##'@return pOST is the vector of the probabilities of osteoarthritis

Oesto_SPHR <- function(population_,
                            parameters_,
                            alive_){
  ### Depression probabilities
  #Calculate the fitted value for the osteoarthritis equation
  FV <- 
  parameters_[,"OST_mu"] + 
  parameters_[,"OST_bta_DXT2"]+
  parameters_[,"OST_bta_BMI"]*population[,"BMI"][alive_]
  
  #Calculate probabilities
  pOST <- 1 - exp(-exp(FV))
  
  #drop the fitted value
  rm(FV)
  
  #return the probability of osteoarthritis
  return(pOST)
}


