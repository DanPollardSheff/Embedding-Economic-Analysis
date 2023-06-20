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




##'@param population_ is the population matrix
##'@param parameter_ is a row of the paramters matrix
##'@param alive_ is a vector of TRUE and FALSES indicating whether a patient 
##'is alive or not
##'@param random_numbs_ is the array of common random numbers
##'@return pBC is the vector of the probabilties of breast cancer

Breast_cancer <- function(population_, parameter_, alive_){
  #Calculate the fitted value
  FV <- parameter_[,"CANB_mu"]+
  parameter_[,"CANB_bta_MEN"]*population_[,"MEN"][alive_]+
  parameter_[,"CANB_bta_BMI"]*population_[,"BMI"][alive_]+
  parameter_[,"CANB_bta_BMIMEN"]*(population_[,"MEN"][alive_]*population_[,"BMI"][alive_])
  #convert to probabilities
  pBC <- 1-exp(-exp(FV))
  #set breast cancer risk to 0 for men
  pBC <- ifelse(population_[,"FEMALE"]==0,0,pBC)
  #remove temporary variables generated in the function
  rm(FV)
  #return the probabilities
  return(pBC)
}

##'@param population_ is the population matrix
##'@param parameter_ is a row of the paramters matrix
##'@param alive_ is a vector of TRUE and FALSES indicating whether a patient 
##'is alive or not
##'@return pCC is the vector of the probabilties of colorectal cancer

Colorectal_cancer <- function(population_, parameter_, alive_){

  FV <- parameter_[,"CANC_mu"]+
    parameter_[,"CANC_bta_MALE"]*(ifelse(population_[,"FEMALE"][alive]==1,0,1))+
    parameter_[,"CANC_bta_BMI"]*population_[,"BMI"][alive_]+
    parameter_[,"CANC_bta_BMIMALE"]*(ifelse(population_[,"FEMALE"][alive]==1,0,1))*population_[,"BMI"][alive_]
  #convert to probabilities
  pCC <- 1-exp(-exp(FV))
  #remove temporary variables generated in the function
  rm(FV)
  return(pCC)
}

