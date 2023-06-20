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

