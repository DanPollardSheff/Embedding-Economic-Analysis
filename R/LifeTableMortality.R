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



##'@param population_, is the population matrix
##'@param lifetables_, is the lifetable probabilities put into a long data frame
##'@param alive_ is a vector of true and falses indicating whether a person
##'is alive or not in this year of the model
##'@return p_mort, is the age gender matched mortality for each person

LifeTableMort <- function(population_,
                          LifeTables_,
                          alive_){
  #convert data into datframes for dplyr
  
          LifeTab <- as.data.frame(LifeTables_)
          pop <- as.data.frame(population_)
          pop <- subset(pop, alive_)
  #join the dataframes
    probs <- dplyr::left_join(pop,LifeTab, by = c("AGE", "FEMALE")) 
    #convert to a matrix for next steps
    probs <- as.matrix(probs)
    #reduce down to mortality rate
    probs <- probs[,c("MortRate_nextyear")]
    
    #remove temporary variables
    rm(LifeTab,pop)
    
  return(probs)
}