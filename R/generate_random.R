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



### Set up random matrix array
##'@param n is the number of patients in this model run
##'@return random_numbers is an array of random numbers for all possible events 
##'for each patient for 100 years
generate_random <- function(n) {
  #List of all events we want a random number for  
  events <- c("GP", "CHF", "IHD", "MI1", "MI2", "STRO", "STRO2", "BLIND", 
                "ULCER", "AMP", "AMP2", "RENAL", "DEATH", "SMO", "MMALB", "ATFIB", 
                "PVD", "eGFRu60", "CANB", "CANC", "DEP", "OST")
  #produce the random number array
    random_numbers <- array(runif(n * length(events) * 100), 
                            dim = c(n, length(events), 100), 
                            dimnames = list(NULL, events, NULL))
    #remove unnecessary variables
    rm(events)
    return(random_numbers)
    
} 


