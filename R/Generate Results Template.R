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




#' @param GlobalVar_, this the global options matrix
#' @return results, this is the results template
#' 


GenerateResultsMatrix <- function(GlobalVar_, endtime_){
    #initialise the results matrix
    events_detailed <- c("Death", "Alive_year_end", 
                         "Undiscounted life years accrued","Discounted life years accrued",
                         "Undiscounted QALYs", "Discounted QALYs",
                         "Undiscounted Costs", "Discounted Costs",
                         "1st MI", "1st MI Hist","pMI1",
                         "2nd MI", "2nd MI Hist", "pMI2", 
                         "1st Stroke", "1st Stroke Hist", "pSTRO1",
                         "2nd Stroke", "2nd Stroke Hist", "pSTRO2",
                         "CHF", "CHF Hist", "pCHF",
                         "IHD", "IHD Hist", "pIHD",
                         "Blindness", "Blindness Hist", "pBLIND",
                         "Ulcer", "Ulcer Hist", "pULCER",
                         "1st Amputation", "1st Amputation Hist", "pAMP1",
                         "2nd Amputation", "2nd Amputation Hist", "pAMP2",
                         "Renal Failure", "Renal Failure Hist", "pRENAL",
                         "PVD", "PVD Hist", "p_PVD",
                         "MMALB","MMALB Hist", "p_MMALB",
                         "ATFIB", "ATFIB Hist" ,"p_ATFIB",
                         "Breast Cancer", "Breast Cancer Hist", "p_BC",
                         "Colorectal Cancer","Colorectal Cancer Hist","p_CC",
                         "Depression","Depression Hist","p_DEP",
                         "Osteoarthritis", "Osteoarthritis Hist", "p_OST",
                         "SMO", "p_SMO",
                         "eGFR", "HBA", "BMI", "SBP", "LDL", "HDL", "WBC", "HEARTR",
                         "HAEM",
                          "year")
    results <- matrix(data=NA, nrow = length(events_detailed), ncol = endtime_)
    rownames(results) <- events_detailed
    #set the year row to be the year
    results["year",] <- 1:endtime_
    #remove unused variables
    rm(events_detailed)
  
  
  return(results)
  
  
  
}