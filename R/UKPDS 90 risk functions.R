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



##'@param population_, is the popualtion matrix
##'@param parameter_, is the row of hte parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return A1c is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

UKPDS_90_contrisk_A1c <- function(population_, parameter_, endtime_){
  
  #set up a matrix to store the results
  A1c <- matrix(data=NA, nrow = length(population_[,"ID"]), ncol = endtime_ + 2)
  #make the first column the ID's of the patients
  A1c[,1] <- population_[,"ID"]
  #Make the second column the baseline HbA1c
  A1c[,2] <- population_[,"HBA"]
  
  for(i in 3:(endtime_+2)){
  A1c[,i] <-  parameter_[,"HbA1c_UKPDS90_cons"]+
    parameter_[,"HbA1c_UKPDS90_female"]*population_[,"FEMALE"]+
    parameter_[,"HbA1c_UKPDS90_AFRCAR"]*population_[,"AFRO"]+
    parameter_[,"HbA1c_UKPDS90_INDIAN"]*population_[,"INDIAN"]+
    parameter_[,"HbA1c_UKPDS90_lagA1c"]*A1c[,(i-1)]+
    parameter_[,"HbA1c_UKPDS90_ln_yearpostdiag"]*log(population_[,"DIAB_DUR"]+ (i-2))+
    parameter_[,"HbA1c_UKPDS90_first_A1c"]*population_[,"HBA_0"]
  }
  
  #name the first column ID
  colnames(A1c) <- -1:endtime_ #give everything the year that it happened as a column name
  colnames(A1c)[1] <- c("ID") #overwrite the first column with ID
  
return(A1c)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return SBP is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

UKPDS_90_contrisk_SBP <- function(population_, parameter_, endtime_){
  #set up a matrix to store the results
  SBP <- matrix(data=NA, nrow = length(population_[,"ID"]), ncol = endtime_ + 2)
  #make the first column the ID's of the patients
  SBP[,1] <- population_[,"ID"]
  #Make the second column the baseline SBP
  SBP[,2] <- population_[,"SBP"]
  for(i in 3:(endtime_+2)){
    SBP[,i] <-  parameter_[,"SBP_UKPDS90_cons"]+
      parameter_[,"SBP_UKPDS90_female"]*population_[,"FEMALE"]+
      parameter_[,"SBP_UKPDS90_INDIAN"]*population_[,"INDIAN"]+
      parameter_[,"SBP_UKPDS90_lagSBP"]*SBP[,(i-1)]+
      parameter_[,"SBP_UKPDS90_ln_yearpostdiag"]*log(population_[,"DIAB_DUR"]+ (i-2))+
      parameter_[,"SBP_UKPDS90_first_SBP"]*population_[,"SBP_0"]
  }
  
  #name the first column ID
  colnames(SBP) <- -1:endtime_ #give everything the year that it happened as a column name
  colnames(SBP)[1] <- c("ID") #overwrite the first column with ID
  
  return(SBP)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return LDL is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

UKPDS_90_contrisk_LDL <- function(population_, parameter_, endtime_){
  #set up a matrix to store the results
  LDL <- matrix(data=NA, nrow = length(population_[,"ID"]), ncol = endtime_ + 2)
  #make the first column the ID's of the patients
  LDL[,1] <- population_[,"ID"]
  #Make the second column the baseline LDL
  LDL[,2] <- population_[,"LDL"]
  for(i in 3:(endtime_+2) ){
    LDL[,i] <-  parameter_[,"LDL_UKPDS90_cons"]+
      parameter_[,"LDL_UKPDS90_female"]*population_[,"FEMALE"]+
      parameter_[,"LDL_UKPDS90_AFRCAR"]*population_[,"AFRO"]+
      parameter_[,"LDL_UKPDS90_INDIAN"]*population_[,"INDIAN"]+
      parameter_[,"LDL_UKPDS90_lagLDL"]*LDL[,(i-1)]+
      parameter_[,"LDL_UKPDS90_ln_yearpostdiag"]*log(population_[,"DIAB_DUR"]+ (i-2))+
      parameter_[,"LDL_UKPDS90_first_LDL"]*population_[,"LDL_0"]
  }
  #name the first column ID
  colnames(LDL) <- -1:endtime_ #give everything the year that it happened as a column name
  colnames(LDL)[1] <- c("ID") #overwrite the first column with ID
  
  
  
  return(LDL)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return LDL is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

UKPDS_90_contrisk_HDL <- function(population_, parameter_, endtime_){
  #set up a matrix to store the results
  HDL <- matrix(data=NA, nrow = length(population_[,"ID"]), ncol = endtime_ + 2)
  #make the first column the ID's of the patients
  HDL[,1] <- population_[,"ID"]
  #Make the second column the baseline LDL
  HDL[,2] <- population_[,"HDL"]
  for(i in 3:(endtime_+2) ){
    HDL[,i] <-  parameter_[,"HDL_UKPDS90_cons"]+
      parameter_[,"HDL_UKPDS90_female"]*population_[,"FEMALE"]+
      parameter_[,"HDL_UKPDS90_AFRCAR"]*population_[,"AFRO"]+
      parameter_[,"HDL_UKPDS90_lagHDL"]*HDL[,(i-1)]+
      parameter_[,"HDL_UKPDS90_first_HDL"]*population_[,"HDL_0"]
  }
  #name the first column ID
  colnames(HDL) <- -1:endtime_ #give everything the year that it happened as a column name
  colnames(HDL)[1] <- c("ID") #overwrite the first column with ID
  
  return(HDL)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return BMI is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

UKPDS_90_contrisk_BMI <- function(population_, parameter_, endtime_){
  #set up a matrix to store the results
  BMI <- matrix(data=NA, nrow = length(population_[,"ID"]), ncol = endtime_ + 2)
  #make the first column the ID's of the patients
  BMI[,1] <- population_[,"ID"]
  #Make the second column the baseline LDL
  BMI[,2] <- population_[,"BMI"]
  for(i in 3: (endtime_+2)){
    BMI[,i] <-  parameter_[,"BMI_UKPDS90_cons"]+
      parameter_[,"BMI_UKPDS90_female"]*population_[,"FEMALE"]+
      parameter_[,"BMI_UKPDS90_AFRCAR"]*population_[,"AFRO"]+
      parameter_[,"BMI_UKPDS90_INDIAN"]*population_[,"INDIAN"]+
      parameter_[,"BMI_UKPDS90_lagBMI"]*BMI[,(i-1)]+
      parameter_[,"BMI_UKPDS90_ln_yearpostdiag"]*log(population_[,"DIAB_DUR"]+ (i-2))+
      parameter_[,"BMI_UKPDS90_first_BMI"]*population_[,"BMI_0"]
  }
  #name the first column ID
  colnames(BMI) <- -1:endtime_ #give everything the year that it happened as a column name
  colnames(BMI)[1] <- c("ID") #overwrite the first column with ID
  
  return(BMI)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return HEARTR is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

UKPDS_90_HEARTR       <- function(population_, parameter_, endtime_){
  #if the end year is not a multiple of 3 round up to this year to allow linear interpolation later
  lastcol <-  ceiling ((endtime_ + 2)/3)*3
  #set up a matrix to store the results
  HEARTR <- matrix(data=NA, nrow = length(population_[,"ID"]), ncol = lastcol)
  #make the first column the ID's of the patients
  HEARTR[,1] <- population_[,"ID"]
  #Make the second column the baseline LDL
  HEARTR[,2] <- population_[,"HEART_R"]
  
  ##years in which to estimate HEART rate (every 3 years)
  years <- seq(from = 5, to = ceiling ((endtime_ + 2)/3)*3, by=3) #start at column 5, the first two columns are ID and baseline values
  
  for(i in years){
      HEARTR[,i] <-  parameter_[,"HeartR_UKPDS90_cons"]+
        parameter_[,"HeartR_UKPDS90_female"]*population_[,"FEMALE"]+
        parameter_[,"HeartR_UKPDS90_lagHeartR"] *HEARTR[,(i-3)]+
        parameter_[,"HeartR_UKPDS90_ln_yearpostdiag"]*log(population_[,"DIAB_DUR"]+ (i-2))+
        parameter_[,"HeartR_UKPDS90_first_HeartR"]*population_[,"HEART_R_0"]
  }  
  
  #linear interpolation in the missing years
  #stop the end year
  for (i in 2:(endtime_+2)){
    #only apply the code, if it is in a year without an estimation
    #there will be an estimation every three years, offset by two columns
    #i.e. year 3 will be in column 5
    if((i-2)/3 != floor((i-2)/3)){
      lower_i <- ((floor((i-2)/3))*3)+2
      upper_i <- ((ceiling((i-2)/3))*3)+2
      
      HEARTR[,i]  <- HEARTR[,upper_i]*(1-(ceiling((i-2)/3)-(i-2)/3))+
        HEARTR[,lower_i]*(1-((i-2)/3-(floor((i-2)/3))))
    }
  }
  
  #name the first column ID
  colnames(HEARTR) <- -1:(lastcol-2) #give everything the year that it happened as a column name
  colnames(HEARTR)[1] <- c("ID") #overwrite the first column with ID
  
  return(HEARTR)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return WBC is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

UKPDS_90_WBC          <- function(population_, parameter_, endtime_){
  #if the end year is not a multiple of 3 round up to this year to allow linear interpolation later
  lastcol <-  ceiling ((endtime_ + 2)/3)*3
  #set up a matrix to store the results
  WBC <- matrix(data=NA, nrow = length(population_[,"ID"]), ncol = lastcol)
  #make the first column the ID's of the patients
  WBC[,1] <- population_[,"ID"]
  #Make the second column the baseline LDL
  WBC[,2] <- population_[,"WBC"]
  
  ##years in which to estimate WBC (every 3 years)
  years <- seq(from = 5, to = ceiling ((endtime_ + 2)/3)*3, by=3) #start at column 5, the first two columns are ID and baseline values
  #Populate the rest of the table
  for(i in years){
      WBC[,i] <-  parameter_[,"WBC_UKPDS90_cons"] +
        parameter_[,"WBC_UKPDS90_female"]*population_[,"FEMALE"]+
        parameter_[,"WBC_UKPDS90_AFRCAR"]*population_[,"AFRO"]+
        parameter_[,"WBC_UKPDS90_lagWBC"]*WBC[,i-3]+
        parameter_[,"WBC_UKPDS90_ln_yearpostdiag"]*log(population_[,"DIAB_DUR"]+(i-2))+
        parameter_[,"WBC_UKPDS90_first_WBC"]*population_[,"WBC_0"]
  }
  
  #Interpolate in the remaining years
  #linear interpolation in the missing years
  #stop the end year
  for (i in 2:(endtime_+2)){
    #only apply the code, if it is in a year without an estimation
    #there will be an estimation every three years, offset by two columns
    #i.e. year 3 will be in column 5
    if((i-2)/3 != floor((i-2)/3)){
      lower_i <- ((floor((i-2)/3))*3)+2
      upper_i <- ((ceiling((i-2)/3))*3)+2
      
      WBC[,i]  <- WBC[,upper_i]*(1-(ceiling((i-2)/3)-(i-2)/3))+
        WBC[,lower_i]*(1-((i-2)/3-(floor((i-2)/3))))
    }
  }
  #name the first column ID
  colnames(WBC) <- -1:(lastcol-2) #give everything the year that it happened as a column name
  colnames(WBC)[1] <- c("ID") #overwrite the first column with ID
  
  return(WBC)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param endtime_, is the number of years to run this model for
##'@return HAEM is the matrix giving the UKPDS90 trajectory for each individual 
##'over time

UKPDS_90_HAEM         <- function(population_, parameter_, endtime_){
  #if the end year is not a multiple of 3 round up to this year to allow linear interpolation later
  lastcol <-  ceiling ((endtime_ + 2)/3)*3
  #set up a matrix to store the results
  HAEM <- matrix(data=NA, nrow = length(population_[,"ID"]), ncol = lastcol)
  #make the first column the ID's of the patients
  HAEM[,1] <- population_[,"ID"]
  #Make the second column the baseline LDL
  HAEM[,2] <- population_[,"HAEM"]
  ##years in which to estimate WBC (every 3 years)
  years <- seq(from = 5, to = ceiling ((endtime_ + 2)/3)*3, by=3) #start at column 5, the first two columns are ID and baseline values
  #Populate the rest of the table
  for (i in years){
      HAEM[,i] <-  parameter_[,"HAEM_UKPDS90_cons"] +
        parameter_[,"HAEM_UKPDS90_female"]*population_[,"FEMALE"]+
        parameter_[,"HAEM_UKPDS90_AFRCAR"]*population_[,"AFRO"]+
        parameter_[,"HAEM_UKPDS90_ln_yearpostdiag"]*log(population_[,"DIAB_DUR"]+ (i-2))+
        parameter_[,"HAEM_UKPDS90_first_HAEM"]*population_[,"HAEM_0"]
  }
  #Interpolate in the remaining years
  #linear interpolation in the missing years
  #stop the end year
  for (i in 2:(endtime_+2)){
    #only apply the code, if it is in a year without an estimation
    #there will be an estimation every three years, offset by two columns
    #i.e. year 3 will be in column 5
    if((i-2)/3 != floor((i-2)/3)){
      lower_i <- ((floor((i-2)/3))*3)+2
      upper_i <- ((ceiling((i-2)/3))*3)+2
      
      HAEM[,i]  <- HAEM[,upper_i]*(1-(ceiling((i-2)/3)-(i-2)/3))+
        HAEM[,lower_i]*(1-((i-2)/3-(floor((i-2)/3))))
    }
  }
  #name the first column ID
  colnames(HAEM) <- -1:(lastcol-2) #give everything the year that it happened as a column name
  colnames(HAEM)[1] <- c("ID") #overwrite the first column with ID
  
  return(HAEM)
  
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param alive_, is vector of TRUE/FALSE that indicates whether a person in the
##'population matrix is alive
##'@return prob_smo is the matrix giving the probability of being a smoker next 
##'year from UKPDS 90

UKPDS_90_smo          <- function(population_, parameter_, alive_){
  
  prob_smo <- matrix(NA, nrow = length(population_[, "FEMALE"]),
                     ncol = 1)
  
  FV <- parameter_[,"SMO_UKPDS90_CONS"]+
    parameter_[,"SMO_UKPDS90_FEMALE"]*population_[,"FEMALE"][alive_]+
    parameter_[,"SMO_UKPDS90_AgeDiag"]*
    (population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
    parameter_[,"SMO_UKPDS90_SMOlastyear"]*population_[,"SMO"][alive_]+
    parameter_[,"SMO_UKPDS90_SMODiag"]*population_[,"SMO_0"][alive_]+ 
    parameter_[,"SMO_UKPDS90_DIABDUR"]*log(population_[,"DIAB_DUR"][alive_]+1)
  
  prob_smo <- 1/(1+exp(-FV))
  
  rm(FV)
  
  return(prob_smo)

}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param alive_, is vector of TRUE/FALSE that indicates whether a person in the
##'population matrix is alive
##'@return prob_MicAlb is the matrix giving the probability of having micro or
##'macro albuminuria next year from UKPDS 90

UKPDS_90_MICALB       <- function(population_, parameter_, alive_){
  
  MicAlb_start <- exp(parameter_[,"MICALB_CONS"]+
                        parameter_[,"MICALB_FEMALE"]*population_[,"FEMALE"][alive_]+
                        parameter_[,"MICALB_AgeDiag"]*
                             (population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
                        parameter_[,"MICALB_SMO"]*population_[,"SMO"][alive_]+
                        parameter_[,"MICALB_SBP_div_10"]*
                             (population_[,"SBP"][alive_]/10)+
                        parameter_[,"MICALB_HbA1c"]*population_[,"HBA"][alive_]+
                        parameter_[,"MICALB_HDL_mult_10"]*
                            (population_[,"HDL"][alive_]*10)+
                        parameter_[,"MICALB_BMI"]*
                        population_[,"BMI"][alive_])*(population_[,"DIAB_DUR"][alive_]^parameter_[,"MICALB_shape"])
  
  MicAlb_end <- exp(parameter_[,"MICALB_CONS"]+
                      parameter_[,"MICALB_FEMALE"]*population_[,"FEMALE"][alive_]+
                      parameter_[,"MICALB_AgeDiag"]*
                      (population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
                      parameter_[,"MICALB_SMO"]*population_[,"SMO"][alive_]+
                      parameter_[,"MICALB_SBP_div_10"]*
                      (population_[,"SBP"][alive_]/10)+
                      parameter_[,"MICALB_HbA1c"]*population_[,"HBA"][alive_]+
                      parameter_[,"MICALB_HDL_mult_10"]*
                      (population_[,"HDL"][alive_]*10)+
                      parameter_[,"MICALB_BMI"]*
                      population_[,"BMI"][alive_])*((population_[,"DIAB_DUR"][alive_]+1)^parameter_[,"MICALB_shape"])
  
 prob_MicAlb <- 1 - exp(MicAlb_start - MicAlb_end)
 #Make MMALB impossible for people with PVD history
 prob_MicAlb <- ifelse(population_[,"MMALB_H"][alive_]==1, 0, prob_MicAlb)
 
 #remove derived variables that aren't returned
 rm(MicAlb_start, MicAlb_end)
 
 return(prob_MicAlb)
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param alive_, is vector of TRUE/FALSE that indicates whether a person in the
##'population matrix is alive
##'@return prob_PVD is the matrix giving the probability of having PVD
##' next year from UKPDS 90

UKPDS_90_PVD          <- function(population_, parameter_, alive_){
  
  PVD_start <- exp(parameter_[,"PVD_Cons"]+
                     parameter_[,"PVD_AgeDiag"]*
                     (population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
                     parameter_[,"PVD_smoker"]*population_[,"SMO"][alive_]+
                     parameter_[,"PVD_SBP_div10"]*
                     (population_[,"SBP"][alive_]/10)+
                     parameter_[,"PVD_HbA1c"]*population_[,"HBA"][alive_]+
                     parameter_[,"PVD_BMI"]*population_[,"BMI"][alive_]+
                    parameter_[,"PVD_LDL_mult10"]*
                     (population_[,"LDL"][alive_]*10))*
    ((population_[,"DIAB_DUR"][alive_])^parameter_[,"PVD_Shape"])
  
  PVD_end <- exp(parameter_[,"PVD_Cons"]+
                    parameter_[,"PVD_AgeDiag"]*
                   (population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
                    parameter_[,"PVD_smoker"]*population_[,"SMO"][alive_]+
                    parameter_[,"PVD_SBP_div10"]*
                   (population_[,"SBP"][alive_]/10)+
                    parameter_[,"PVD_HbA1c"]*population_[,"HBA"][alive_]+
                    parameter_[,"PVD_BMI"]*population_[,"BMI"][alive_]+
                    parameter_[,"PVD_LDL_mult10"]*
                   (population_[,"LDL"][alive_]*10)
  )*((population_[,"DIAB_DUR"][alive_]+1)^parameter_[,"PVD_Shape"])
  
  prob_PVD <- 1 - exp(PVD_start - PVD_end)
  
  #Make PVD impossible for people with PVD history
  prob_PVD <- ifelse(population_[,"PVD_H"]==1, 0, prob_PVD)
  #remove derived variables that are not returned
  rm(PVD_start, PVD_end)
  #Return the probability that someone has PVD
  return(prob_PVD)
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param alive_, is vector of TRUE/FALSE that indicates whether a person in the
##'population matrix is alive
##'@return prob_ATFIB is the matrix giving the probability of having ATFIB
##' next year from UKPDS 90

UKPDS_90_ATFIB        <- function(population_, parameter_, alive_){
  
  ATFIB_start <- exp(parameter_[,"ATFIB_CONS"]+
                       parameter_[,"ATFIB_AGEDAIG"]*
                       (population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
                       parameter_[,"ATFIB_BMI"]*population_[,"BMI"][alive_]
  )*(population_[,"DIAB_DUR"][alive_])
  
  ATFIB_end <- exp(parameter_[,"ATFIB_CONS"]+
                     parameter_[,"ATFIB_AGEDAIG"]*
                   (population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
                     parameter_[,"ATFIB_BMI"]*population_[,"BMI"][alive_]
  )*(population_[,"DIAB_DUR"][alive_]+1)
  
  prob_ATFIB <- 1 - exp(ATFIB_start - ATFIB_end)
  
  #Make ATFIB impossible for people with PVD history
  prob_ATFIB <- ifelse(population_[,"ATFIB_H"][alive_]==1, 0, prob_ATFIB)
  
  #remove unnecessary derived variables
  rm(ATFIB_start,ATFIB_end )
  return(prob_ATFIB)
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param alive_, is vector of TRUE/FALSE that indicates whether a person in the
##'population matrix is alive
##'@return prob_eGRFu60 is the vector giving the probability of having an eGFR
##'under 60 next year


UKPDS_90_binrary_peGFRu60 <- function(population_, parameter_, alive_){
  
  eGFR_start <- exp(parameter_[,"eGFR_bu60_CONS"]+
                      parameter_[,"eGFR_bu60_FEMALE"]*population_[,"FEMALE"][alive_]+
                      parameter_[,"eGFR_bu60_AFRO"]*population_[,"AFRO"][alive_]+
                      parameter_[,"eGFR_bu60_INDIAN"]*population_[,"INDIAN"][alive_]+
                      parameter_[,"eGFR_bu60_AGEDAIG"]*
                     (population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
                      parameter_[,"eGFR_bu60_SBP_div10"]*
                     (population_[,"SBP"][alive_]/10)+
                      parameter_[,"eGFR_bu60_BMI"]*population_[,"BMI"][alive_]+
                      parameter_[,"eGFR_bu60_HDL_div10"]*
                      (population_[,"HDL"][alive_]*10)+
                      parameter_[,"eGFR_bu60_LDL_div10"]*
                      (population_[,"LDL"][alive_]*10)
  )*((population_[,"DIAB_DUR"][alive_])^parameter_[,"eGFR_bu60_Shape"])
  
  eGFR_end <- exp(parameter_[,"eGFR_bu60_CONS"]+
                    parameter_[,"eGFR_bu60_FEMALE"]*population_[,"FEMALE"][alive_]+
                    parameter_[,"eGFR_bu60_AFRO"]*population_[,"AFRO"][alive_]+
                    parameter_[,"eGFR_bu60_INDIAN"]*population_[,"INDIAN"][alive_]+
                    parameter_[,"eGFR_bu60_AGEDAIG"]*
                    (population_[,"AGE"][alive_]-population_[,"DIAB_DUR"][alive_])+
                    parameter_[,"eGFR_bu60_SBP_div10"]*
                    (population_[,"SBP"][alive_]/10)+
                    parameter_[,"eGFR_bu60_BMI"]*population_[,"BMI"][alive_]+
                    parameter_[,"eGFR_bu60_HDL_div10"]*
                    (population_[,"HDL"][alive_]*10)+
                    parameter_[,"eGFR_bu60_LDL_div10"]*
                    (population_[,"LDL"][alive_]*10)
  )*((population_[,"DIAB_DUR"][alive_]+1)^parameter_[,"eGFR_bu60_Shape"])
  
  prob_eGRFu60 <- 1 - exp(eGFR_start - eGFR_end)
  
  #remove unecessary derived variables
  rm(eGFR_start,eGFR_end)
  
  #People with eGFR < 60 cannot go above 60 in the UKPDS 90 framework 
  #(it is a time to event equation)
  #For these people, the probability that eGFR is under 60 is set to 1
  prob_eGRFu60 <- ifelse(population_[,"eGFR"][alive_] < 60, 1, prob_eGRFu60)
  
  return(prob_eGRFu60)
}

##'@param population_, is the population matrix
##'@param parameter_, is the row of the parameter matrix for this model run
##'@param alive_, is vector of TRUE/FALSE that indicates whether a person in the
##'population matrix is alive
##'@param eGRFu60_, is a vector, when it is equal to 1 the person has an eGFR
##'of under 60, 0 otherwise
##'@return egfr is the eGFR of the each patient next year

UKPDS_90_eGFR         <- function(population_, parameter_, eGRFu60_, alive_){
  FV_u60 <- parameter_[,"eGFR_cu60_CONS"]+
    parameter_[,"eGFR_cu60_Female"]*population_[,"FEMALE"][alive_]+
    parameter_[,"eGFR_cu60_AFRO"]*population_[,"AFRO"][alive_]+
    parameter_[,"eGFR_cu60_INDIAN"]*population_[,"INDIAN"][alive_]+
    parameter_[,"eGFR_cu60_eGFR"]*population_[,"eGFR"][alive_]+
    parameter_[,"eGFR_cu60_firsteGFR"]*population_[,"eGFR_0"][alive_]+ 
    parameter_[,"eGFR_cu60_DIABDUR"]*log(population_[,"DIAB_DUR"][alive_]+1)
  
    pnorm1 <- pnorm((0-FV_u60)/parameter_[,"eGFR_cu60_Shape"])
    pnorm2 <- pnorm((60-FV_u60)/parameter_[,"eGFR_cu60_Shape"])
    dnorm1 <- dnorm((0-FV_u60)/parameter_[,"eGFR_cu60_Shape"])
    dnorm2 <- dnorm((60-FV_u60)/parameter_[,"eGFR_cu60_Shape"])
    
  FV_o60 <- parameter_[,"eGFR_co60_CONS"]+
    parameter_[,"eGFR_co60_Female"]*population_[,"FEMALE"][alive_]+
    parameter_[,"eGFR_co60_AFRO"]*population_[,"AFRO"][alive_]+
    parameter_[,"eGFR_co60_INDIAN"]*population_[,"INDIAN"][alive_]+
    parameter_[,"eGFR_co60_eGFR"]*population_[,"eGFR"][alive_]+
    parameter_[,"eGFR_co60_firsteGFR"]*population_[,"eGFR_0"][alive_]+ 
    parameter_[,"eGFR_co60_DIABDUR"]*log(population_[,"DIAB_DUR"][alive_]+1) 
    
    pnorm3 <- pnorm((60-FV_o60)/parameter_[,"eGFR_co60_Shape"])
    dnorm3 <- dnorm((60-FV_o60)/parameter_[,"eGFR_co60_Shape"])
    
    #If the patient's egfr is predicted to be under 60 apply one tobit model, if 
    #not apply the other
    egfr <-ifelse(eGRFu60_==1,
                 FV_u60-parameter_[,"eGFR_cu60_Shape"]*((dnorm1-dnorm2)/(pnorm1-pnorm2)),
                 FV_o60+parameter_[,"eGFR_co60_Shape"]*((dnorm3)/(1-pnorm3)))
    
    ##remove other all dervied values except current eGFR
    rm(FV_u60, FV_o60, pnorm1, pnorm2, dnorm1, dnorm2, pnorm3, dnorm3)
    
    return(egfr)
}

UKPDS_90_underlying_traj <- function(population_, coef_, endtime_){
  #store all results
  HbA1c_UKPDStraj   <- UKPDS_90_contrisk_A1c(population_, coef_, endtime_)
  SBP_UKPDStraj     <- UKPDS_90_contrisk_SBP(population_, coef_, endtime_)
  LDL_UKPDStraj     <- UKPDS_90_contrisk_LDL(population_, coef_, endtime_)
  HDL_UKPDStraj     <- UKPDS_90_contrisk_HDL(population_, coef_, endtime_)
  BMI_UKPDStraj     <- UKPDS_90_contrisk_BMI(population_, coef_, endtime_)
  HR_UKPDStraj      <- UKPDS_90_HEARTR(population_, coef_, endtime_)
  WBC_UKPDStraj     <- UKPDS_90_WBC(population_, coef_, endtime_)
  HAEM_UKPDStraj    <- UKPDS_90_HAEM(population_, coef_, endtime_)
  #push these matrices to the global environment
  for (variable in ls()) {
    assign(variable, get(variable), envir = .GlobalEnv)
  }
 
}