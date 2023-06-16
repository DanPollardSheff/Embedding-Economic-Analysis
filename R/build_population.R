#' Generate the baseline population
#' @param diab_diab_population_ is a matrix of baseline characteristics read into 
#' this function
#' @param PopulationVariables_ is a list of all the variables in the population of this 
#' model
#' @param GlobalVars_ is the global variables matrix
#' @return population is a matrix of patient characteristics for use in the model
build_population <- function(diag_diab_population_, PopulationVariables_, GlobalVars_) {
  #limit the raw population characteristics to be the same as the number se
  #in the global varaibles
  diag_diab_population_ <- diag_diab_population_[1:as.numeric(GlobalVars_["n","Value"]),]
  #stop the simulation if there are too few patients in the csv files
  if(length(diag_diab_population_[,"CURR_AGE"]) > as.numeric(GlobalVars_["n","Value"])){
    stop("there are too few patients in the csv files, some patients will get 0 for
         their characteristics in the simulation")
  }
  
  
  n <- nrow(diag_diab_population_)  
  year <- 0
  population <- matrix(0, nrow = n, ncol = length(PopulationVariables_[,"Variable"]))
  colnames(population) <- PopulationVariables_[,"Variable"]
  population[, "ID"] <- 1:n
  population[, "CONS"] <- 1
  population[, "T"] <- 0
  population[, "AGE_0"] <- floor(diag_diab_population_[, "CURR_AGE"])
  population[, "MALE"] <- (-1*diag_diab_population_[, "Female"])+1
  population[, "FEMALE"] <- diag_diab_population_[, "Female"]
  
  #Read in a dummy variable for Afro-caribean descent (1= afro-caribean, 0=otherwise)
  population[, "AFRO"] <- diag_diab_population_[,"AFRO"]
  population[,"INDIAN"] <- diag_diab_population_[,"INDIAN"]
  #Set the binary variable for smoking status
  population[, "SMO"] <- ifelse(diag_diab_population_[, "Smoking"]>0,1,0)
  #record the history of diabetes (1 = yes, 0 = no)
  
  population[, "AGE"] <- floor(diag_diab_population_[, "CURR_AGE"])
  population[, "MEN"] <- replace(population[, "MEN"], population[, "AGE"] > 51 & population[, "FEMALE"] == 1, 1)
  population[, "HBA"] <- round(diag_diab_population_[, "HbA1c"],1)
  #This has been kept the same as in the SPHR diabetes model
  #Record the BMI of the population
  population[, "BMI"] <- diag_diab_population_[, "BMI"]
  #Apply a logical constraint to the BMI so that it cannot be less than 5kg/m2
  population[, "BMI"] <- replace(diag_diab_population_[, "BMI"], population[, "BMI"] < 5, 5)
  #Record the HDL cholesterol (add units)
  population[, "HDL"] <- diag_diab_population_[, "HDL"]
  #Record systolic blood pressure(add units)
  population[, "SBP"] <- diag_diab_population_[, "SBP"]
  
  
  #Use the Ara formula to estimate baseline QALYs
  #as there is a parameter determining baseline utility, set these two values to 0 for the time being
  population[, "QALY"] <- 0 
  population[, "EQ5D"] <- 0
  
  population[, "DEP_H"] <- runif(n) < (435/4781) #Source: Ali et al 2009. Prevalence of diagnosed depression in South Asian
  #and white European people with type 1 and type 2 diabetes mellitus in a UK secondary care population
  
  #Record the baseline BMI of the population in the HSE data
  population[,"BMI_0"] <- diag_diab_population_[,"Baseline_BMI"]
  
  #Record whether someone is on 1st line therapy for T2DM
  population[, "MET"]<-diag_diab_population_[,"MET"]
  #Record whether someone is on 2nd line therapy for T2DM
  population[,"MET2"]<- diag_diab_population_[,"MET2"]
  #Record whether someone is on 3rd line therapy for T2DM
  population[,"INSU"]<- diag_diab_population_[,"INSU"]
  
  ##parameters for the treatment version of the SPHR diabetes model 
  population[,"AMP_E"] <- diag_diab_population_[,"AMP_E"]
  population[,"AMP_H"] <- diag_diab_population_[,"AMP_H"]
  population[,"AMP2_H"] <- diag_diab_population_[,"AMP2_H"]
  population[,"IHD_E"] <- diag_diab_population_[,"IHD_E"]
  population[,"IHD_H"] <- diag_diab_population_[,"IHD_H"]
  population[,"CHF_E"] <- diag_diab_population_[,"CHF_E"]
  population[,"CHF_H"] <- diag_diab_population_[,"CHF_H"]
  population[,"RENAL_E"] <- diag_diab_population_[,"RENAL_E"]
  population[,"RENAL_H"] <- diag_diab_population_[,"RENAL_H"]
  population[,"STRO_E"] <- diag_diab_population_[,"STROKE_E"]
  population[,"STRO_H"] <- diag_diab_population_[,"STROKE_H"]
  population[,"MI_E"] <- diag_diab_population_[,"MI_E"]
  population[,"MI_H"] <- diag_diab_population_[,"MI_H"]
  population[,"MI2_E"] <- diag_diab_population_[,"MI2_E"]
  population[,"MI2_H"] <- diag_diab_population_[,"MI2_H"]
  population[,"BLIND_E"] <- diag_diab_population_[,"BLIND_E"]
  population[,"BLIND_H"] <- diag_diab_population_[,"BLIND_H"]
  population[,"MMALB_H"] <- diag_diab_population_[,"MIC_ALB"]
  population[,"ATFIB_H"] <- diag_diab_population_[,"ATFIB"]
  population[,"PVD_H"] <- diag_diab_population_[,"PVD"]
  
  #Add in diabetes treatment specific biomarkers
  population[,"HAEM"] <- diag_diab_population_[,"HAEM"]
  population[,"WBC"] <- diag_diab_population_[,"WBC"]
  population[,"eGFR"] <- diag_diab_population_[,"eGFR"]
  population[,"eGFR_U_60"] <- ifelse(diag_diab_population_[,"eGFR"]<60,diag_diab_population_[,"eGFR"],60)
  population[,"eGFR_O_60"] <- ifelse(diag_diab_population_[,"eGFR"]>60,diag_diab_population_[,"eGFR"]-60,0)
  population[,"HEART_R"] <- diag_diab_population_[,"Heart.rate"]
  population[,"BMI_U_18_5"] <- ifelse(population[,"BMI"]<18.5,1,0)
  population[,"BMI_O_E_25"] <- ifelse(population[,"BMI"]>=25,1,0)
  population[,"LDL"] <- diag_diab_population_[,"LDL"]
  population[,"LDL_O_35"] <- ifelse(population[,"LDL"]>3.5, population[,"LDL"]-3.5,0)
  #add in diabetes treatment specific chars
  population[,"DIAB_DUR"] <- diag_diab_population_[,"DIAB_DUR"]
  
  #add in values for first observations for each risk factor needed
  #in the absence of information this will be the baseline value
  population[,"HBA_0"] <- diag_diab_population_[,"Baseline_HbA1c"]
  population[,"LDL_0"] <- diag_diab_population_[,"Baseline_LDL"]
  population[,"HDL_0"] <- diag_diab_population_[,"Baseline_HDL"]
  population[,"HEART_R_0"] <- population[,"HEART_R"]
  population[,"HAEM_0"] <- population[,"HAEM"]
  population[,"WBC_0"] <- population[,"WBC"]
  population[,"eGFR_0"] <- population[,"eGFR"]
  population[,"SMO_0"] <- population[,"SMO"]
  population[,"SBP_0"] <- diag_diab_population_[,"Baseline_SBP"]
  
  #Give noone a history of Ulcers
  population[,"ULCER_H"] <-  0
  
  #Make all cause death missing, as missing indicates someone is alive in the code
  population[, "F_ALLCAUSE"] <- NA
  #Make smoking missing, so you can easily see whether smoking is assessed
  population[,"p_SMO"]<- NA
  
  return(population)
}