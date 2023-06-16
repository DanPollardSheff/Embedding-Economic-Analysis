##'@param n_ is the number of patients in the model
##'@param treatment_ is a text term indicating the current treatment option
##'@param parameter_ is the row of the parameter matrix
##'@param endtime_ is a number indicating how many years the simulation is being run for
##'@param GlobalVars_ is the Global Variables matrix, this allows the duration of treatment 
##' effect to be controlled
##'@param attend_se_ is a TRUE/FALSE vector that indicates whether a patient attends
##' a structured education course in each year 
##'@return INTE_A1c is a matrix that gives the reduction in A1c for each patient 
##'in each year compared to a patient on a "normal" trajectory

initialise_intervention_dt_HbA1c <- function(n_,
                                             treatment_, 
                                             parameter_,
                                             endtime_, 
                                             GlobalVars_, 
                                             attend_se_) {
  INTE_A1c <- matrix(data=0, nrow = n_, ncol =endtime_+2)
  INTE_A1c[,1] <- 1:n_ #make the first column equivalent to the patient ID for matching later on
  if(treatment_ == "test" | treatment_ == "Mt_HOOD_2018_QOL_A1c"){
    INTE_A1c[,2:(endtime_+2)] <- -0.5
  }else if (treatment_ == "Embedding_TrialEffect_All" | treatment_ == "Embedding_TrialEffect_PriandSS"){
    INTE_A1c[,2] <- parameter_[,"Intv_Embedding_HbA1c"]
    
    ####Duration scenarios
    if(as.numeric(GlobalVars_["Treatment effect duration","Value"])==3){#effects last until year 3
      INTE_A1c[,3:4]  <- parameter_[,"Intv_Embedding_HbA1c_2yr"]
    }else if (as.numeric(GlobalVars_["Treatment effect duration","Value"])==5){#effects last until year 5
      INTE_A1c[,3:6]  <- parameter_[,"Intv_Embedding_HbA1c_2yr"]
    }else{#effects last 10 years
      INTE_A1c[,3:11]  <- parameter_[,"Intv_Embedding_HbA1c_2yr"] 
    }
  }else if (treatment_ == "Embedding_TrialEffect_All_1yr"){
    INTE_A1c[,2] <- parameter_[,"Intv_Embedding_HbA1c"]
    
    ####Duration scenarios
    if(as.numeric(GlobalVars_["Treatment effect duration","Value"])==3){#effects last until year 3
      INTE_A1c[,3:4]  <- parameter_[,"Intv_Embedding_HbA1c"] 
    } else if (as.numeric(GlobalVars_["Treatment effect duration","Value"])==5){#effects last until year 5
      INTE_A1c[,3:6]  <- parameter_[,"Intv_Embedding_HbA1c"]
    }else{#effects last 10 years
      INTE_A1c[,3:11]  <- parameter_[,"Intv_Embedding_HbA1c"]
    }
    
  }else if (treatment_ == "Embedding_MetaAnalysis_All" | 
            treatment_ == "Control_MetaAnalysis_all" |
            treatment_ == "Embedding_MetaAnalysis_1yr"){

    ####Duration scenarios
    if(as.numeric(GlobalVars_["Treatment effect duration","Value"])==101){
      #Lifetime Effect
      INTE_A1c[,2:(endtime_+2)]  <- ifelse(attend_se_[,2]==1,
                                 parameter_[,"Intv_MA_HbA1c"],
                                 0) 
      INTE_A1c[,3:(endtime_+2)]  <- ifelse(attend_se_[,3]==1,
                                 parameter_[,"Intv_MA_HbA1c"],
                                 INTE_A1c[,3:endtime_+2]) 
      
    }else if (as.numeric(GlobalVars_["Treatment effect duration","Value"])==15){
      #10 years of full effect followed by 5 year waning
      #Create a matrix to store the effects
      HbA1c <- matrix(data=0, nrow = 1, ncol = 15)
      NoHbA1c <- matrix(data=0, nrow = 1, ncol = 15)
      #Trajectory for people who se in year 1
      HbA1c[1:10]  <- parameter_[,"Intv_MA_HbA1c"]
      
      HbA1c[11] <- parameter_[,"Intv_MA_HbA1c"] - (1*parameter_[,"Intv_MA_HbA1c"]/5)
      
      HbA1c[12] <- parameter_[,"Intv_MA_HbA1c"] - (2*parameter_[,"Intv_MA_HbA1c"]/5)
      
      HbA1c[13] <- parameter_[,"Intv_MA_HbA1c"] - (3*parameter_[,"Intv_MA_HbA1c"]/5)
      
      HbA1c[14] <- parameter_[,"Intv_MA_HbA1c"] - (4*parameter_[,"Intv_MA_HbA1c"]/5)
      
      #Year 1 effect for people who went to an SME course in year 1
      INTE_A1c[,2] <- ifelse(attend_se_[,2]==1, HbA1c[1],0)
      #Year 2 effect for people who went to an SME course in year 1
      #Year 1 effect for people who went to an SME course in year 2
      INTE_A1c[,3] <- ifelse(attend_se_[,2]==1, HbA1c[2],0)
      INTE_A1c[,3] <- ifelse(attend_se_[,3]==1, HbA1c[1],INTE_A1c[,3])
      #Year 3 effect for people who went to an SME course in year 1
      #Year 2 effect for people who went to an SME course in year 2
      INTE_A1c[,4] <- ifelse(attend_se_[,2]==1, HbA1c[3],0)
      INTE_A1c[,4] <- ifelse(attend_se_[,3]==1, HbA1c[2],INTE_A1c[,4])
      #Year 4 effect for people who went to an SME course in year 1
      #Year 3 effect for people who went to an SME course in year 2
      INTE_A1c[,5] <- ifelse(attend_se_[,2]==1, HbA1c[4],0)
      INTE_A1c[,5] <- ifelse(attend_se_[,3]==1, HbA1c[3],INTE_A1c[,5])
      #Year 5 effect for people who went to an SME course in year 1
      #Year 4 effect for people who went to an SME course in year 2
      INTE_A1c[,6] <- ifelse(attend_se_[,2]==1, HbA1c[5],0)
      INTE_A1c[,6] <- ifelse(attend_se_[,3]==1, HbA1c[4],INTE_A1c[,6])
      #Year 6 effect for people who went to an SME course in year 1
      #Year 5 effect for people who went to an SME course in year 2
      INTE_A1c[,7] <- ifelse(attend_se_[,2]==1, HbA1c[6],0)
      INTE_A1c[,7] <- ifelse(attend_se_[,3]==1, HbA1c[5],INTE_A1c[,7])
      #Year 7 effect for people who went to an SME course in year 1
      #Year 6 effect for people who went to an SME course in year 2
      INTE_A1c[,8] <- ifelse(attend_se_[,2]==1, HbA1c[7],0)
      INTE_A1c[,8] <- ifelse(attend_se_[,3]==1, HbA1c[6],INTE_A1c[,8])
      #Year 8 effect for people who went to an SME course in year 1
      #Year 7 effect for people who went to an SME course in year 2
      INTE_A1c[,9] <- ifelse(attend_se_[,2]==1, HbA1c[8],0)
      INTE_A1c[,9] <- ifelse(attend_se_[,3]==1, HbA1c[7],INTE_A1c[,9])
      #Year 9 effect for people who went to an SME course in year 1
      #Year 8 effect for people who went to an SME course in year 2
      INTE_A1c[,10] <- ifelse(attend_se_[,2]==1, HbA1c[9],0)
      INTE_A1c[,10] <- ifelse(attend_se_[,3]==1, HbA1c[8],INTE_A1c[,10])
      #Year 10 effect for people who went to an SME course in year 1
      #Year 9 effect for people who went to an SME course in year 2
      INTE_A1c[,11] <- ifelse(attend_se_[,2]==1, HbA1c[10],0)
      INTE_A1c[,11] <- ifelse(attend_se_[,3]==1, HbA1c[9],INTE_A1c[,11])
      #Year 11 effect for people who went to an SME course in year 1
      #Year 10 effect for people who went to an SME course in year 2
      INTE_A1c[,12] <- ifelse(attend_se_[,2]==1, HbA1c[11],0)
      INTE_A1c[,12] <- ifelse(attend_se_[,3]==1, HbA1c[10],INTE_A1c[,12])
      #Year 12 effect for people who went to an SME course in year 1
      #Year 11 effect for people who went to an SME course in year 2
      INTE_A1c[,13] <- ifelse(attend_se_[,2]==1, HbA1c[12],0)
      INTE_A1c[,13] <- ifelse(attend_se_[,3]==1, HbA1c[11],INTE_A1c[,13])
      #Year 13 effect for people who went to an SME course in year 1
      #Year 12 effect for people who went to an SME course in year 2
      INTE_A1c[,14] <- ifelse(attend_se_[,2]==1, HbA1c[13],0)
      INTE_A1c[,14] <- ifelse(attend_se_[,3]==1, HbA1c[12],INTE_A1c[,14])
      #Year 14 effect for people who went to an SME course in year 1
      #Year 13 effect for people who went to an SME course in year 2
      INTE_A1c[,15] <- ifelse(attend_se_[,2]==1, HbA1c[14],0)
      INTE_A1c[,15] <- ifelse(attend_se_[,3]==1, HbA1c[13],INTE_A1c[,15])
      #Year 15 effect for people who went to an SME course in year 1
      #Year 14 effect for people who went to an SME course in year 2
      INTE_A1c[,16] <- ifelse(attend_se_[,2]==1, HbA1c[15],0)
      INTE_A1c[,16] <- ifelse(attend_se_[,3]==1, HbA1c[14],INTE_A1c[,16])
      #Year 14 effect for people who went to an SME course in year 2
      INTE_A1c[,17] <- ifelse(attend_se_[,3]==1, HbA1c[15],0)
    }else{
      #Create a matrix to store the effects
      HbA1c <- matrix(data=0, nrow = 1, ncol = 10)
      NoHbA1c <- matrix(data=0, nrow = 1, ncol = 10)
      #Trajectory for people who se in year 1
      HbA1c[1:6]  <- parameter_[,"Intv_MA_HbA1c"]
      
      HbA1c[7] <-parameter_[,"Intv_MA_HbA1c"] - (1*parameter_[,"Intv_MA_HbA1c"]/4)
      
      HbA1c[8] <-parameter_[,"Intv_MA_HbA1c"] - (2*parameter_[,"Intv_MA_HbA1c"]/4)
      
      HbA1c[9] <-parameter_[,"Intv_MA_HbA1c"] - (3*parameter_[,"Intv_MA_HbA1c"]/4)
      
      #Year 1 effect for people who went to an SME course in year 1
      INTE_A1c[,2] <- ifelse(attend_se_[,2]==1, HbA1c[1],0)
      #Year 2 effect for people who went to an SME course in year 1
      #Year 1 effect for people who went to an SME course in year 2
      INTE_A1c[,3] <- ifelse(attend_se_[,2]==1, HbA1c[2],0)
      INTE_A1c[,3] <- ifelse(attend_se_[,3]==1, HbA1c[1],INTE_A1c[,3])
      #Year 3 effect for people who went to an SME course in year 1
      #Year 2 effect for people who went to an SME course in year 2
      INTE_A1c[,4] <- ifelse(attend_se_[,2]==1, HbA1c[3],0)
      INTE_A1c[,4] <- ifelse(attend_se_[,3]==1, HbA1c[2],INTE_A1c[,4])
      #Year 4 effect for people who went to an SME course in year 1
      #Year 3 effect for people who went to an SME course in year 2
      INTE_A1c[,5] <- ifelse(attend_se_[,2]==1, HbA1c[4],0)
      INTE_A1c[,5] <- ifelse(attend_se_[,3]==1, HbA1c[3],INTE_A1c[,5])
      #Year 5 effect for people who went to an SME course in year 1
      #Year 4 effect for people who went to an SME course in year 2
      INTE_A1c[,6] <- ifelse(attend_se_[,2]==1, HbA1c[5],0)
      INTE_A1c[,6] <- ifelse(attend_se_[,3]==1, HbA1c[4],INTE_A1c[,6])
      #Year 6 effect for people who went to an SME course in year 1
      #Year 5 effect for people who went to an SME course in year 2
      INTE_A1c[,7] <- ifelse(attend_se_[,2]==1, HbA1c[6],0)
      INTE_A1c[,7] <- ifelse(attend_se_[,3]==1, HbA1c[5],INTE_A1c[,7])
      #Year 7 effect for people who went to an SME course in year 1
      #Year 6 effect for people who went to an SME course in year 2
      INTE_A1c[,8] <- ifelse(attend_se_[,2]==1, HbA1c[7],0)
      INTE_A1c[,8] <- ifelse(attend_se_[,3]==1, HbA1c[6],INTE_A1c[,8])
      #Year 8 effect for people who went to an SME course in year 1
      #Year 7 effect for people who went to an SME course in year 2
      INTE_A1c[,9] <- ifelse(attend_se_[,2]==1, HbA1c[8],0)
      INTE_A1c[,9] <- ifelse(attend_se_[,3]==1, HbA1c[7],INTE_A1c[,9])
      #Year 9 effect for people who went to an SME course in year 1
      #Year 8 effect for people who went to an SME course in year 2
      INTE_A1c[,10] <- ifelse(attend_se_[,2]==1, HbA1c[9],0)
      INTE_A1c[,10] <- ifelse(attend_se_[,3]==1, HbA1c[8],INTE_A1c[,10])
      #Year 10 effect for people who went to an SME course in year 1
      #Year 9 effect for people who went to an SME course in year 2
      INTE_A1c[,11] <- ifelse(attend_se_[,2]==1, HbA1c[10],0)
      INTE_A1c[,11] <- ifelse(attend_se_[,3]==1, HbA1c[9],INTE_A1c[,11])
      #Year 10 effect for people who went to an SME course in year 2
      INTE_A1c[,12] <- ifelse(attend_se_[,3]==1, HbA1c[10],0)
    }
  }else{ #if no treatment option is selected, leave them at baseline values
    INTE_A1c[,2:(endtime_+2)] <- 0
  }
  
  return(INTE_A1c)
}

##'@param n_ is the number of patients in the model
##'@param treatment_ is a text term indicating the current treatment option
##'@param parameter_ is the row of the parameter matrix
##'@param endtime_ is a number indicating how many years the simulation is being run for
##'@param GlobalVars_
##'@param attend_se_ is three column matrix. Column 1 is patient ID, column 2 is 
##'a 0,1 vector with 1 indicating attendance at an SE course in the first model year,
##'column 3 is a 0,1 vector with 1 indicating attendance at an SE course in the second
##'model year 
##'@return INTE_BMI is a matrix that gives the reduction in BMI for each patient 
##'in each year

initialise_intervention_dt_BMI <- function(n_,
                                           treatment_, 
                                           parameter_,
                                           endtime_,
                                           GlobalVars_, 
                                           attend_se_) {
  INTE_BMI <- matrix(data=0, nrow = n_, ncol =endtime_+2)
  INTE_BMI[,1] <- 1:n_ #make the first column equivalent to the patient ID for matching later on
  if(treatment_ == "test"|treatment_ == "Mt_HOOD_2018_QOL_BMI"){
    INTE_BMI[,2:(endtime_+2)] <- -1
  }else if (treatment_ == "Embedding_TrialEffect_All"){
    INTE_BMI[,2] <- parameter_[,"Intv_Embedding_BMI"]
    
    ####Duration scenarios
    if(as.numeric(GlobalVars_["Treatment effect duration", "Value"])==3){#effects last until year 3
      INTE_BMI[,3:4]  <- parameter_[,"Intv_Embedding_BMI"]
    }else if (as.numeric(GlobalVars_["Treatment effect duration", "Value"])==5){#effects last until year 5
      INTE_BMI[,3:6]  <- parameter_[,"Intv_Embedding_BMI"]
    }else{#effects last 10 years
      INTE_BMI[,3:11]  <- parameter_[,"Intv_Embedding_BMI"] 
    }
  } else if (treatment_ == "Embedding_TrialEffect_All_1yr"){
    INTE_BMI[,2] <- parameter_[,"Intv_Embedding_BMI"]
    
    ####Duration scenarios
    if(as.numeric(GlobalVars_["Treatment effect duration", "Value"])==10){#effects last until year 10
      INTE_BMI[,3:11]  <- parameter_[,"Intv_Embedding_BMI"]
    } else if (as.numeric(GlobalVars_["Treatment effect duration", "Value"])==5){#effects last until year 5
      INTE_BMI[,3:6]  <- parameter_[,"Intv_Embedding_BMI"]
    }else{#effects last 3 years
      INTE_BMI[,3:4]  <- parameter_[,"Intv_Embedding_BMI"] 
    }
    
  }else if (treatment_ == "Embedding_MetaAnalysis_All" | 
            treatment_ == "Control_MetaAnalysis_all" |
            treatment_ == "Embedding_MetaAnalysis_1yr"){
    
    ####Duration scenarios
    if(as.numeric(GlobalVars_["Treatment effect duration", "Value"])==101){
      #Lifetime Effect
      INTE_BMI[,2:(endtime_+2)]  <- ifelse(attend_se_[,2]==1,
                                 parameter_[,"Intv_MA_BMI"],
                                 0) 
      INTE_BMI[,3:(endtime_+2)]  <- ifelse(attend_se_[,3]==1,
                                 parameter_[,"Intv_MA_BMI"],
                                 INTE_BMI[,3:(endtime_+2)]) 
      
    } else if (as.numeric(GlobalVars_["Treatment effect duration", "Value"])==15){
      #10 years of full effect followed by 5 year waning
      #Create a matrix to store the effects
      BMI <- matrix(data=0, nrow = 1, ncol = 15)
      #Trajectory for people who se in year 1
      BMI[1:10]  <- parameter_[,"Intv_MA_BMI"]
      
      BMI[11] <- parameter_[,"Intv_MA_BMI"] - (1*parameter_[,"Intv_MA_BMI"]/5)
      
      BMI[12] <- parameter_[,"Intv_MA_BMI"] - (2*parameter_[,"Intv_MA_BMI"]/5)
      
      BMI[13] <- parameter_[,"Intv_MA_BMI"] - (3*parameter_[,"Intv_MA_BMI"]/5)
      
      BMI[14] <- parameter_[,"Intv_MA_BMI"] - (4*parameter_[,"Intv_MA_BMI"]/5)
      
      #Year 1 effect for people who went to an SME course in year 1
      INTE_BMI[,2] <- ifelse(attend_se_[,2]==1, BMI[1],0)
      #Year 2 effect for people who went to an SME course in year 1
      #Year 1 effect for people who went to an SME course in year 2
      INTE_BMI[,3] <- ifelse(attend_se_[,2]==1, BMI[2],0)
      INTE_BMI[,3] <- ifelse(attend_se_[,3]==1, BMI[1],INTE_BMI[,3])
      #Year 3 effect for people who went to an SME course in year 1
      #Year 2 effect for people who went to an SME course in year 2
      INTE_BMI[,4] <- ifelse(attend_se_[,2]==1, BMI[3],0)
      INTE_BMI[,4] <- ifelse(attend_se_[,3]==1, BMI[2],INTE_BMI[,4])
      #Year 4 effect for people who went to an SME course in year 1
      #Year 3 effect for people who went to an SME course in year 2
      INTE_BMI[,5] <- ifelse(attend_se_[,2]==1, BMI[4],0)
      INTE_BMI[,5] <- ifelse(attend_se_[,3]==1, BMI[3],INTE_BMI[,5])
      #Year 5 effect for people who went to an SME course in year 1
      #Year 4 effect for people who went to an SME course in year 2
      INTE_BMI[,6] <- ifelse(attend_se_[,2]==1, BMI[5],0)
      INTE_BMI[,6] <- ifelse(attend_se_[,3]==1, BMI[4],INTE_BMI[,6])
      #Year 6 effect for people who went to an SME course in year 1
      #Year 5 effect for people who went to an SME course in year 2
      INTE_BMI[,7] <- ifelse(attend_se_[,2]==1, BMI[6],0)
      INTE_BMI[,7] <- ifelse(attend_se_[,3]==1, BMI[5],INTE_BMI[,7])
      #Year 7 effect for people who went to an SME course in year 1
      #Year 6 effect for people who went to an SME course in year 2
      INTE_BMI[,8] <- ifelse(attend_se_[,2]==1, BMI[7],0)
      INTE_BMI[,8] <- ifelse(attend_se_[,3]==1, BMI[6],INTE_BMI[,8])
      #Year 8 effect for people who went to an SME course in year 1
      #Year 7 effect for people who went to an SME course in year 2
      INTE_BMI[,9] <- ifelse(attend_se_[,2]==1, BMI[8],0)
      INTE_BMI[,9] <- ifelse(attend_se_[,3]==1, BMI[7],INTE_BMI[,9])
      #Year 9 effect for people who went to an SME course in year 1
      #Year 8 effect for people who went to an SME course in year 2
      INTE_BMI[,10] <- ifelse(attend_se_[,2]==1, BMI[9],0)
      INTE_BMI[,10] <- ifelse(attend_se_[,3]==1, BMI[8],INTE_BMI[,10])
      #Year 10 effect for people who went to an SME course in year 1
      #Year 9 effect for people who went to an SME course in year 2
      INTE_BMI[,11] <- ifelse(attend_se_[,2]==1, BMI[10],0)
      INTE_BMI[,11] <- ifelse(attend_se_[,3]==1, BMI[9],INTE_BMI[,11])
      #Year 11 effect for people who went to an SME course in year 1
      #Year 10 effect for people who went to an SME course in year 2
      INTE_BMI[,12] <- ifelse(attend_se_[,2]==1, BMI[11],0)
      INTE_BMI[,12] <- ifelse(attend_se_[,3]==1, BMI[10],INTE_BMI[,12])
      #Year 12 effect for people who went to an SME course in year 1
      #Year 11 effect for people who went to an SME course in year 2
      INTE_BMI[,13] <- ifelse(attend_se_[,2]==1, BMI[12],0)
      INTE_BMI[,13] <- ifelse(attend_se_[,3]==1, BMI[11],INTE_BMI[,13])
      #Year 13 effect for people who went to an SME course in year 1
      #Year 12 effect for people who went to an SME course in year 2
      INTE_BMI[,14] <- ifelse(attend_se_[,2]==1, BMI[13],0)
      INTE_BMI[,14] <- ifelse(attend_se_[,3]==1, BMI[12],INTE_BMI[,14])
      #Year 14 effect for people who went to an SME course in year 1
      #Year 13 effect for people who went to an SME course in year 2
      INTE_BMI[,15] <- ifelse(attend_se_[,2]==1, BMI[14],0)
      INTE_BMI[,15] <- ifelse(attend_se_[,3]==1, BMI[13],INTE_BMI[,15])
      #Year 15 effect for people who went to an SME course in year 1
      #Year 14 effect for people who went to an SME course in year 2
      INTE_BMI[,16] <- ifelse(attend_se_[,2]==1, BMI[15],0)
      INTE_BMI[,16] <- ifelse(attend_se_[,3]==1, BMI[14],INTE_BMI[,16])
      #Year 14 effect for people who went to an SME course in year 2
      INTE_BMI[,17] <- ifelse(attend_se_[,3]==1, BMI[15],INTE_BMI[,17])
    }else{
      #Create a matrix to store the effects
      BMI <- matrix(data=0, nrow = 1, ncol = 10)
      #Trajectory for people who se in year 1
      BMI[1:6]  <- parameter_[,"Intv_MA_BMI"]
      
      BMI[7] <-parameter_[,"Intv_MA_BMI"] - (1*parameter_[,"Intv_MA_BMI"]/4)
      
      BMI[8] <-parameter_[,"Intv_MA_BMI"] - (2*parameter_[,"Intv_MA_BMI"]/4)
      
      BMI[9] <-parameter_[,"Intv_MA_BMI"] - (3*parameter_[,"Intv_MA_BMI"]/4)
      
      #Year 1 effect for people who went to an SME course in year 1
      INTE_BMI[,2] <- ifelse(attend_se_[,2]==1, BMI[1],0)
      #Year 2 effect for people who went to an SME course in year 1
      #Year 1 effect for people who went to an SME course in year 2
      INTE_BMI[,3] <- ifelse(attend_se_[,2]==1, BMI[2],0)
      INTE_BMI[,3] <- ifelse(attend_se_[,3]==1, BMI[1],INTE_BMI[,3])
      #Year 3 effect for people who went to an SME course in year 1
      #Year 2 effect for people who went to an SME course in year 2
      INTE_BMI[,4] <- ifelse(attend_se_[,2]==1, BMI[3],0)
      INTE_BMI[,4] <- ifelse(attend_se_[,3]==1, BMI[2],INTE_BMI[,4])
      #Year 4 effect for people who went to an SME course in year 1
      #Year 3 effect for people who went to an SME course in year 2
      INTE_BMI[,5] <- ifelse(attend_se_[,2]==1, BMI[4],0)
      INTE_BMI[,5] <- ifelse(attend_se_[,3]==1, BMI[3],INTE_BMI[,5])
      #Year 5 effect for people who went to an SME course in year 1
      #Year 4 effect for people who went to an SME course in year 2
      INTE_BMI[,6] <- ifelse(attend_se_[,2]==1, BMI[5],0)
      INTE_BMI[,6] <- ifelse(attend_se_[,3]==1, BMI[4],INTE_BMI[,6])
      #Year 6 effect for people who went to an SME course in year 1
      #Year 5 effect for people who went to an SME course in year 2
      INTE_BMI[,7] <- ifelse(attend_se_[,2]==1, BMI[6],0)
      INTE_BMI[,7] <- ifelse(attend_se_[,3]==1, BMI[5],INTE_BMI[,7])
      #Year 7 effect for people who went to an SME course in year 1
      #Year 6 effect for people who went to an SME course in year 2
      INTE_BMI[,8] <- ifelse(attend_se_[,2]==1, BMI[7],0)
      INTE_BMI[,8] <- ifelse(attend_se_[,3]==1, BMI[6],INTE_BMI[,8])
      #Year 8 effect for people who went to an SME course in year 1
      #Year 7 effect for people who went to an SME course in year 2
      INTE_BMI[,9] <- ifelse(attend_se_[,2]==1, BMI[8],0)
      INTE_BMI[,9] <- ifelse(attend_se_[,3]==1, BMI[7],INTE_BMI[,9])
      #Year 9 effect for people who went to an SME course in year 1
      #Year 8 effect for people who went to an SME course in year 2
      INTE_BMI[,10] <- ifelse(attend_se_[,2]==1, BMI[9],0)
      INTE_BMI[,10] <- ifelse(attend_se_[,3]==1, BMI[8],INTE_BMI[,10])
      #Year 10 effect for people who went to an SME course in year 1
      #Year 9 effect for people who went to an SME course in year 2
      INTE_BMI[,11] <- ifelse(attend_se_[,2]==1, BMI[10],0)
      INTE_BMI[,11] <- ifelse(attend_se_[,3]==1, BMI[9],INTE_BMI[,11])
      #Year 10 effect for people who went to an SME course in year 2
      INTE_BMI[,12] <- ifelse(attend_se_[,3]==1, BMI[10],0)
    }
  }
  
  
  return(INTE_BMI)
}

##'@param n_ is the number of patients in the model
##'@param treatment_ is a text term indicating the current treatment option
##'@param parameter_ is the row of the parameter matrix
##'@param endtime_ is a number indicating how many years the simulation is being run for
##'@param GlobalVars_
##'@param attend_se_ is three column matrix. Column 1 is patient ID, column 2 is 
##'a 0,1 vector with 1 indicating attendance at an SE course in the first model year,
##'column 3 is a 0,1 vector with 1 indicating attendance at an SE course in the second
##'model year 
##'@return INTE_SBP is a matrix that gives the reduction in SBP(mmHg) for each patient 
##'in each year

initialise_intervention_dt_SBP <- function(n_,
                                           treatment_, 
                                           parameter_,
                                           endtime_,
                                           GlobalVars_, 
                                           attend_se_) {
  INTE_SBP <- matrix(data=0, nrow = n_, ncol =endtime_+2)
  INTE_SBP[,1] <- 1:n_ #make the first column equivalent to the patient ID for matching later on
  if(treatment_ == "test" | treatment_ == "Mt_HOOD_2018_QOL_SBP"){
    INTE_SBP[,2:(endtime_+2)] <- -10
  }else if (treatment_ == "Embedding_TrialEffect_All"){
    INTE_SBP[,2] <- parameter_[,"Intv_Embedding_SBP"]
    
    ####Duration scenarios
    if(as.numeric(GlobalVars_["Treatment effect duration","Value"])==3){#effects last until year 3
      INTE_SBP[,3:4]  <- parameter_[,"Intv_Embedding_SBP"]
    }else if (as.numeric(GlobalVars_["Treatment effect duration","Value"])==5){#effects last until year 5
      INTE_SBP[,3:6]  <- parameter_[,"Intv_Embedding_SBP"]
    }else{#effects last 10 years
      INTE_SBP[,3:11]  <- parameter_[,"Intv_Embedding_SBP"] 
    }
  }
  else if (treatment_ == "Embedding_TrialEffect_All_1yr"){
    INTE_SBP[,2] <- parameter_[,"Intv_Embedding_SBP"]
    
    ####Duration scenarios
    if(as.numeric(GlobalVars_["Treatment effect duration","Value"])==10){#effects last until year 10
      INTE_SBP[,3:11]  <- parameter_[,"Intv_Embedding_SBP"]
    } else if (as.numeric(GlobalVars_["Treatment effect duration","Value"])==5){#effects last unitl year 5
      INTE_SBP[,3:6]  <- parameter_[,"Intv_Embedding_SBP"]
    }else{#effects last 3 years
      INTE_SBP[,3:4]  <- parameter_[,"Intv_Embedding_SBP"] 
    }
    
  }else if (treatment_ == "Embedding_MetaAnalysis_All" | 
            treatment_ == "Control_MetaAnalysis_all" |
            treatment_ == "Embedding_MetaAnalysis_1yr"){
    
    ####Duration scenarios
    if(as.numeric(GlobalVars_["Treatment effect duration","Value"])==101){
      #Lifetime Effect
      INTE_SBP[,2:(endtime_+2)]  <- ifelse(attend_se_[,2]==1,
                                 parameter_[,"Intv_MA_SBP"],
                                 0) 
      INTE_SBP[,3:(endtime_+2)]  <- ifelse(attend_se_[,3]==1,
                                 parameter_[,"Intv_MA_SBP"],
                                 INTE_SBP[,2:(endtime_+2)]) 
      
    } else if (as.numeric(GlobalVars_["Treatment effect duration","Value"])==15){
      #10 years of full effect followed by 5 year waning
      #Create a matrix to store the effects
      SBP <- matrix(data=0, nrow = 1, ncol = 15)
      #Trajectory for people who se in year 1
      SBP[1:10]  <- parameter_[,"Intv_MA_SBP"]
      
      SBP[11] <- parameter_[,"Intv_MA_SBP"] - (1*parameter_[,"Intv_MA_SBP"]/5)
      
      SBP[12] <- parameter_[,"Intv_MA_SBP"] - (2*parameter_[,"Intv_MA_SBP"]/5)
      
      SBP[13] <- parameter_[,"Intv_MA_SBP"] - (3*parameter_[,"Intv_MA_SBP"]/5)
      
      SBP[14] <- parameter_[,"Intv_MA_SBP"] - (4*parameter_[,"Intv_MA_SBP"]/5)
      
      #Year 1 effect for people who went to an SME course in year 1
      INTE_SBP[,2] <- ifelse(attend_se_[,2]==1, SBP[1],0)
      #Year 2 effect for people who went to an SME course in year 1
      #Year 1 effect for people who went to an SME course in year 2
      INTE_SBP[,3] <- ifelse(attend_se_[,2]==1, SBP[2],0)
      INTE_SBP[,3] <- ifelse(attend_se_[,3]==1, SBP[1],INTE_SBP[,3])
      #Year 3 effect for people who went to an SME course in year 1
      #Year 2 effect for people who went to an SME course in year 2
      INTE_SBP[,4] <- ifelse(attend_se_[,2]==1, SBP[3],0)
      INTE_SBP[,4] <- ifelse(attend_se_[,3]==1, SBP[2],INTE_SBP[,4])
      #Year 4 effect for people who went to an SME course in year 1
      #Year 3 effect for people who went to an SME course in year 2
      INTE_SBP[,5] <- ifelse(attend_se_[,2]==1, SBP[4],0)
      INTE_SBP[,5] <- ifelse(attend_se_[,3]==1, SBP[3],INTE_SBP[,5])
      #Year 5 effect for people who went to an SME course in year 1
      #Year 4 effect for people who went to an SME course in year 2
      INTE_SBP[,6] <- ifelse(attend_se_[,2]==1, SBP[5],0)
      INTE_SBP[,6] <- ifelse(attend_se_[,3]==1, SBP[4],INTE_SBP[,6])
      #Year 6 effect for people who went to an SME course in year 1
      #Year 5 effect for people who went to an SME course in year 2
      INTE_SBP[,7] <- ifelse(attend_se_[,2]==1, SBP[6],0)
      INTE_SBP[,7] <- ifelse(attend_se_[,3]==1, SBP[5],INTE_SBP[,7])
      #Year 7 effect for people who went to an SME course in year 1
      #Year 6 effect for people who went to an SME course in year 2
      INTE_SBP[,8] <- ifelse(attend_se_[,2]==1, SBP[7],0)
      INTE_SBP[,8] <- ifelse(attend_se_[,3]==1, SBP[6],INTE_SBP[,8])
      #Year 8 effect for people who went to an SME course in year 1
      #Year 7 effect for people who went to an SME course in year 2
      INTE_SBP[,9] <- ifelse(attend_se_[,2]==1, SBP[8],0)
      INTE_SBP[,9] <- ifelse(attend_se_[,3]==1, SBP[7],INTE_SBP[,9])
      #Year 9 effect for people who went to an SME course in year 1
      #Year 8 effect for people who went to an SME course in year 2
      INTE_SBP[,10] <- ifelse(attend_se_[,2]==1, SBP[9],0)
      INTE_SBP[,10] <- ifelse(attend_se_[,3]==1, SBP[8],INTE_SBP[,10])
      #Year 10 effect for people who went to an SME course in year 1
      #Year 9 effect for people who went to an SME course in year 2
      INTE_SBP[,11] <- ifelse(attend_se_[,2]==1, SBP[10],0)
      INTE_SBP[,11] <- ifelse(attend_se_[,3]==1, SBP[9],INTE_SBP[,11])
      #Year 11 effect for people who went to an SME course in year 1
      #Year 10 effect for people who went to an SME course in year 2
      INTE_SBP[,12] <- ifelse(attend_se_[,2]==1, SBP[11],0)
      INTE_SBP[,12] <- ifelse(attend_se_[,3]==1, SBP[10],INTE_SBP[,12])
      #Year 12 effect for people who went to an SME course in year 1
      #Year 11 effect for people who went to an SME course in year 2
      INTE_SBP[,13] <- ifelse(attend_se_[,2]==1, SBP[12],0)
      INTE_SBP[,13] <- ifelse(attend_se_[,3]==1, SBP[11],INTE_SBP[,13])
      #Year 13 effect for people who went to an SME course in year 1
      #Year 12 effect for people who went to an SME course in year 2
      INTE_SBP[,14] <- ifelse(attend_se_[,2]==1, SBP[13],0)
      INTE_SBP[,14] <- ifelse(attend_se_[,3]==1, SBP[12],INTE_SBP[,14])
      #Year 14 effect for people who went to an SME course in year 1
      #Year 13 effect for people who went to an SME course in year 2
      INTE_SBP[,15] <- ifelse(attend_se_[,2]==1, SBP[14],0)
      INTE_SBP[,15] <- ifelse(attend_se_[,3]==1, SBP[13],INTE_SBP[,15])
      #Year 15 effect for people who went to an SME course in year 1
      #Year 14 effect for people who went to an SME course in year 2
      INTE_SBP[,16] <- ifelse(attend_se_[,2]==1, SBP[15],0)
      INTE_SBP[,16] <- ifelse(attend_se_[,3]==1, SBP[14],INTE_SBP[,16])
      #Year 14 effect for people who went to an SME course in year 2
      INTE_SBP[,17] <- ifelse(attend_se_[,3]==1, SBP[15],0)
    }else{
      #Create a matrix to store the effects
      SBP <- matrix(data=0, nrow = 1, ncol = 10)
      #Trajectory for people who se in year 1
      SBP[1:6]  <- parameter_[,"Intv_MA_SBP"]
      
      SBP[7] <-parameter_[,"Intv_MA_SBP"] - (1*parameter_[,"Intv_MA_SBP"]/4)
      
      SBP[8] <-parameter_[,"Intv_MA_SBP"] - (2*parameter_[,"Intv_MA_SBP"]/4)
      
      SBP[9] <-parameter_[,"Intv_MA_SBP"] - (3*parameter_[,"Intv_MA_SBP"]/4)
      
      #Year 1 effect for people who went to an SME course in year 1
      INTE_SBP[,2] <- ifelse(attend_se_[,2]==1, SBP[1],0)
      #Year 2 effect for people who went to an SME course in year 1
      #Year 1 effect for people who went to an SME course in year 2
      INTE_SBP[,3] <- ifelse(attend_se_[,2]==1, SBP[2],0)
      INTE_SBP[,3] <- ifelse(attend_se_[,3]==1, SBP[1],INTE_SBP[,3])
      #Year 3 effect for people who went to an SME course in year 1
      #Year 2 effect for people who went to an SME course in year 2
      INTE_SBP[,4] <- ifelse(attend_se_[,2]==1, SBP[3],0)
      INTE_SBP[,4] <- ifelse(attend_se_[,3]==1, SBP[2],INTE_SBP[,4])
      #Year 4 effect for people who went to an SME course in year 1
      #Year 3 effect for people who went to an SME course in year 2
      INTE_SBP[,5] <- ifelse(attend_se_[,2]==1, SBP[4],0)
      INTE_SBP[,5] <- ifelse(attend_se_[,3]==1, SBP[3],INTE_SBP[,5])
      #Year 5 effect for people who went to an SME course in year 1
      #Year 4 effect for people who went to an SME course in year 2
      INTE_SBP[,6] <- ifelse(attend_se_[,2]==1, SBP[5],0)
      INTE_SBP[,6] <- ifelse(attend_se_[,3]==1, SBP[4],INTE_SBP[,6])
      #Year 6 effect for people who went to an SME course in year 1
      #Year 5 effect for people who went to an SME course in year 2
      INTE_SBP[,7] <- ifelse(attend_se_[,2]==1, SBP[6],0)
      INTE_SBP[,7] <- ifelse(attend_se_[,3]==1, SBP[5],INTE_SBP[,7])
      #Year 7 effect for people who went to an SME course in year 1
      #Year 6 effect for people who went to an SME course in year 2
      INTE_SBP[,8] <- ifelse(attend_se_[,2]==1, SBP[7],0)
      INTE_SBP[,8] <- ifelse(attend_se_[,3]==1, SBP[6],INTE_SBP[,8])
      #Year 8 effect for people who went to an SME course in year 1
      #Year 7 effect for people who went to an SME course in year 2
      INTE_SBP[,9] <- ifelse(attend_se_[,2]==1, SBP[8],0)
      INTE_SBP[,9] <- ifelse(attend_se_[,3]==1, SBP[7],INTE_SBP[,9])
      #Year 9 effect for people who went to an SME course in year 1
      #Year 8 effect for people who went to an SME course in year 2
      INTE_SBP[,10] <- ifelse(attend_se_[,2]==1, SBP[9],0)
      INTE_SBP[,10] <- ifelse(attend_se_[,3]==1, SBP[8],INTE_SBP[,10])
      #Year 10 effect for people who went to an SME course in year 1
      #Year 9 effect for people who went to an SME course in year 2
      INTE_SBP[,11] <- ifelse(attend_se_[,2]==1, SBP[10],0)
      INTE_SBP[,11] <- ifelse(attend_se_[,3]==1, SBP[9],INTE_SBP[,11])
      #Year 10 effect for people who went to an SME course in year 2
      INTE_SBP[,12] <- ifelse(attend_se_[,3]==1, SBP[10],0)
    }
  }else{
    #If no option is specified, set treatment effects to 0
    INTE_SBP[,2:(endtime_+2)] <-0 
  }
  
  return(INTE_SBP)
}

##'@param n_ is the number of patients in the model
##'@param treatment_ is a text term indicating the current treatment option
##'@param parameter_ is the row of the parameter matrix
##'@param endtime_ is a number indicating how many years the simulation is being run for
##'@param GlobalVars_
##'@param attend_se_ is three column matrix. Column 1 is patient ID, column 2 is 
##'a 0,1 vector with 1 indicating attendance at an SE course in the first model year,
##'column 3 is a 0,1 vector with 1 indicating attendance at an SE course in the second
##'model year 
##'@return INTE_HDL is a matrix that gives the change in high density lipoprotein cholesterol 
##'for each patient in each year
initialise_intervention_dt_HDL <- function(n_,
                                           treatment_, 
                                           parameter_,
                                           endtime_,
                                           GlobalVars_, 
                                           attend_se_) {
  INTE_HDL <- matrix(data=0, nrow = n_, ncol =(endtime_+2))
  INTE_HDL[,1] <- 1:n_ #make the first column equivalent to the patient ID for matching later on
  
  if(treatment_ == "Mt_HOOD_2018_QOL_ALL" | treatment_ == "Mt_HOOD_2018_QOL_HDL"){
    INTE_HDL[,2:(endtime_+2)] <- -0.5
  }else if (treatment_ == "Embedding_TrialEffect_All"){
    INTE_HDL[,2] <- parameter_[,"Intv_Embedding_HDL"]
    
    ####Duration scenarios
    if(as.numeric(GlobalVars_["Treatment effect duration","Value"])==3){#effects last until year 3
      INTE_HDL[,3:4]  <- parameter_[,"Intv_Embedding_HDL"]
    }else if (as.numeric(GlobalVars_["Treatment effect duration","Value"])==5){#effects last until year 5
      INTE_HDL[,3:6]  <- parameter_[,"Intv_Embedding_HDL"]
    }else{#effects last 10 years
      INTE_HDL[,3:11]  <- parameter_[,"Intv_Embedding_HDL"] 
    }
  }
  else if (treatment_ == "Embedding_TrialEffect_All_1yr"){
    INTE_HDL[,2] <- parameter_[,"Intv_Embedding_HDL"]
    
    ####Duration scenarios
    if(as.numeric(GlobalVars_["Treatment effect duration","Value"])==10){#effects last until year 10
      INTE_HDL[,3:11]  <- parameter_[,"Intv_Embedding_HDL"]
    } else if (as.numeric(GlobalVars_["Treatment effect duration","Value"])==5){#effects last unitl year 5
      INTE_HDL[,3:6]  <- parameter_[,"Intv_Embedding_HDL"]
    }else{#effects last 3 years
      INTE_HDL[,3:4]  <- parameter_[,"Intv_Embedding_HDL"] 
    }
    
  }else if (treatment_ == "Embedding_MetaAnalysis_All" | 
            treatment_ == "Control_MetaAnalysis_all" |
            treatment_ == "Embedding_MetaAnalysis_1yr"){
    
    ####Duration scenarios
    if(as.numeric(GlobalVars_["Treatment effect duration","Value"])==101){
      #Lifetime Effect
      INTE_HDL[,2:(endtime_+2)]  <- ifelse(attend_se_[,2]==1,
                                 parameter_[,"Intv_MA_HDL"],
                                 0) 
      INTE_HDL[,3:(endtime_+2)]  <- ifelse(attend_se_[,3]==1,
                                 parameter_[,"Intv_MA_HDL"],
                                 INTE_HDL[,3:(endtime_+2)]) 
      
    } else if (as.numeric(GlobalVars_["Treatment effect duration","Value"])==15){
      #10 years of full effect followed by 5 year waning
      #Create a matrix to store the effects
      HDL <- matrix(data=0, nrow = 1, ncol = 15)
      #Trajectory for people who se in year 1
      HDL[1:10]  <- parameter_[,"Intv_MA_HDL"]
      
      HDL[11] <- parameter_[,"Intv_MA_HDL"] - (1*parameter_[,"Intv_MA_HDL"]/5)
      
      HDL[12] <- parameter_[,"Intv_MA_HDL"] - (2*parameter_[,"Intv_MA_HDL"]/5)
      
      HDL[13] <- parameter_[,"Intv_MA_HDL"] - (3*parameter_[,"Intv_MA_HDL"]/5)
      
      HDL[14] <- parameter_[,"Intv_MA_HDL"] - (4*parameter_[,"Intv_MA_HDL"]/5)
      
      #Year 1 effect for people who went to an SME course in year 1
      INTE_HDL[,2] <- ifelse(attend_se_[,2]==1, HDL[1],0)
      #Year 2 effect for people who went to an SME course in year 1
      #Year 1 effect for people who went to an SME course in year 2
      INTE_HDL[,3] <- ifelse(attend_se_[,2]==1, HDL[2],0)
      INTE_HDL[,3] <- ifelse(attend_se_[,3]==1, HDL[1],INTE_HDL[,3])
      #Year 3 effect for people who went to an SME course in year 1
      #Year 2 effect for people who went to an SME course in year 2
      INTE_HDL[,4] <- ifelse(attend_se_[,2]==1, HDL[3],0)
      INTE_HDL[,4] <- ifelse(attend_se_[,3]==1, HDL[2],INTE_HDL[,4])
      #Year 4 effect for people who went to an SME course in year 1
      #Year 3 effect for people who went to an SME course in year 2
      INTE_HDL[,5] <- ifelse(attend_se_[,2]==1, HDL[4],0)
      INTE_HDL[,5] <- ifelse(attend_se_[,3]==1, HDL[3],INTE_HDL[,5])
      #Year 5 effect for people who went to an SME course in year 1
      #Year 4 effect for people who went to an SME course in year 2
      INTE_HDL[,6] <- ifelse(attend_se_[,2]==1, HDL[5],0)
      INTE_HDL[,6] <- ifelse(attend_se_[,3]==1, HDL[4],INTE_HDL[,6])
      #Year 6 effect for people who went to an SME course in year 1
      #Year 5 effect for people who went to an SME course in year 2
      INTE_HDL[,7] <- ifelse(attend_se_[,2]==1, HDL[6],0)
      INTE_HDL[,7] <- ifelse(attend_se_[,3]==1, HDL[5],INTE_HDL[,7])
      #Year 7 effect for people who went to an SME course in year 1
      #Year 6 effect for people who went to an SME course in year 2
      INTE_HDL[,8] <- ifelse(attend_se_[,2]==1, HDL[7],0)
      INTE_HDL[,8] <- ifelse(attend_se_[,3]==1, HDL[6],INTE_HDL[,8])
      #Year 8 effect for people who went to an SME course in year 1
      #Year 7 effect for people who went to an SME course in year 2
      INTE_HDL[,9] <- ifelse(attend_se_[,2]==1, HDL[8],0)
      INTE_HDL[,9] <- ifelse(attend_se_[,3]==1, HDL[7],INTE_HDL[,9])
      #Year 9 effect for people who went to an SME course in year 1
      #Year 8 effect for people who went to an SME course in year 2
      INTE_HDL[,10] <- ifelse(attend_se_[,2]==1, HDL[9],0)
      INTE_HDL[,10] <- ifelse(attend_se_[,3]==1, HDL[8],INTE_HDL[,10])
      #Year 10 effect for people who went to an SME course in year 1
      #Year 9 effect for people who went to an SME course in year 2
      INTE_HDL[,11] <- ifelse(attend_se_[,2]==1, HDL[10],0)
      INTE_HDL[,11] <- ifelse(attend_se_[,3]==1, HDL[9],INTE_HDL[,11])
      #Year 11 effect for people who went to an SME course in year 1
      #Year 10 effect for people who went to an SME course in year 2
      INTE_HDL[,12] <- ifelse(attend_se_[,2]==1, HDL[11],0)
      INTE_HDL[,12] <- ifelse(attend_se_[,3]==1, HDL[10],INTE_HDL[,12])
      #Year 12 effect for people who went to an SME course in year 1
      #Year 11 effect for people who went to an SME course in year 2
      INTE_HDL[,13] <- ifelse(attend_se_[,2]==1, HDL[12],0)
      INTE_HDL[,13] <- ifelse(attend_se_[,3]==1, HDL[11],INTE_HDL[,13])
      #Year 13 effect for people who went to an SME course in year 1
      #Year 12 effect for people who went to an SME course in year 2
      INTE_HDL[,14] <- ifelse(attend_se_[,2]==1, HDL[13],0)
      INTE_HDL[,14] <- ifelse(attend_se_[,3]==1, HDL[12],INTE_HDL[,14])
      #Year 14 effect for people who went to an SME course in year 1
      #Year 13 effect for people who went to an SME course in year 2
      INTE_HDL[,15] <- ifelse(attend_se_[,2]==1, HDL[14],0)
      INTE_HDL[,15] <- ifelse(attend_se_[,3]==1, HDL[13],INTE_HDL[,15])
      #Year 15 effect for people who went to an SME course in year 1
      #Year 14 effect for people who went to an SME course in year 2
      INTE_HDL[,16] <- ifelse(attend_se_[,2]==1, HDL[15],0)
      INTE_HDL[,16] <- ifelse(attend_se_[,3]==1, HDL[14],INTE_HDL[,16])
      #Year 14 effect for people who went to an SME course in year 2
      INTE_HDL[,17] <- ifelse(attend_se_[,3]==1, HDL[15],0)
    }else{
      #Create a matrix to store the effects
      HDL <- matrix(data=0, nrow = 1, ncol = 10)
      #Trajectory for people who se in year 1
      HDL[1:6]  <- parameter_[,"Intv_MA_HDL"]
      
      HDL[7] <-parameter_[,"Intv_MA_HDL"] - (1*parameter_[,"Intv_MA_HDL"]/4)
      
      HDL[8] <-parameter_[,"Intv_MA_HDL"] - (2*parameter_[,"Intv_MA_HDL"]/4)
      
      HDL[9] <-parameter_[,"Intv_MA_HDL"] - (3*parameter_[,"Intv_MA_HDL"]/4)
      
      #Year 1 effect for people who went to an SME course in year 1
      INTE_HDL[,2] <- ifelse(attend_se_[,2]==1, HDL[1],0)
      #Year 2 effect for people who went to an SME course in year 1
      #Year 1 effect for people who went to an SME course in year 2
      INTE_HDL[,3] <- ifelse(attend_se_[,2]==1, HDL[2],0)
      INTE_HDL[,3] <- ifelse(attend_se_[,3]==1, HDL[1],INTE_HDL[,3])
      #Year 3 effect for people who went to an SME course in year 1
      #Year 2 effect for people who went to an SME course in year 2
      INTE_HDL[,4] <- ifelse(attend_se_[,2]==1, HDL[3],0)
      INTE_HDL[,4] <- ifelse(attend_se_[,3]==1, HDL[2],INTE_HDL[,4])
      #Year 4 effect for people who went to an SME course in year 1
      #Year 3 effect for people who went to an SME course in year 2
      INTE_HDL[,5] <- ifelse(attend_se_[,2]==1, HDL[4],0)
      INTE_HDL[,5] <- ifelse(attend_se_[,3]==1, HDL[3],INTE_HDL[,5])
      #Year 5 effect for people who went to an SME course in year 1
      #Year 4 effect for people who went to an SME course in year 2
      INTE_HDL[,6] <- ifelse(attend_se_[,2]==1, HDL[5],0)
      INTE_HDL[,6] <- ifelse(attend_se_[,3]==1, HDL[4],INTE_HDL[,6])
      #Year 6 effect for people who went to an SME course in year 1
      #Year 5 effect for people who went to an SME course in year 2
      INTE_HDL[,7] <- ifelse(attend_se_[,2]==1, HDL[6],0)
      INTE_HDL[,7] <- ifelse(attend_se_[,3]==1, HDL[5],INTE_HDL[,7])
      #Year 7 effect for people who went to an SME course in year 1
      #Year 6 effect for people who went to an SME course in year 2
      INTE_HDL[,8] <- ifelse(attend_se_[,2]==1, HDL[7],0)
      INTE_HDL[,8] <- ifelse(attend_se_[,3]==1, HDL[6],INTE_HDL[,8])
      #Year 8 effect for people who went to an SME course in year 1
      #Year 7 effect for people who went to an SME course in year 2
      INTE_HDL[,9] <- ifelse(attend_se_[,2]==1, HDL[8],0)
      INTE_HDL[,9] <- ifelse(attend_se_[,3]==1, HDL[7],INTE_HDL[,9])
      #Year 9 effect for people who went to an SME course in year 1
      #Year 8 effect for people who went to an SME course in year 2
      INTE_HDL[,10] <- ifelse(attend_se_[,2]==1, HDL[9],0)
      INTE_HDL[,10] <- ifelse(attend_se_[,3]==1, HDL[8],INTE_HDL[,10])
      #Year 10 effect for people who went to an SME course in year 1
      #Year 9 effect for people who went to an SME course in year 2
      INTE_HDL[,11] <- ifelse(attend_se_[,2]==1, HDL[10],0)
      INTE_HDL[,11] <- ifelse(attend_se_[,3]==1, HDL[9],INTE_HDL[,11])
      #Year 10 effect for people who went to an SME course in year 2
      INTE_HDL[,12] <- ifelse(attend_se_[,3]==1, HDL[10],0)
    }
  }
  
  return(INTE_HDL)
}

##'@param n_ is the number of patients in the model
##'@param treatment_ is a text term indicating the current treatment option
##'@param parameter_ is the row of the parameter matrix
##'@param endtime_ is a number indicating how many years the simulation is being run for
##'@return INTE_LDL is a matrix that gives the reduction in HDL cholesterol 
##'for each patient in each year

initialise_intervention_dt_LDL <- function(n_,
                                           treatment_, 
                                           parameter_,
                                           endtime_,
                                           GlobalVars_, 
                                           attend_se_) {
  INTE_LDL <- matrix(data=0, nrow = n_, ncol = (endtime_+2))
  INTE_LDL[,1] <- 1:n_ #make the first column equivalent to the patient ID for matching later on
  
  if(treatment_ == "Mt_HOOD_2018_QOL_ALL" | treatment_ == "Mt_HOOD_2018_QOL_LDL"){
    INTE_LDL[,2:(endtime_+2)] <- -0.5
  }else if (treatment_ == "Embedding_TrialEffect_All"){
    INTE_LDL[,2] <- parameter_[,"Intv_Embedding_LDL"]
    
    ####Duration scenarios
    if(as.numeric(GlobalVars_["Treatment effect duration","Value"] )==3){#effects last until year 3
      INTE_LDL[,3:4]  <- parameter_[,"Intv_Embedding_LDL"]
    }else if (as.numeric(GlobalVars_["Treatment effect duration","Value"] )==5){#effects last until year 5
      INTE_LDL[,3:6]  <- parameter_[,"Intv_Embedding_LDL"]
    }else{#effects last 10 years
      INTE_LDL[,3:11]  <- parameter_[,"Intv_Embedding_LDL"] 
    }
  }
  else if (treatment_ == "Embedding_TrialEffect_All_1yr"){
    INTE_LDL[,2] <- parameter_[,"Intv_Embedding_LDL"]
    
    ####Duration scenarios
    if(as.numeric(GlobalVars_["Treatment effect duration","Value"] )==10){#effects last until year 10
      INTE_LDL[,3:11]  <- parameter_[,"Intv_Embedding_LDL"]
    } else if (as.numeric(GlobalVars_["Treatment effect duration","Value"] )==5){#effects last until year 5
      INTE_LDL[,3:6]  <- parameter_[,"Intv_Embedding_LDL"]
    }else{#effects last 3 years
      INTE_LDL[,3:4]  <- parameter_[,"Intv_Embedding_LDL"] 
    }
    
  }else if (treatment_ == "Embedding_MetaAnalysis_All" | 
            treatment_ == "Control_MetaAnalysis_all" |
            treatment_ == "Embedding_MetaAnalysis_1yr"){
    
    ####Duration scenarios
    if(as.numeric(GlobalVars_["Treatment effect duration","Value"] )==101){
      #Lifetime Effect
      INTE_LDL[,2:(endtime_+2)]  <- ifelse(attend_se_[,2]==1,
                                 parameter_[,"Intv_MA_LDL"],
                                 0) 
      INTE_LDL[,3:(endtime_+2)]  <- ifelse(attend_se_[,3]==1,
                                 parameter_[,"Intv_MA_LDL"],
                                 INTE_LDL[,3:(endtime_+2)]) 
      
    } else if (as.numeric(GlobalVars_["Treatment effect duration","Value"] )==15){
      #10 years of full effect followed by 5 year waning
      #Create a matrix to store the effects
      LDL <- matrix(data=0, nrow = 1, ncol = 15)
      #Trajectory for people who se in year 1
      LDL[1:10]  <- parameter_[,"Intv_MA_LDL"]
      
      LDL[11] <- parameter_[,"Intv_MA_LDL"] - (1*parameter_[,"Intv_MA_LDL"]/5)
      
      LDL[12] <- parameter_[,"Intv_MA_LDL"] - (2*parameter_[,"Intv_MA_LDL"]/5)
      
      LDL[13] <- parameter_[,"Intv_MA_LDL"] - (3*parameter_[,"Intv_MA_LDL"]/5)
      
      LDL[14] <- parameter_[,"Intv_MA_LDL"] - (4*parameter_[,"Intv_MA_LDL"]/5)
      
      #Year 1 effect for people who went to an SME course in year 1
      INTE_LDL[,2] <- ifelse(attend_se_[,2]==1, LDL[1],0)
      #Year 2 effect for people who went to an SME course in year 1
      #Year 1 effect for people who went to an SME course in year 2
      INTE_LDL[,3] <- ifelse(attend_se_[,2]==1, LDL[2],0)
      INTE_LDL[,3] <- ifelse(attend_se_[,3]==1, LDL[1],INTE_LDL[,3])
      #Year 3 effect for people who went to an SME course in year 1
      #Year 2 effect for people who went to an SME course in year 2
      INTE_LDL[,4] <- ifelse(attend_se_[,2]==1, LDL[3],0)
      INTE_LDL[,4] <- ifelse(attend_se_[,3]==1, LDL[2],INTE_LDL[,4])
      #Year 4 effect for people who went to an SME course in year 1
      #Year 3 effect for people who went to an SME course in year 2
      INTE_LDL[,5] <- ifelse(attend_se_[,2]==1, LDL[4],0)
      INTE_LDL[,5] <- ifelse(attend_se_[,3]==1, LDL[3],INTE_LDL[,5])
      #Year 5 effect for people who went to an SME course in year 1
      #Year 4 effect for people who went to an SME course in year 2
      INTE_LDL[,6] <- ifelse(attend_se_[,2]==1, LDL[5],0)
      INTE_LDL[,6] <- ifelse(attend_se_[,3]==1, LDL[4],INTE_LDL[,6])
      #Year 6 effect for people who went to an SME course in year 1
      #Year 5 effect for people who went to an SME course in year 2
      INTE_LDL[,7] <- ifelse(attend_se_[,2]==1, LDL[6],0)
      INTE_LDL[,7] <- ifelse(attend_se_[,3]==1, LDL[5],INTE_LDL[,7])
      #Year 7 effect for people who went to an SME course in year 1
      #Year 6 effect for people who went to an SME course in year 2
      INTE_LDL[,8] <- ifelse(attend_se_[,2]==1, LDL[7],0)
      INTE_LDL[,8] <- ifelse(attend_se_[,3]==1, LDL[6],INTE_LDL[,8])
      #Year 8 effect for people who went to an SME course in year 1
      #Year 7 effect for people who went to an SME course in year 2
      INTE_LDL[,9] <- ifelse(attend_se_[,2]==1, LDL[8],0)
      INTE_LDL[,9] <- ifelse(attend_se_[,3]==1, LDL[7],INTE_LDL[,9])
      #Year 9 effect for people who went to an SME course in year 1
      #Year 8 effect for people who went to an SME course in year 2
      INTE_LDL[,10] <- ifelse(attend_se_[,2]==1, LDL[9],0)
      INTE_LDL[,10] <- ifelse(attend_se_[,3]==1, LDL[8],INTE_LDL[,10])
      #Year 10 effect for people who went to an SME course in year 1
      #Year 9 effect for people who went to an SME course in year 2
      INTE_LDL[,11] <- ifelse(attend_se_[,2]==1, LDL[10],0)
      INTE_LDL[,11] <- ifelse(attend_se_[,3]==1, LDL[9],INTE_LDL[,11])
      #Year 11 effect for people who went to an SME course in year 1
      #Year 10 effect for people who went to an SME course in year 2
      INTE_LDL[,12] <- ifelse(attend_se_[,2]==1, LDL[11],0)
      INTE_LDL[,12] <- ifelse(attend_se_[,3]==1, LDL[10],INTE_LDL[,12])
      #Year 12 effect for people who went to an SME course in year 1
      #Year 11 effect for people who went to an SME course in year 2
      INTE_LDL[,13] <- ifelse(attend_se_[,2]==1, LDL[12],0)
      INTE_LDL[,13] <- ifelse(attend_se_[,3]==1, LDL[11],INTE_LDL[,13])
      #Year 13 effect for people who went to an SME course in year 1
      #Year 12 effect for people who went to an SME course in year 2
      INTE_LDL[,14] <- ifelse(attend_se_[,2]==1, LDL[13],0)
      INTE_LDL[,14] <- ifelse(attend_se_[,3]==1, LDL[12],INTE_LDL[,14])
      #Year 14 effect for people who went to an SME course in year 1
      #Year 13 effect for people who went to an SME course in year 2
      INTE_LDL[,15] <- ifelse(attend_se_[,2]==1, LDL[14],0)
      INTE_LDL[,15] <- ifelse(attend_se_[,3]==1, LDL[13],INTE_LDL[,15])
      #Year 15 effect for people who went to an SME course in year 1
      #Year 14 effect for people who went to an SME course in year 2
      INTE_LDL[,16] <- ifelse(attend_se_[,2]==1, LDL[15],0)
      INTE_LDL[,16] <- ifelse(attend_se_[,3]==1, LDL[14],INTE_LDL[,16])
      #Year 14 effect for people who went to an SME course in year 2
      INTE_LDL[,17] <- ifelse(attend_se_[,3]==1, LDL[15],0)
    }else{
      #Create a matrix to store the effects
      LDL <- matrix(data=0, nrow = 1, ncol = 10)
      #Trajectory for people who se in year 1
      LDL[1:6]  <- parameter_[,"Intv_MA_LDL"]
      
      LDL[7] <-parameter_[,"Intv_MA_LDL"] - (1*parameter_[,"Intv_MA_LDL"]/4)
      
      LDL[8] <-parameter_[,"Intv_MA_LDL"] - (2*parameter_[,"Intv_MA_LDL"]/4)
      
      LDL[9] <-parameter_[,"Intv_MA_LDL"] - (3*parameter_[,"Intv_MA_LDL"]/4)
      
      #Year 1 effect for people who went to an SME course in year 1
      INTE_LDL[,2] <- ifelse(attend_se_[,2]==1, LDL[1],0)
      #Year 2 effect for people who went to an SME course in year 1
      #Year 1 effect for people who went to an SME course in year 2
      INTE_LDL[,3] <- ifelse(attend_se_[,2]==1, LDL[2],0)
      INTE_LDL[,3] <- ifelse(attend_se_[,3]==1, LDL[1],INTE_LDL[,3])
      #Year 3 effect for people who went to an SME course in year 1
      #Year 2 effect for people who went to an SME course in year 2
      INTE_LDL[,4] <- ifelse(attend_se_[,2]==1, LDL[3],0)
      INTE_LDL[,4] <- ifelse(attend_se_[,3]==1, LDL[2],INTE_LDL[,4])
      #Year 4 effect for people who went to an SME course in year 1
      #Year 3 effect for people who went to an SME course in year 2
      INTE_LDL[,5] <- ifelse(attend_se_[,2]==1, LDL[4],0)
      INTE_LDL[,5] <- ifelse(attend_se_[,3]==1, LDL[3],INTE_LDL[,5])
      #Year 5 effect for people who went to an SME course in year 1
      #Year 4 effect for people who went to an SME course in year 2
      INTE_LDL[,6] <- ifelse(attend_se_[,2]==1, LDL[5],0)
      INTE_LDL[,6] <- ifelse(attend_se_[,3]==1, LDL[4],INTE_LDL[,6])
      #Year 6 effect for people who went to an SME course in year 1
      #Year 5 effect for people who went to an SME course in year 2
      INTE_LDL[,7] <- ifelse(attend_se_[,2]==1, LDL[6],0)
      INTE_LDL[,7] <- ifelse(attend_se_[,3]==1, LDL[5],INTE_LDL[,7])
      #Year 7 effect for people who went to an SME course in year 1
      #Year 6 effect for people who went to an SME course in year 2
      INTE_LDL[,8] <- ifelse(attend_se_[,2]==1, LDL[7],0)
      INTE_LDL[,8] <- ifelse(attend_se_[,3]==1, LDL[6],INTE_LDL[,8])
      #Year 8 effect for people who went to an SME course in year 1
      #Year 7 effect for people who went to an SME course in year 2
      INTE_LDL[,9] <- ifelse(attend_se_[,2]==1, LDL[8],0)
      INTE_LDL[,9] <- ifelse(attend_se_[,3]==1, LDL[7],INTE_LDL[,9])
      #Year 9 effect for people who went to an SME course in year 1
      #Year 8 effect for people who went to an SME course in year 2
      INTE_LDL[,10] <- ifelse(attend_se_[,2]==1, LDL[9],0)
      INTE_LDL[,10] <- ifelse(attend_se_[,3]==1, LDL[8],INTE_LDL[,10])
      #Year 10 effect for people who went to an SME course in year 1
      #Year 9 effect for people who went to an SME course in year 2
      INTE_LDL[,11] <- ifelse(attend_se_[,2]==1, LDL[10],0)
      INTE_LDL[,11] <- ifelse(attend_se_[,3]==1, LDL[9],INTE_LDL[,11])
      #Year 10 effect for people who went to an SME course in year 2
      INTE_LDL[,12] <- ifelse(attend_se_[,3]==1, LDL[10],0)
    }
  }
  return(INTE_LDL)
}

##'@param Input_Prob_ is a numeric number between 0 and 1
##'@param HR_ is a hazard ratio 
##'@return output_prob is the new probability after applying the hazard ratio

Hazard_Ratio_Intervention <- function(Input_Prob_,
                                      HR_){

  input_rate <- -log(1-Input_Prob_)/1
  output_rate <- input_rate*HR_
  output_prob <- 1-exp(-output_rate*1)
  
  return(output_prob)
  
}

##'@param n_ is the number of patients in the model
##'@param treatment_ is a text term indicating the current treatment option
##'@param parameter_ is the row of the parameter matrix
##'@return attend_se is a vector of probabilities 

initialise_intervention_dt_attendse <- function(n_, treatment_, parameter_){
  attend_se <- matrix(data=0, nrow = n_, ncol = 3)
  attend_se[,1] <- seq(from = 1, to = n_, by = 1)
  if(treatment_ == "Control_MetaAnalysis_all"| treatment_ == "Baseline") {
    #probability of attending SE
    attend_se_1 <- ifelse(runif(n_) < parameter_[, "Cont_Embedding_prob_attendSE"],1,0)
    attend_se[,2] <- attend_se_1
    #only one period for control, so no year 2 start of SE
  }else if (treatment_ == "Embedding_MetaAnalysis_All"|
            treatment_ == "Embedding_TrialEffect_All"){
    #probability of attending SE
    temp <- parameter_[, "Cont_Embedding_prob_attendSE"]
    temp <- temp / (1-temp)
    temp <- temp*parameter_[, "Intv_Embedding_OR_attendSE"]
    p_attend_se_y1 <- temp/(1+temp)
    
    attend_se_1 <- ifelse(runif(n_) < p_attend_se_y1,1,0)
    attend_se[,2] <- attend_se_1
    
    #probability of attending SE
    temp <- parameter_[, "Cont_Embedding_prob_attendSE"]
    temp <- temp / (1-temp)
    temp <- temp*parameter_[, "Intv_Embedding_OR_attendSE_2yr"]
    p_attend_se_y2 <- temp/(1+temp)
    
    #normalise based on the probability of attending SE in year 1
    p_attend_se_y2_nosey1 <- ((p_attend_se_y2 - p_attend_se_y1) / (1-p_attend_se_y1))
    
    attend_se_2 <- ifelse(runif(n_) < p_attend_se_y2_nosey1,1,0)
    attend_se[,3] <- attend_se_2
    
    #Overwrite the year 2 values with no attendance if they attend SE in year 1
    attend_se[,3] <- ifelse(attend_se[,2]==1,0, attend_se[,3])
    
  }else if (treatment_ == "Embedding_MetaAnalysis_1yr"|
            treatment_ == "Embedding_TrialEffect_1yr"){
    #probability of attending SE
    temp <- parameter_[, "Cont_Embedding_prob_attendSE"]
    temp <- temp / (1-temp)
    temp <- temp*parameter_[, "Intv_Embedding_OR_attendSE"]
    p_attend_se_y1 <- temp/(1+temp)
    
    attend_se_1 <- ifelse(runif(n_) < p_attend_se_y1,1,0)
    attend_se[,2] <- attend_se_1
    
  }
  return(attend_se)
}
