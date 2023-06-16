library(MASS)
library(VGAM)
library(doParallel)
library(parallel)
set.seed(429)

source("all_model_files.R")

source("Global Options.R")

####CHF ########################################################################
population_CHF <- read.csv("Unit tests/UKPDS82standard person - CHF.csv")
population_CHF_clean <- build_population(population_CHF, PopulationVariables)

CHF <- First_CHF_UKPDS_82(population_CHF_clean, parameter[1,], parameter[1,"Diab_1stCHF_Rho"], "Baseline", population_CHF_clean[,"ID"]>0)
if(round(CHF[1],3) != 0.027){
  stop("Error in CHF probabilities")
}

##Death, logistic###############################################################
population_death_log <-read.csv("Unit tests/UKPDS82standard person - DeathEventNoHist.csv")
population_death_log_clean <- build_population(population_death_log, PopulationVariables)

DeathEventNoHist <- Death_Event_NoHist_UKPDS_82(population_death_log_clean,
                                                parameter[1,],
                                                "bl",
                                                population_death_log_clean[,"ID"]>0)

if(round(DeathEventNoHist[1],3) !=0.597){
  stop("Error in probabilities of death")
}

#Give the same person heart failure & not MI
population_death_log_clean[,"CHF_E"] <- 1
population_death_log_clean[,"MI_E"] <- 0

DeathEventNoHist2 <- Death_Event_NoHist_UKPDS_82(population_death_log_clean,
                                                parameter[1,],
                                                "bl",
                                                population_death_log_clean[,"ID"]>0)

if(round(DeathEventNoHist2[1],3) !=0.286){
  stop("Error in probabilities of death - logistic regression estimates")
}

##Death, Gompertz##############################################################
population_death_Gomp <-read.csv("Unit tests/UKPDS82standard person - DeathHistNoEvent.csv")
population_death_Gomp_clean <- build_population(population_death_Gomp, PopulationVariables)

DeathNoEventHistUKPDS82 <- Death_NoEvent_Hist_UKPDS_82(population_death_Gomp_clean,
                                                       parameter[1,],
                                                       parameter[1,"Diab_Mort_Hist_NoEvent_Phi"],
                                                       "bl",
                                                       population_death_Gomp_clean[,"ID"]>0)
#do UKPDS82 calculations, their probs look wrong
UKPDS82_hazyrstart_hist_nevent <- 
  (0.073^-1)*exp(-9.207-0.293+0.374+0.048*6+0.632)*(exp(0.073*70)-1)

UKPDS82_hazyrend_hist_nevent<- 
  (0.073^-1)*exp(-9.207-0.293+0.374+0.048*6+0.632)*(exp(0.073*71)-1)

UKPDS82_pdeath_hist_nevent <- 
  1-exp(UKPDS82_hazyrstart_hist_nevent - UKPDS82_hazyrend_hist_nevent)
  
if(round(DeathNoEventHistUKPDS82[1],4) != round(UKPDS82_pdeath_hist_nevent, 4)){
  stop("Error in probabilities of death - Gompertz estimates")
}