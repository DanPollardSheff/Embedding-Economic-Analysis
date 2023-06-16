####'This function runs the model for a given set of patients and parameters
####'@param population_, is the population matrix
####'@param parameters_, is one row of the parameters matrix
####'@param endtime_, is the number of years to run the simulation for
####'@param GlobalVars_, is the matrix giving the global variables
####'@param random_numbs_, is an array of common random numbers giving a random number
####'@param LifeTables_, is a dataframe containing life table information in formate
####'that can easily be matched to the population matrix
####'draw for each patient in each year for every event in which a random number 
####'is required 
####'@param SOUR_ is the number of the second order uncetainy run (PSA run)
####'@return results, is the results matrix
run_model <- function(population_, 
                      parameters_, 
                                  endtime_, 
                                  treatment_, 
                                  GlobalVars_,
                                  random_numbers_,
                                  LifeTables_){
  #see if the model run is deterministic
  #if it is run the model on the mean parameter values
  if(GlobalVars_["run_psa","Value"]==F){
    
    modelresults <- run_simulation(population_, 
                                   parameters_, 
                                     endtime_, 
                                     treatment_, 
                                     GlobalVars_,
                                     random_numbers_,
                                     LifeTables_,
                                     1)
  }else{#otherwise the model is PSA
    PSAiterations <- 2:(as.numeric(GlobalVars_["psa_count","Value"])+1)
    
    #set up parrallel processing
    #these are the additional global variables that need to be set for parrallel
    #processing functions
    numCores <- detectCores()-1
    numCores <- min (numCores, as.numeric(GlobalVars["Number of cores", "Value"]))
    cl <- makeCluster(numCores)
    #Set a random number seed for parallel processing
    clusterEvalQ(cl, set.seed(1))
    
    #Set up the cluster processing and push all objects in the global enviroment 
    #to all the clustes
    registerDoParallel(cl)
    clusterExport(cl, ls(envir = .GlobalEnv))
    modelresults <- parLapply(cl = cl,
                           PSAiterations, 
                           run_simulation,
                           population_ = population_,
                           parameters_ = parameters_,
                           endtime_ = endtime_,
                           treatment_ = treatment_, 
                           GlobalVars_ = GlobalVars_, 
                           random_numbs_ = random_numbers_,
                           LifeTables_ = LifeTables_)
    stopCluster(cl)
    
    if(GlobalVars["Results_output", "Value"] == "Summary"){
      modelresults <- matrix(unlist(modelresults), ncol=24, byrow=T)
    colnames(modelresults) <- c("Life Years", 
                                "Discounted Life Years",
                                "QALYs", 
                                "Discounted QALYs", 
                                "Costs",
                                "Discounted Costs",
                                "10 year 1st MIs",
                                "10 year 2nd MIs",
                                "10 year 1st Strokes",
                                "10 year 2nd Strokes",
                                "10 year CHF",
                                "10 year IHD",
                                "10 year blindness",
                                "10 year ulcers",
                                "10 year 1st amputations",
                                "10 year 2nd amputations",
                                "10 year renal failures",
                                "10 year PVD",
                                "10 year MMALB",
                                "10 year Atrial Fibriliation",
                                "10 year breast cancer",
                                "10 year colorectal cancer",
                                "10 year depression",
                                "10 year oestoarthritis")
    }
  }
  return(modelresults)
}
