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