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


