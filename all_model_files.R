sapply(list.files("R",full.names=T), source)

#Load parameters
load("data/parameter.rda")
#Load population variables
PopulationVariables <- read.csv("data/PopulationVariables.csv")
#Load life tables
LifeTables <- as.matrix(read.csv("data/LifeTables.csv"))
#Makes the Lifetables numeric, like the population matrix
LifeTables[,"AGE"] <- as.numeric(LifeTables[,"AGE"] )
LifeTables[,"FEMALE"] <- as.numeric(LifeTables[,"FEMALE"])

