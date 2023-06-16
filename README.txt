To run this model you must install R and R studio. Our analyses were run in R version 4.3.0

Before running any models you should add a folder called Results to this directory. This folder is ingored by git version control and contains the actual model results. 

To simply run the analyses detailed in the attached report, open the Embedding Economics Analysis.Rproj in R studio. Load and then run the .R script you want to run. 

The main file to run is the Run all Analyses.R file. This replicates the analyses in Davies et al [REF to be added when published] 

The other files contained in the directory are: 
all_model_files.R, this file calls in all R functions used in the model (all of the scripts containing these functions are located in the folder called R)
Global Options.R, this file generates a matrix containing all Global Options in the model (e.g. discount rate) and a column explaining what each Global Option is. This is pulled through the simulation to control what functions return

The other folders and files contained within are:
data - this folder contains the data used to populate the simulations
LifeTables.csv, is a reformatting of ONS lifetables for use in the simulation
parameter.rda, is a file containing all of the model parameters, with thier mean values in the first row and sampled uncertainty in the subsequent rows
PopulationVariables.csv, is a file containing a list of all vairables that each patient in the simulation will have and a description of what each varaible is

Populations - this contains the population that is run through the model, prior to cleaning. We store these data in .csv files
csv files contained here contained simulated populations, if modifying with data that is directly bootstrapped from individual patient level data, make sure that you do not load your population csv file into this folder and make it
available under seperate licence terms. 


R - this folder contains all of the user written functions in the simulation model
build_population.R - this contains the functions that convert the population csv files into the populations run throught the simulation
Cancer Risks.R - this contains the functions that estimate the probability of developing cancer in each year of the simulation
Costs.R - this contains the function that calculates the cost per year in the simulation
Depression.R - this contains the function that calculates the probability of developing depression in a given year
Generate Results Template.R - this contains the function that generates the correct matrix to store the model results top be outputted by the simulation
Generate Results.R - this contains functions that populate specific resulsts templates (althought  the defaul method is contained in Run model.R as this is necessary for most results collection methods)
generate_random.R - this contains the function that generates the common random numbers. This is different for each patient for each event for up to 100 years. These are the same across PSA runs and different analysis arms
intervention.R - this contains the functions that estimate the matrices given the intervention effects on A1c, BMI, SBP, HDL and LDL cholesterol for each patient in each year. This also contains the function that estimates whether each
patient attends an SSME course in a given year. 
LifeTableMortality.R - this contains the function that matches each patient's gender and age to all cause mortality from ONS life tables
Oestoarthritis functions.R - this contains the function that estimates each patient's probability of developing oestoarthritis
QALYs.R - this constains the function that estimates each patients QALY in accrued in each of the simulation. For patients who live this is equivalent to their utility in each year, but patients who die are assumed to die halfway
through the year. 
Run model.R - this contains the function that will conduct a full model run for a single row of the parameters matrix
Run simulation.R - this contains the function that runs the model for either the first row of the parameters matrix (deterministic) or across multiple rows to run the PSA analysis
UKPDS 82 risk functions.R - this contains the functions that predict the probability of all events in the UKPDS 82 risk functions
UKPDS 90 risk functions.R - this contains the functions that implement all of the equations in UKPDS 90 
Update Events.R - this contains the fuction that happens at the end of each simulation year. It ensures that all events that year are recorded as historical events and events are set to not happening for the next simulation year
Update Pat Chars.R - this contains the function that happens at the end of each simulation year. This ensures that all patient characteristics are set to what they should be for the next year in the simulation. 

Standard model runs - work in progress. This folder contains some .R files to run the model in standard configurations to allow easier bug checking. 
UKPDSpopNoLifeTableMort.R - this file allows comparison of model event rates to the appendices of UKPDS 90 for validation


Unit test - This folder contains some unit tests to help with debugging, particuklarly in the risk functions used in this model 
Deterministic Runs.R - this runs the base case deterministically
Patient Stability Check.R - this runs the base case deterministically for a large number of patients. It sets the model to return the patient characteristics matrix, so that mean discounted QALYs and mean discounted costs can be assessed
with respect to the number of patients in the simulation
Test Costs.R - file used to test what costs are returned in different model configruations in this simulation
Test Interventions BMI.R - this file will test the model BMI function in intervention.R to see what BMI's are returned in different model configurations in the determinstic model
Test Interventions HDL.R - this file will test the model HDL function in intervention.R to see what HDL's are returned in different model configurations in the determinstic model
Test Interventions LDL.R - this file will test the model HDL function in intervention.R to see what HDL's are returned in different model configurations in the determinstic model
Test Interventions SBP.R - this file will test the model HDL function in intervention.R to see what HDL's are returned in different model configurations in the determinstic model
Test Interventions HBA1c.R - this file will test the model HbA1c function in intervention.R to see what HbA1c's are returned in different model configurations in the determinstic model
UKPDS82.R - this file will test the functions written in UKPDS82.R versus any validation descriptions constained in the appendices of this paper
UKPDS90.R - this file will test the functions written in UKPDS90.R versus any validation descriptions constained in the appendices of this paper
UKPDS82standard person - CHF.csv, UKPDS82standard person - DeathEventNoHist.csv, UKPDS82standard person - DeathHistNoEvent.csv, UKPDS90standardperson.csv - this files contain the patient characteristics of people used in 
the UKPDS82.R and UKPDS90.R files

