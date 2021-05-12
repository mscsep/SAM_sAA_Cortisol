#' ---	
#' title: Load & Restructure Saliva data
#' author: "Milou Sep"	
#' date: "25/10/2018"	
#' output:	
#'   html_document: default	
#' ---	

# Load Packages -----------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)

# Load Data ---------------------------------------------------------------
# Load Cort &sAA Salivette data from excel masterfile in R.
Cort <- read_delim("data/SAM_sCortisol.csv", ";", escape_double = FALSE, trim_ws = TRUE, na = "NA")
sAA <- read_delim("data/SAM_sAA.csv", ";", escape_double = FALSE, trim_ws = TRUE, na = "NA")

# merge Data 
SalivaSAM <- list(Cort,sAA)

# Clean dataframes --------------------------------------------------------
# restructure variables
Clean_data <- list()
for ( i in 1:2){
  data <- SalivaSAM[[i]] %>% separate(`Proefpersoon.samplenummer`, into = c("id", "Sample")) # Str. split in Subject name & sample number [info from: https://stackoverflow.com/questions/4350440/split-data-frame-string-column-into-multiple-columns]
  data$id <- sub("SMGH", "", data$id) # Remove SMGH from SubjectNumber
  data$id <- as.factor(data$id)
  data$Sample <- as.factor(data$Sample)
  Clean_data[[i]] <- subset(data, select = -c( Notes)) # Remove "notes" colums
  # str(Clean_data[[i]]) # for checking
}
Saliva_data_clean <- merge(Clean_data[[1]], Clean_data[[2]], by=c("id", "Sample"))
names(Saliva_data_clean)[3:4] <- c("CORT", "sAA")

# Add experimental conditions ---------------------------------------------
# Load experimental conditions
experimental_conditions <- read.csv2("data/SAM_Experimental_Conditions.csv")
names(experimental_conditions) <- c("id", "Condition") # Rename to be similar to CORT & sAA dataframe 
experimental_conditions$id <- sub("SMGH", "", experimental_conditions$id) # Remove SMGH from SubjectNumber

# Merge CORT, sAA data & Experimental conditions
Saliva_data_condition <- merge(Saliva_data_clean, experimental_conditions, by=c("id"))
Saliva_data_condition$Condition <- factor(Saliva_data_condition$Condition, levels = c(1,2,3), labels=c("Delayed", "Direct", "Control"))

# Remove unneeded variables -----------------------------------------------
rm(Clean_data, Cort, data, sAA, SalivaSAM, i, Saliva_data_clean, experimental_conditions)

# Save cleaned endocrine data ---------------------------------------------
saveRDS(Saliva_data_condition,'processed_data/sAA_sCORT_condition_dataset.rds')
