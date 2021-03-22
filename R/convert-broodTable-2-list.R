#==================================================================================================
#Project Name: KUSKOKWIM RIVER CHINOOK SALMON FORECAST MODEL - Convert brood table to list
#Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
#Date: 4.9.20
#
#Purpose: To convert brood table (matrix) to a list more useful for sibling or stock-recruitment based forecasts.
#
#==================================================================================================
#NOTES:
#
#==================================================================================================
require(tidyverse)
require(dplyr)

# Define Workflow Paths ============================================
# *Assumes you are working from the Sergent_Streamflow R project
wd <- getwd()
dir.data <- file.path(wd,"data")

# Read in Brood Table ==============================================
brood.dat <- read.csv(file.path(dir.data, "Kusko-BroodTable.csv"), header=TRUE)

# Convert to List =================================================
brood.list <- brood.dat %>% gather(-Escapement)

head(brood.list)
