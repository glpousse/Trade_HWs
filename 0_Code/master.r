#####################
#####################
# ---- Trade HW1 ----
#####################
#####################

# This script presents R code written as part of the empirical exercise in the HW1 assigment, as part of 
# Professor Thierry Mayer's M2 PhD Track class on International Economics. 

rm(list = ls()) 

# Replace the Working Directory to where .zip file is stored
wd <- "/Users/glpou/Documents/SCIENCESPO/M2/S4/Trade/HW1/"
setwd(wd)

# Libraries 
library(haven)

#################################################################
# ---- Empirical exercise : market potential and development ----
#################################################################

df_og <- read_dta("1_Data/biltrade.dta")



