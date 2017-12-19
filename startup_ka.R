#' reads in mock data for testing
#' KMA 8/10/17
#' 
path <- ifelse(file.exists("S:/"), "S:/", "/Volumes/files/Shared/")

#' function for loading multiple packages will assess whether a package needs to be installed before loading
#' the argument pkg takes a list of package names in quotes e.g. pkg = c("dplyr","tidyr")

pkgs <- c("tidyverse", "readxl", "RCurl")
for(pkg in pkgs) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
  library(pkg, character.only = TRUE)
}


#this one merges files together
source("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/develop_ka/varname_to_varID.R")

#this one adds value labels when appropriate
source("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/develop_ka/add_valuesets.R")


#this one changes varnames to english titles (after values have been addressed)
source("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/develop_ka/change_varnames_to_vartitles.R")



surveyFolder <- "Admissions"
IPEDS_data_location_general <- "S:\\IRO\\resources\\IPEDS\\All Surveys"
IPEDS_data_location <- paste(IPEDS_data_location_general,surveyFolder, sep="\\")
IPEDS_test <- merge_IPEDS_data(IPEDS_data_location)
IPEDS_data <- IPEDS_test$data
IPEDS_dictionary <- IPEDS_test$dictionary
IPEDS_valuesets <- IPEDS_test$valuesets

IPEDS_data_values <- add_values(longtable = IPEDS_data, valueset = IPEDS_valuesets)

IPEDS_data_clean <- change_varnames_vartitles(longtable = IPEDS_data_values, varnames = IPEDS_dictionary)
