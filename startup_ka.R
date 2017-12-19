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
source("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/varname_to_varID.R")

#this one adds value labels when appropriate
source("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/add_valuesets.R")


#this one changes varnames to english titles (after values have been addressed)
source("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/change_varnames_to_vartitles.R")

peerlist <- read.csv(paste0(path, "IRO/resources/IPEDS/Peer List.csv"))

surveyFolder <- "Completions"
IPEDS_data_location_general <- "S:\\IRO\\resources\\IPEDS\\All Surveys"
IPEDS_data_location <- paste(IPEDS_data_location_general,surveyFolder, sep="\\")
IPEDS_test <- merge_IPEDS_data(IPEDS_data_location)
IPEDS_data <- IPEDS_test$data
IPEDS_dictionary <- IPEDS_test$dictionary
IPEDS_valuesets <- IPEDS_test$valuesets


IPEDS_data <- subset(IPEDS_data, IPEDS_data$UNITID %in% peerlist$unitid)

IPEDS_data_values <- add_values(longtable = IPEDS_data, valueset = IPEDS_valuesets, ignore_size_warning = T)

IPEDS_data_clean <- change_varnames_vartitles(longtable = IPEDS_data_values, varnames = IPEDS_dictionary, ignore_size_warning = T)

write.csv(IPEDS_data_clean,  paste0(IPEDS_data_location,"/compiled/",surveyFolder, "_compiled.csv"), row.names = F)

write.csv(IPEDS_dictionary,  paste0(IPEDS_data_location,"/compiled/",surveyFolder, "_dictionary.csv"), row.names = F)

write.csv(IPEDS_valuesets,  paste0(IPEDS_data_location,"/compiled/",surveyFolder, "_valuesets.csv"), row.names = F)
