# csv approach to row binding ipeds files over time
# KMF 8.10.17

# assumes that IPEDS csvs have been downloaded and are saved in folders by IPEDS survey name 
#   (i.e. only fall enrollment files are saved in the fall enrollment folder, etc)

#assumes the pre-existence of a compiled vartable csv



#set up the directories where your files are stored
#inputDirectory <- #path to the folder containing all the csvs for a given IPEDS survey and nothing else#
#outputDirectory <- #path to a DIFFERENT folder, where you want to save comipled file.
#sourceDirectory <- #path to a DIFFERENT folder, where you have saved the "filename to tablename" script
#vartableDirectory <-  #path to a DIFFERENT folder, where you have saved the compiled vartable csv
  
 
#first, install and require all necessary packages
pkgs <- c( "dplyr", "stringr", "RCurl")
for(pkg in pkgs) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
  library(pkg, character.only = TRUE)
}
 
  
  
#source FROM GITHUB MASTER the function that trims file name into table name
source("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/filename_to_tablename.R")
#source FROM GITHUB MASTER the function that generates academic year field
source("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/acad_yr_function.R")


#read in compiled varatable csv and create reference file for table name and survey
vartable <- read.csv(paste0(path, "IRO/resources/IPEDS/documentation/vartable_compiled_uniqueTitles.csv"))
vartable$TABLE_TRIM <- table_from_column(vartable$TABLENAME)
ipeds_tables <- unique(vartable[,c("TABLE_TRIM", "SURVEY")])
ds_list <- list()

#IMPORTANT NOTE -- IPEDS finance survey csv for year 07-08 had an error that we fixed by hand.
# the fields F2A20 and XF2A20 were swtiched.  must be fixed for files to bind correctly.

setwd(inputDirectory)
#for loop to read in all the files in the inputDirectory and store them in a list
for (i in 1:length(list.files())) {
  fileName <- list.files()[i]
  ds <- read.csv(fileName, check.names=FALSE, stringsAsFactors = F, na.strings = c(".", "", " ", NA))
  #call function to trim dates out of csv filename -- create Table Name
  ds$TABLE_TRIM <- table_from_file(inputDirectory)
  #join "SURVEY" field in from ipeds_tables, and then un-factorize SURVEY
  ds <- dplyr::left_join(ds, ipeds_tables, by = "TABLE_TRIM")
  SURVEY <- as.character(first(ds$SURVEY))
  # call adacemic year function
  ds$ACAD_YEAR <- acad_year(fileName, SURVEY)
  #store each ds in a list
  ds_list[[i]] <- assign(paste("ds",i,sep=""), ds)
}


#row bind the ds's together
full_ds <- dplyr::bind_rows(ds_list)

#remove all fields that begin with X -- these are imputed.
full_ds <- select(full_ds, -starts_with("X"))


#subset and write out the compiled file
# eventually, call a subset function that emma will write, but for now just do it to test
full_ds <- subset(full_ds, full_ds$UNITID %in% peerlist$unitid)
write.csv(full_ds,  paste0(outputDirectory, "/",IPEDSSURVEY, "_compiled.csv"), row.names = F)


##########################

# KF TESTING WITH HER OWN FILE PATHS
IPEDSSURVEY <- "Student Financial Aid"
path <- ifelse(file.exists("S:/"), "S:/", "/Volumes/files/Shared/")
setwd(path)
peerlist <- read.csv(paste0(path, "IRO/resources/IPEDS/Peer List.csv"))


inputDirectory <- paste0(path,"IRO/resources/IPEDS/", IPEDSSURVEY, "/input")
outputDirectory <- paste0(path,"IRO/resources/IPEDS/", IPEDSSURVEY, "/compiled")
sourceDirectory <-paste0(path,"IRO/resources/IPEDS/code/")
vartableDirectory <- paste0(path,"IRO/resources/IPEDS/documentation/")


