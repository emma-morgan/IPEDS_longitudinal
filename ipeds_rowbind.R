# csv approach to row binding ipeds files over time
# KMF 8.10.17

# assumes that IPEDS csvs have been downloaded and are saved in folders by IPEDS survey name 
#   (i.e. only fall enrollment files are saved in the fall enrollment folder, etc)

#set up the directories where your files are stored
inputDirectory <- #path to the folder containing all the csvs for a given IPEDS survey and nothing else#
outputDirectory <- #path to a DIFFERENT folder, where you want to save comipled file.

#for loop to read in all the files in the inputDirectory
for (i in 1:length(list.files())) {
  setwd(inputDirectory)
  fileName <- list.files()[i]
  ds <- read.csv(fileName, check.names=FALSE)
  ds_list <- list()
  ds_list[[i]] <- assign(paste("ds",i,sep=""), ds)
}




##########################

# KF TESTING WITH HER OWN FILE PATHS
IPEDSSURVEY <- "Fall Enrollment"

path <- ifelse(file.exists("S:/"), "S:/", "/Volumes/files/Shared/")

inputDirectory <- paste0(path,"IRO/resources/IPEDS/(Old) csv file compilation/", IPEDSSURVEY, "/input")
outputDirectory <- paste0(path,"IRO/resources/IPEDS/", IPEDSSURVEY, "/compiled")
datedDirectory <- paste0(path,"IRO/resources/IPEDS/", IPEDSSURVEY, "/Dated Directory")



