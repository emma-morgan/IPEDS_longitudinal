### create compiled multi year dictionaries to be used with Emma's code
#ECM
#7.6.2016
#Code for compiling IPEDS dictionary files across years
#Adjusted KMF code to merge into unduplicated file, taking variable title from most recent year
#Adjustments are still needed to standardize and use for multiple purposes
#Tufts University OIRE
#emma.morgan@tufts.edu

# KMF
# 7.1.16
# Smith College IR
# kfoley@smith.edu


#This takes dictionary excel files downloaded directly form the IPEDS website with the same name
#current example is for fall enrollment (ef_a)
#with a different file, year will need to be changed. I have code for this somewhere and need to add it in

 
# reshape package must be loaded for merge__all (ECM)
library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(reshape)
library(readxl)

#set wd as necessary for a given set of varlist csvs
setwd("Q:/Staff/OIRE/Staff Folders/Emma/Peer Database/IPEDS/Fall Enrollment A")

#create empty list and read in all teh csvs in your wd
ds_list <- list()

for (i in 1:length(list.files())) {

  workFile <- list.files()[i]
  year <- substr(workFile,3,6)
  capture.output(sheets <- excel_sheets(workFile), file='NUL')
  sheets <- tolower(sheets)
  if ("varlist" %in% sheets) {
    sheetName <- "varlist"
  }
  else {
    print(sheets)
    sheetName <- readline(prompt="Enter the name of the sheet: ")
  }
  capture.output(ds <- read_excel(workFile,sheetName), file ='NUL')
  ds[['year']] <- year
  names(ds) <- toupper(names(ds))
  ds_list[[i]] <- assign(paste("ds",i,sep=""),ds)
}

rm(ds)

myall <- merge_all(ds_list)
all_unduplicated <- unique(ddply(myall, .(VARNAME), function(x) data.frame(VARNAME=x[,"VARNAME"], 
                                                    YEAR=max(x$YEAR))))

final_all <- merge(all_unduplicated, myall, all=FALSE)  

#write compiled dictionary file to folder

write.csv(final_all, "Q:/Staff/OIRE/Staff Folders/Emma/Peer Database/IPEDS/Compiled Dictionary/efa_dictionary.csv")

