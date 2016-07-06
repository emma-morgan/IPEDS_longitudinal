### create compiled multi year dictionaries to be used with Emma's code
# KMF
# 7.1.16
# Smith College IR
# kfoley@smith.edu

##This script assumes IPEDS "varlist" tabs from dictionary files have been saved as csv
# and that the "vartitle" column has been renamed to include the year (i.e. "2008vartitle")

# frequenlty used packages that I just always library in case I need them.  
# reshape package must be loaded for merge_recurse
library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(reshape)

#set wd as necessary for a given set of varlist csvs
setwd("Q:/Staff/OIRE/Staff Folders/Emma/Peer Database/IPEDS/Graduations")

#create empty list and read in all teh csvs in your wd
ds_list <- list()

for (i in 1:length(list.files())) {

  workFile <- list.files()[i]
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
  names(ds) <- toupper(names(ds))
  ds_list[[i]] <- assign(paste("ds",i,sep=""),ds)
}

rm(ds)



for (i in 1:length(list.files())){
  ds <- read.csv(list.files()[i], check.names=FALSE)
  colnames(ds)=gsub(".*\\.", "", colnames(ds))
  ds_list [[i]]<- assign(paste("ds", i,sep=""), ds)
  
}

rm(ds)

all <- merge_recurse(ds_list)

  #Some years of data have lowercase headers, so change all to uppercase for consistency
  names(ds1) <- toupper(names(ds1))
  names(ds2) <- toupper(names(ds2))
  names(ds3) <- toupper(names(ds3))
  names(ds4) <- toupper(names(ds4))
  names(ds5) <- toupper(names(ds5))
  names(ds6) <- toupper(names(ds6))
  names(ds7) <- toupper(names(ds7))

  
    
# join all varlist files together -- outer join
  
all <- merge(ds1, ds2, by = intersect(names(ds1), names(ds2)), all.x=T, all.y=T) 
all <- merge(all, ds3, by = intersect(names(all), names(ds3)), all.x=T, all.y=T)
all <- merge(all, ds4, by = intersect(names(all), names(ds4)), all.x=T, all.y=T)
all <- merge(all, ds5, by = intersect(names(all), names(ds5)), all.x=T, all.y=T)
all <- merge(all, ds6, by = intersect(names(all), names(ds6)), all.x=T, all.y=T)
all <- merge(all, ds7, by = intersect(names(all), names(ds7)), all.x=T, all.y=T)

#write compiled dictionary file out to S drive

write.csv(all, "/Volumes/files-1/Shared/IRO/DATA/WORK/IPEDS/IPEDS cleanup using R/Fall Enrollment/dictionaries/compiled dictionary/compiled.csv")

