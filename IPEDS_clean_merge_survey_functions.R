#*****************************************************************
# Function to clean and merge IPEDS survey files across years
# Includes general functions as well as more specific and nuanced functions
# that will be used to clean parts of specific surveys
#*****************************************************************

# Contact Emma Morgan with questions
# Emma.Morgan@tufts.edu
# Updated 11/24/15

library(plyr)

IPEDS_clean_merge <- function (inputDirectory, varDictionary, peerFile, 
                               outputDirectory, outputName, datedDirectory, freq_table, lookup_dict) {
  
  # Specify where R will find your file set
  setwd(inputDirectory)
  
  # Set up necessary blank lists
  # List to hold survey data from various years
  ds_list <- list()
  
  # For each file in directory:
  # (1) read in the file
  # Create list of rows to keep based on peers/comparison schools
  # Create list of Columns to drop (imputation columns or blank fields for Tufts)
  # store "good" data in file_list
  
  # Read in all years of survey files; capitalize to standardize column titles
  for (i in 1:length(list.files())) {
    fileName <- list.files()[i]
    ds <- read.csv(fileName, check.names=FALSE)
    
    #Some years of data have lowercase headers, so change all to uppercase for consistency
    names(ds) <- toupper(names(ds))
    dropCol <- c()
    
    # below, we will Select rows for comparison institutions
    # this will use UNITID %in% list from peerFile$unitid
    
    # identify columns to delete (imputation & blank for Tufts)
    tuftsRow <- which(ds$UNITID == "168148")
    
    #We will only want to delete based on "blanks" if we have one row for Tufts;
    #If there are mutliple Tufts rows, then only remove imputation variables 
    
    # When there is only one row per institution, remove imputation rows
    # and any column for which Tufts (or your main institution) has a null value
    
    if (length(tuftsRow) == 1) {
      myRow <- tuftsRow
      for (k in 1:ncol(ds)) {
        
        #drop imputation columns
        if (substring(names(ds)[k],1,1) == "X") {
          dropCol <- c(dropCol,k)
        }
        
        #Drop columns for which primary institution is null/blank
        else if (is.na(ds[myRow,k])) {
          dropCol <- c(dropCol,k)
        }
        else if (ds[myRow,k] == ".") {
          dropCol <- c(dropCol,k)
        }
      }
    }
    
    #If we have multiple rows per file, only remove imputation columns and leave all others
    
    else if (length(tuftsRow > 1)) {
      for (k in 1:ncol(ds)) {
        if (substring(names(ds)[k],1,1) == "X") {
          dropCol <- c(dropCol,k)
        }
      }
    }
    
    # create a new ds with only the "good" rows and columns
    ds_good <- ds[ds$UNITID %in% peerFile$unitid, -dropCol]
    
    # We now want to add columns for Fiscal Year, Fall, and Institution Name
    # Create placeholders first
    ds_good$FY <- ""
    ds_good$Fall <- ""
    ds_good$Institution_Name <- ""
    
    # Add FY and "Fall" for joining purposes in Tableau
    # FY is based on the type of survey file and when data is collected
    # IPEDS_FY function is defined below
    
    #NEED TO CHANGE THIS - MADE ADJUSTMENTS TO IPEDS_FY FUNCTION
    # function now returns fall, FY, surveyName, surveyType, and fileName
    
    survey_details <- IPEDS_FY(fileName)
    
    ds_good$FY <- survey_details[[1]]
    ds_good$Fall <- ds_good$FY-1
    
    ds_good$`Survey Code` <- survey_details[[2]]
    ds_good$`Survey Title` <- survey_details[[3]]
    survey <- survey_details[[2]]
    
    # Add Institution Name
    for (l in 1:nrow(ds_good)) {
      p <- which(ds_good$UNITID[l] == peerFile$unitid)
      ds_good$Institution_Name[l] <- as.matrix(peerFile$`institution name`)[p]
    }
    
    # Store the good info in our new list
    ds_list[[i]] <- assign(paste("ds",i,sep=""), ds_good)
    
  }
  

  full_ds <- as.data.frame(rbind.fill.matrix(ds_list))
  
  #For certain surveys, we may want to reshape the data to be more user-friendly
  #Functions used in these steps are all defined below
  
  if (freq_table==TRUE) {
    full_ds <- lookup(full_ds, lookup_dict)
  }

  #Want to fix these so we actually run them according to the type of survey, not acronym
  #Generate admissions stats by gender for relevant data set (IC)
  if ("ENRLT" %in% names(full_ds)) {
    admit_by_gender(full_ds)
  }
  
   
#   # Add degree level and field of study descriptions using degree_cip
#   if (paste(substr(fileName,1,1),substr(fileName,6,7),sep="")=="c_a") {
#     full_ds <- degree_cip(full_ds)
#   }
#   
  
  #Generate fall enrollment stats by gender and race/ethnicity for relevent data set (EFA_A)
  if ("EFTOTLT" %in% names(full_ds) & "EFALEVEL" %in% names(full_ds)) {
    enroll_by_GRE(full_ds)
  }
  
  # Replace variable codes with full header titles
  
  for (m in 1:ncol(full_ds)) {
    if (names(full_ds)[m] %in% varDictionary$varname) {
      a <- which(names(full_ds)[m] == as.matrix(varDictionary$varname))
      names(full_ds)[m] <- as.matrix(varDictionary$varTitle)[a]
    }
  }
  
  # Set directory to where you want the file to output
  setwd(outputDirectory)
  
  # Write full .csv to output directory
  write.csv(full_ds, paste(outputName, ".csv", sep=""), row.names = FALSE, na="")
  
  #Set dated directory to save copies of files
  setwd(datedDirectory)
  write.csv(full_ds, file = paste(outputName,Sys.Date(), ".csv",sep=""),row.names = FALSE, na="")
  
}


# *******************************************************************************
# Function for general lookup tables
# This can be used for ANYTHING that has a frequency table in addition to a varlist
# function: lookup
# *******************************************************************************

trim.leading <- function (x)  sub("^\\s+", "", x)

lookup <- function(myData,lookup_dict) {
  mydict_directory <- "Q:/Staff/President, Provost, Trustees/TAAC Dashboard/Peer Comparative Data/IPEDs Data Center/IPEDS Surveys/IPEDS Dictionary"
  
  #mydict_directory <- "P:/Tableau user group/3-1-16/Reference"
  dictionary_title <- lookup_dict
  setwd(mydict_directory)
  myDict <- read.csv(dictionary_title, check.names = FALSE)
  
  codebooklist <- list()
  
  colnum <- ncol(myData)
  
  #Establish codebooks for each variable; additionally create columns for description and rename these accordingly
  
  lookupvars <- levels(myDict$varname)
  
  for (k in 1:length(lookupvars)) {
    codebook <- myDict[myDict$varname == lookupvars[k],]
    codebooklist[[k]] <- assign(paste("codebook",k,sep="_"),codebook)
    myData[,colnum+k] <- ""
    colnames(myData)[colnum+k] <- paste(lookupvars[k],"desc",sep="_")
  }
  
  for (i in 1:nrow(myData)) {
    for (j in 1:length(lookupvars)) {
      if (lookupvars[j] %in% names(myData)) {
        mycol <- which(names(myData)==lookupvars[j])
        myData[,mycol] <- trim.leading(myData[,mycol])
        a <- which(as.numeric(myData[i,mycol]) == codebooklist[[j]]$codevalue)
        myData[i,colnum+j] <- as.matrix(codebooklist[[j]]$valuelabel)[a]
      }
    }
  }
  return (myData)
}



# *******************************************************************************
# Function for Admissions from IPEDS by Gender, survey IC_AY
# function: admit_by_gender
# *******************************************************************************
admit_by_gender <- function(full_ds) {
  
  myFile <- full_ds
  
  m <- 3*nrow(myFile)
  
  df <- matrix("",ncol=10, nrow=m)
  
  for (i in 1:nrow(myFile)) {
    r <- 3*(i-1)+1
    r2 <- r+2
    df[r:r2,1] <- as.matrix(full_ds$UNITID)[i]
    df[r:r2,2] <- as.matrix(full_ds$'Institution_Name')[i]
    df[r:r2,3] <- as.matrix(full_ds$Fall)[i]
    df[r:r2,4] <- as.matrix(full_ds$FY)[i]
    df[r:r2,5] <- c("Men","Women","Total")
    
    #Applicants
    df[r,6] <- as.matrix(full_ds$APPLCNM)[i]
    df[r+1,6] <- as.matrix(full_ds$APPLCNW)[i]
    df[r+2,6] <- as.matrix(full_ds$APPLCN)[i]
    
    #Admits
    df[r,7] <- as.matrix(full_ds$ADMSSNM)[i]
    df[r+1,7] <- as.matrix(full_ds$ADMSSNW)[i]
    df[r+2,7] <- as.matrix(full_ds$ADMSSN)[i]
    
    #Enrolled
    df[r,8] <- as.matrix(full_ds$ENRLM)[i]
    df[r+1,8] <- as.matrix(full_ds$ENRLW)[i]
    df[r+2,8] <- as.matrix(full_ds$ENRLT)[i]
    
    #Enrolled - Full Time
    df[r,9] <- as.matrix(full_ds$ENRLFTM)[i]
    df[r+1,9] <- as.matrix(full_ds$ENRLFTW)[i]
    df[r+2,9] <- as.matrix(full_ds$ENRLFT)[i]
    
    #Enrolled - Part Time
    df[r,10] <- as.matrix(full_ds$ENRLPTM)[i]
    df[r+1,10] <- as.matrix(full_ds$ENRLPTW)[i]
    df[r+2,10] <- as.matrix(full_ds$ENRLPT)[i]
    
  }
  
  dframe <- data.frame(df)
  
  names(dframe) <- c("UNITID","Institution Name", "Fall","FY", "Gender", "Applicants", "Admits", "Enrolled", "Enrolled - Full Time", "Enrolled - Part Time")
  
  setwd("Q:/Staff/President, Provost, Trustees/TAAC Dashboard/Peer Comparative Data/IPEDs Data Center/Compiled Survey Files from R")
  write.csv(dframe, "Undergrad Admissions Stats by Gender.csv", row.names=FALSE)
  
  setwd("Q:/Staff/President, Provost, Trustees/TAAC Dashboard/Peer Comparative Data/IPEDs Data Center/R Output for Tableau/Dated Files")
  write.csv(dframe, file = paste("Undergrad Admit Stats by Gender",Sys.Date(), ".csv",sep=""),row.names = FALSE)
}


# 
# *******************************************************************************
# Function for Fall Enrollment from IPEDS by Gender and Race/Ethnicity
# function: enroll_by_GRE, survey EF_A
# Create files for FTFT fall enrollment and total degree-seeking undergrad fall enrollment
# *******************************************************************************

enroll_by_GRE <- function(full_ds) {
  
  myFile <- full_ds
  
  ftft <- myFile[myFile$EFALEVEL==24,]
  ug <- myFile[myFile$EFALEVEL==3,]
  
  m1 <- 3*nrow(ftft)
  m2 <- 3*nrow(ug)
  
  df1 <- matrix("", ncol=16, nrow=m1)
  df2 <- matrix("", ncol=16, nrow=m2)
  
  # FTFT Undergraduate
  for (i in 1:nrow(ftft)) {
    r <- 3*(i-1)+1
    r2 <- r+2
    df1[r:r2,1] <- as.matrix(ftft$UNITID)[i]
    df1[r:r2,2] <- as.matrix(ftft$'Institution_Name')[i]
    df1[r:r2,3] <- as.matrix(ftft$Fall)[i]
    df1[r:r2,4] <- as.matrix(ftft$FY)[i]
    df1[r:r2,5] <- c("Men","Women","Total")
    
    #American Indian or Alaska Native
    df1[r,6] <- as.matrix(ftft$EFAIANM)[i]
    df1[r+1,6] <- as.matrix(ftft$EFAIANW)[i]
    df1[r+2,6] <- as.matrix(ftft$EFAIANT)[i]
    
    #Asian
    df1[r,7] <- as.matrix(ftft$EFASIAM)[i]
    df1[r+1,7] <- as.matrix(ftft$EFASIAW)[i]
    df1[r+2,7] <- as.matrix(ftft$EFASIAT)[i]
    
    #Black or African American
    df1[r,8] <- as.matrix(ftft$EFBKAAM)[i]
    df1[r+1,8] <- as.matrix(ftft$EFBKAAW)[i]
    df1[r+2,8] <- as.matrix(ftft$EFBKAAT)[i]
    
    #Hispanic
    df1[r,9] <- as.matrix(ftft$EFHISPM)[i]
    df1[r+1,9] <- as.matrix(ftft$EFHISPW)[i]
    df1[r+2,9] <- as.matrix(ftft$EFHISPT)[i]
    
    #Native Hawaiian or other Pacifc Islander
    df1[r,10] <- as.matrix(ftft$EFNHPIM)[i]
    df1[r+1,10] <- as.matrix(ftft$EFNHPIW)[i]
    df1[r+2,10] <- as.matrix(ftft$EFNHPIT)[i]
    
    #White
    df1[r,11] <- as.matrix(ftft$EFWHITM)[i]
    df1[r+1,11] <- as.matrix(ftft$EFWHITW)[i]
    df1[r+2,11] <- as.matrix(ftft$EFWHITT)[i]
    
    #Two or more Races
    df1[r,12] <- as.matrix(ftft$EF2MORM)[i]
    df1[r+1,12] <- as.matrix(ftft$EF2MORW)[i]
    df1[r+2,12] <- as.matrix(ftft$EF2MORT)[i]
    
    #Nonresident Alien
    df1[r,13] <- as.matrix(ftft$EFNRALM)[i]
    df1[r+1,13] <- as.matrix(ftft$EFNRALW)[i]
    df1[r+2,13] <- as.matrix(ftft$EFNRALT)[i]
    
    #Race/Ethnicity Unknown
    df1[r,14] <- as.matrix(ftft$EFUNKNM)[i]
    df1[r+1,14] <- as.matrix(ftft$EFUNKNW)[i]
    df1[r+2,14] <- as.matrix(ftft$EFUNKNT)[i]
    
    #Grand Total
    df1[r,15] <- as.matrix(ftft$EFTOTLM)[i]
    df1[r+1,15] <- as.matrix(ftft$EFTOTLW)[i]
    df1[r+2,15] <- as.matrix(ftft$EFTOTLT)[i]
    
    df1[r:r2,16] <- as.matrix(ftft$EFALevel_desc)[i]
    
  }
  
  dframe1 <- data.frame(df1)
  
  names(dframe1) <- c("UNITID","Institution Name", "Fall","FY", "Gender", "American Indian or Alaska Native","Asian","Black or African American","Hispanic","Native Hawaiian or other Pacifc Islander","White","Two or more Races","Nonresident Alien","Race/Ethnicity Unknown","Grand Total","Level of Student")
  
  setwd("Q:/Staff/President, Provost, Trustees/TAAC Dashboard/Peer Comparative Data/IPEDs Data Center/Compiled Survey Files from R")
  write.csv(dframe1, "FTFT Undergrad Fall Enrollment by Gender and RE.csv", row.names=FALSE)
  
  setwd("Q:/Staff/President, Provost, Trustees/TAAC Dashboard/Peer Comparative Data/IPEDs Data Center/R Output for Tableau/Dated Files")
  write.csv(dframe1, file = paste("FTFT Undergrad Fall Enrollment by Gender and RE",Sys.Date(), ".csv",sep=""),row.names = FALSE)
  
  # All undergraduate degree/certificate seeking students
  for (i in 1:nrow(ug)) {
    r <- 3*(i-1)+1
    r2 <- r+2
    df2[r:r2,1] <- as.matrix(ug$UNITID)[i]
    df2[r:r2,2] <- as.matrix(ug$'Institution_Name')[i]
    df2[r:r2,3] <- as.matrix(ug$Fall)[i]
    df2[r:r2,4] <- as.matrix(ug$FY)[i]
    df2[r:r2,5] <- c("Men","Women","Total")
    
    #American Indian or Alaska Native
    df2[r,6] <- as.matrix(ug$EFAIANM)[i]
    df2[r+1,6] <- as.matrix(ug$EFAIANW)[i]
    df2[r+2,6] <- as.matrix(ug$EFAIANT)[i]
    
    #Asian
    df2[r,7] <- as.matrix(ug$EFASIAM)[i]
    df2[r+1,7] <- as.matrix(ug$EFASIAW)[i]
    df2[r+2,7] <- as.matrix(ug$EFASIAT)[i]
    
    #Black or African American
    df2[r,8] <- as.matrix(ug$EFBKAAM)[i]
    df2[r+1,8] <- as.matrix(ug$EFBKAAW)[i]
    df2[r+2,8] <- as.matrix(ug$EFBKAAT)[i]
    
    #Hispanic
    df2[r,9] <- as.matrix(ug$EFHISPM)[i]
    df2[r+1,9] <- as.matrix(ug$EFHISPW)[i]
    df2[r+2,9] <- as.matrix(ug$EFHISPT)[i]
    
    #Native Hawaiian or other Pacifc Islander
    df2[r,10] <- as.matrix(ug$EFNHPIM)[i]
    df2[r+1,10] <- as.matrix(ug$EFNHPIW)[i]
    df2[r+2,10] <- as.matrix(ug$EFNHPIT)[i]
    
    #White
    df2[r,11] <- as.matrix(ug$EFWHITM)[i]
    df2[r+1,11] <- as.matrix(ug$EFWHITW)[i]
    df2[r+2,11] <- as.matrix(ug$EFWHITT)[i]
    
    #Two or more Races
    df2[r,12] <- as.matrix(ug$EF2MORM)[i]
    df2[r+1,12] <- as.matrix(ug$EF2MORW)[i]
    df2[r+2,12] <- as.matrix(ug$EF2MORT)[i]
    
    #Nonresident Alien
    df2[r,13] <- as.matrix(ug$EFNRALM)[i]
    df2[r+1,13] <- as.matrix(ug$EFNRALW)[i]
    df2[r+2,13] <- as.matrix(ug$EFNRALT)[i]
    
    #Race/Ethnicity Unknown
    df2[r,14] <- as.matrix(ug$EFUNKNM)[i]
    df2[r+1,14] <- as.matrix(ug$EFUNKNW)[i]
    df2[r+2,14] <- as.matrix(ug$EFUNKNT)[i]
    
    #Grand Total
    df2[r,15] <- as.matrix(ug$EFTOTLM)[i]
    df2[r+1,15] <- as.matrix(ug$EFTOTLW)[i]
    df2[r+2,15] <- as.matrix(ug$EFTOTLT)[i]
    
    df2[r:r2,16] <- as.matrix(ug$EFALevel_desc)[i]
    
  }
  
  dframe2 <- data.frame(df2)
  
  names(dframe2) <- c("UNITID","Institution Name", "Fall","FY", "Gender", "American Indian or Alaska Native","Asian","Black or African American","Hispanic","Native Hawaiian or other Pacifc Islander","White","Two or more Races","Nonresident Alien","Race/Ethnicity Unknown","Grand Total","Level of Student")
  
  setwd("Q:/Staff/President, Provost, Trustees/TAAC Dashboard/Peer Comparative Data/IPEDs Data Center/Compiled Survey Files from R")
  write.csv(dframe2, "Undergrad Fall Enrollment by Gender and RE.csv", row.names=FALSE)
  
  setwd("Q:/Staff/President, Provost, Trustees/TAAC Dashboard/Peer Comparative Data/IPEDs Data Center/R Output for Tableau/Dated Files")
  write.csv(dframe2, file = paste("Undergrad Fall Enrollment by Gender and RE",Sys.Date(), ".csv",sep=""),row.names = FALSE)
  
}
