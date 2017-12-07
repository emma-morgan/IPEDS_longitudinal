#Function to create long and lean IPEDS file, compiled across years

IPEDS_long_lean <- function() {
  
  #Identify location of files to compile across years
  
  myUNITID <- readline(prompt = "Enter the UNITID of your institution of interest: ")
  
  mydir <- choose.dir(caption="Select directory location of your data files")
  setwd(mydir)
  
  ds_list <- list()
  
  #First set this up with a single file; then create the for loop
  #This is reading in each DATA file; we're going to need to see how the dictionary fits in...
  i <- 7
  
  fileName <- list.files()[i]
  ds <- read.csv(fileName, check.names=FALSE)
  
  #Still need to do some cleaning; get rid of imputation code columns, unnecessary data
  #Using code from IPEDS_clean_merge_survey_functions.R
  
  names(ds) <- toupper(names(ds))
  dropCol <- c()
  
  # below, we will Select rows for comparison institutions
  # this will use UNITID %in% list from peerFile$unitid
  
  # identify columns to delete (imputation & blank for Tufts)
  myInstRow <- which(ds[['UNITID']] == myUNITID)
  
  #We will only want to delete based on "blanks" if we have one row for Tufts;
  #If there are mutliple Tufts rows, then only remove imputation variables 
  
  # When there is only one row per institution, remove imputation rows
  # and any column for which Tufts (or your main institution) has a null value
  
  if (length(myInstRow) == 1) {
    myRow <- myInstRow
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
  #Possible for the future: May want to delete the row if ALL rows are null/blank, but this seems complicated without out much benefit
  #Leave for now
  
  else if (length(myInstRow > 1)) {
    for (k in 1:ncol(ds)) {
      if (substring(names(ds)[k],1,1) == "X") {
        dropCol <- c(dropCol,k)
      }
    }
  }
  
  # create a new ds with only the "good" rows and columns
  #No longer want to include only our peers, because we want a more comprehensive list;
    #May end up limiting based on header, but I'm thinking that it could be more efficient to just filter into the database
  ds_good <- ds[, -dropCol]
  
  # We now want to add columns for Fiscal Year, Fall, and Institution Name
  #UPDATE - Institution_Name will come from the Header file, not from this survey file
  #FY and Fall are still good to have
  #We will also want survey code and survey name
  # Create placeholders first
  ds_good['FY'] <- ""
  ds_good['Fall'] <- ""
  ds_good['surveyType'] <- ""
  ds_good['surveyName'] <- ""
  ds_good['OrigFileName'] <- ""

  # Add FY and "Fall" for joining purposes in Tableau
  # FY is based on the type of survey file and when data is collected
  # IPEDS_FY function is defined below
  
  #NEED TO CHANGE THIS - MADE ADJUSTMENTS TO IPEDS_FY FUNCTION
  # function now returns fall, FY, surveyName, surveyType, and fileName as named variables
  
  survey_details <- IPEDS_FY(fileName)
  
  ds_good['FY'] <- survey_details[['FY']]
  ds_good['Fall'] <- survey_details[['fall']]
  
  ds_good['surveyType'] <- survey_details[['surveyType']]
  ds_good['surveyName'] <- survey_details[['surveyName']]
  ds_good['OrigFileName'] <- survey_details[['filename']]

  #This is where we deviate...we need to now make it long and lean BEFORE compiling across years
  
  #Actually, maybe merge across years and then pivot? This is getting tricky, and I have 1/2 hour to continue working
  # before I move on to somethign else (likely less interesting)
  
  ds_list[[i]] <- assign(paste("ds",i,sep=""), ds_good)
  
  full_ds <- as.data.frame(rbind.fill.matrix(ds_list))
  
  #Need to add lookup dictionary for frequencies; this will also need to be compiled across years...
  
  #Should there be a lookup table?
  
  freq_table <- readline(prompt = "Is there a frequency lookup table? (TRUE or FALSE) ")
  
  #Now I need to change my lookup function to provide better descriptions of the variables...
  
  if (toupper(freq_table)==TRUE || toupper(freq_table)=="T") {
    lookup_dict <- choose.files(caption= cat("Survey type: ",survey_details[['surveyName']],"\n",
                                               "Select file with lookup dictionary for this survey",sep=""))
    full_ds <- lookup(full_ds, lookup_dict)
    
    "\t", LETTERS[1:3], "\n"
  }
  
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
  
  
  # Store the good info in our new list

  
}

  
  #Maybe I should have a function that's "tableau_long_lean" that basically reformats data?
  
  #Tableau_long_lean
    
    #Identify columns to repeat
  
    #iterate through and pivot to Tableau format
  
}