#Created by Emma Morgan
#2/26/2018
#Updated 4/2/2018

# There are 3 files that are part of this. They were previously part of the varname_to_varID.R script,
#  but now are being maintained in this file. Ideally, anything involing the lookup functions can be part of this script.
# 
# The 2 functions loaded by this script are as follows:
#   1. compile_lookup_list
#   2. lookup_unique
#   
# compile_lookup_list requires functions acad_year and table_from_file. These are part of the filename_to_tablename.R
#   and acadyear.R, which we can see are loaded before running these.
#   
# 
#   

script_filename_to_tablename <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/filename_to_tablename.R",
                                              ssl.verifypeer = FALSE)
script_acadyear <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/acad_yr_function.R",
                                 ssl.verifypeer = FALSE)
eval(parse(text = script_filename_to_tablename))
eval(parse(text = script_acadyear))
rm("script_filename_to_tablename","script_acadyear")

#Given your data location and survey name, this function will return
# a unique compiled dictionary and a list of dictionary dfs indexed by AY

compile_lookup_list <- function(IPEDS_data_location, sheetName) {
  
  setwd(paste(IPEDS_data_location, "Dictionary",sep="/"))
  lookup_list <- list()
  
  for (i in 1:length(list.files())) {
    fileName <- list.files()[i]
    tableName <- table_from_file(getwd(),i)
    if (sheetName %in% readxl::excel_sheets(fileName)) {
      var_sheet <- sheetName
    } else if (tolower(sheetName) %in% tolower(readxl::excel_sheets(fileName))){
      sheetNames <- readxl::excel_sheets(fileName)
      sheetIndex <- which(tolower(sheetName)==tolower(sheetNames))
      var_sheet <- sheetNames[[sheetIndex]]
    } else {
      print(paste("No sheet ",sheetName," in file ",fileName,sep=""))
      next}
    ds <- readxl::read_excel(fileName, sheet=var_sheet, na = c(".", "", " ", NA))
    names(ds) <- toupper(names(ds))
    #call function to trim dates out of csv filename -- create Table Name
    ds$TABLE_TRIM <- tableName
    
    # call academic year function
    ay <- acad_year(fileName, tableName)
    ds[['ACAD_YEAR']] <- ay
    ds[['FILENAME']] <- fileName
    
    #Add VARIABLE_ID or VALUESET_ID which will then be added to the data set to reduce confusion
    if (sheetName == "varlist") {
      ds[["VARIABLE_ID"]] <- paste(ds[['VARNAME']],ds[['VARNUMBER']],sep="_")
    } else if (sheetName == "Frequencies") {
      ds[['VARIABLE_ID']] <- paste(ds[['VARNAME']],ds[['VARNUMBER']],sep="_")
      ds[['VALUESET_ID']] <- paste(ds[['VARNAME']],ds[['VARNUMBER']],ds[['CODEVALUE']],sep="_")
    }  
    
    #store each ds in a list with year as name, so we can match with data
    lookup_list[[as.character(ay)]] <- assign(paste("ds",i,sep=""), ds)
  }
  return(lookup_list)
}


lookup_unique <- function(lookup_list, sheetName) {
  #Check to make sure we have necessary columns
  lookup_full <- data.table::rbindlist(lookup_list, fill=TRUE)
  if (sheetName == "varlist") {
    necessary_cols <- c("VARNAME","VARTITLE","VARIABLE_ID","TABLE_TRIM","ACAD_YEAR","FILENAME")
    lookup_col <- "VARIABLE_ID"
  } else if (sheetName=="Frequencies") {
    necessary_cols <- c("VARNAME","CODEVALUE","VALUELABEL","VARIABLE_ID","VALUESET_ID","TABLE_TRIM", "ACAD_YEAR","FILENAME")
    lookup_col <- "VALUESET_ID"
  }
  if (! all(necessary_cols %in% names(lookup_full))) {
    stop("lookup table does not have required columns from dictionary files; please fix your data frame and try again")
  }
  
  names(lookup_full)[which(names(lookup_full)==lookup_col)] <- "LOOKUP_ID"
  
  #Sort so that we have the most recent year LAST; this will keep the most recent version of the variable
  lookup_sorted <- data.table::setorder(lookup_full,-ACAD_YEAR,LOOKUP_ID)
  
  lookup_unique <- lookup_sorted[!duplicated(lookup_sorted[['LOOKUP_ID']], fromLast = FALSE),]
  
  #Check if we have repeated descriptions (VARTITLE or VALUELABEL)
  
  if (sheetName=="varlist") {descr_col <- "VARTITLE"
  } else if (sheetName == "Frequencies") {descr_col <- "VALUELABEL"}
  
  duplicate_descr <- which(duplicated(lookup_unique[[descr_col]], fromLast=FALSE) |
                             duplicated(lookup_unique[[descr_col]], fromLast=TRUE))
  descr_use <- paste(descr_col,"USE",sep="_")
  lookup_unique[[descr_use]] <- lookup_unique[[descr_col]]
  
  if (sheetName=="varlist" & length(duplicate_descr) > 0 ){
    lookup_unique[[descr_use]][duplicate_descr] <- sapply(duplicate_descr, 
                                                          function(x) lookup_unique[[descr_use]][x] <- 
                                                            paste(lookup_unique[[descr_use]][x],"(",
                                                                  lookup_unique[['LOOKUP_ID']][x],")",sep=""))
  }
  names(lookup_unique)[which(names(lookup_full)=="LOOKUP_ID")] <- lookup_col
  
  #Replace stated academic years to year indices in the vartitles; this currently is necessary
    #only for Student Financial Aid (SFA) and Student Charges (IC_AY)
  if (sheetName == "varlist" & unique(lookup_unique$TABLE_TRIM) %in% c("SFA", "IC_AY")) {
    lookup_unique <- dictionary_years_to_index(lookup_unique)
  }
  
  
  return (lookup_unique)
}
