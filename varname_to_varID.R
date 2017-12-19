

script_filename_to_tablename <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/filename_to_tablename.R", ssl.verifypeer = FALSE)

script_acadyear <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/acad_yr_function.R", ssl.verifypeer = FALSE)


eval(parse(text = script_filename_to_tablename))

eval(parse(text = script_acadyear))


rm("script_filename_to_tablename","script_acadyear")

#Given your data location and survey name, this function will return
  # a unique compiled dictionary and a list of dictionary dfs indexed by AY

compile_lookup_list <- function(IPEDS_data_location, sheetName) {
  
  setwd(paste(IPEDS_data_location, "Dictionary",sep="\\"))
  lookup_list <- list()
  
  for (i in 1:length(list.files())) {
    fileName <- list.files()[i]
    tableName <- table_from_file(getwd(),i)
    if (sheetName %in% readxl::excel_sheets(fileName)) {
      var_sheet <- sheetName
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
  lookup_full <- dplyr::bind_rows(lookup_list)
  if (sheetName == "varlist") {
    necessary_cols <- c("VARNAME","VARTITLE","VARIABLE_ID","TABLE_TRIM","ACAD_YEAR","FILENAME")
    lookup_col <- "VARIABLE_ID"
  }
  else if (sheetName=="Frequencies") {
    necessary_cols <- c("VARNAME","CODEVALUE","VALUELABEL","VARIABLE_ID","VALUESET_ID","TABLE_TRIM", "ACAD_YEAR","FILENAME")
    lookup_col <- "VALUESET_ID"
  }
  if (! all(necessary_cols %in% names(lookup_full))) {
    stop("lookup table does not have required columns from dictionary files; please fix your data frame and try again")
  }
  
  names(lookup_full)[which(names(lookup_full)==lookup_col)] <- "LOOKUP_ID"
  
  #Sort so that we have the most recent year LAST; this will keep the most recent version of the variable
  lookup_sorted <- data.table::setorder(lookup_full,-ACAD_YEAR,LOOKUP_ID)
  
  lookup_unique <- lookup_sorted[!duplicated(lookup_sorted['LOOKUP_ID'], fromLast = FALSE),]
  
  #Check if we have repeated descriptions (VARTITLE or VALUELABEL)
  
  if (sheetName=="varlist") {descr_col <- "VARTITLE"
  }
  else if (sheetName == "Frequencies") {descr_col <- "VALUELABEL"}
  
  duplicate_descr <- which(duplicated(lookup_unique[descr_col], fromLast=FALSE) |
                             duplicated(lookup_unique[descr_col], fromLast=TRUE))
  descr_use <- paste(descr_col,"USE",sep="_")
  lookup_unique[[descr_use]] <- lookup_unique[[descr_col]]
  
  if (sheetName=="varlist" & length(duplicate_descr) > 0 ){
    lookup_unique[[descr_use]][duplicate_descr] <- sapply(duplicate_descr, 
                                                          function(x) lookup_unique[[descr_use]][x] <- 
                                                            paste(lookup_unique[[descr_use]][x],"(",
                                                                  lookup_unique[['LOOKUP_ID']][x],")",sep=""))
  }
  names(lookup_unique)[which(names(lookup_full)=="LOOKUP_ID")] <- lookup_col
  return (lookup_unique)
}


#Within a single document, replace varname with varID
replace_varname_ID <- function(ds, dict) {
  
  vars <- dict$VARNAME[2:nrow(dict)]
  ds_new <- ds %>%
    dplyr::mutate(ROW_ID=1:nrow(ds)) %>%
    tidyr::gather("VARNAME","VALUE",!!vars) %>%
    dplyr::left_join(dplyr::select(dict, "VARNAME","VARIABLE_ID")) %>%
    dplyr::select(-VARNAME) %>%
    tidyr::spread(key=VARIABLE_ID,value=VALUE) %>%
    dplyr::select(-ROW_ID)
  
  return(ds_new)
}



#Borrowed from Kathy's ipeds_rowbind.R - this should be functionalized
merge_IPEDS_data <- function (IPEDS_data_location){
  
  dictionary_list <- compile_lookup_list(IPEDS_data_location=IPEDS_data_location, sheetName="varlist")
  dictionary_unique <- lookup_unique(dictionary_list, sheetName ="varlist")
  
  #Check to see if valuesets exist for this survey
  
  has_valueset <- sapply(list.files(paste(IPEDS_data_location,"Dictionary",sep="\\")), function(x) "Frequencies" %in% readxl::excel_sheets(x))
  if (all (! has_valueset)) {
    print("This survey does not have valuesets")
    valueset_unique <- NULL
  } else {
    valueset_list <- compile_lookup_list(IPEDS_data_location=IPEDS_data_location, sheetName="Frequencies")
    valueset_unique <- lookup_unique(valueset_list, sheetName="Frequencies")
  }
  setwd(paste(IPEDS_data_location, "Data",sep="\\"))
  
  
  ds_list <- list()
  
  for (i in 1:length(list.files())) {
    fileName <- list.files()[i]
    tableName <- table_from_file(getwd(),i)
    ds_orig <- read.csv(fileName, check.names=FALSE, stringsAsFactors = F, na.strings = c(".", "", " ", NA))
    names(ds_orig) <- toupper(names(ds_orig))
    #Remove imputed variables
    ds_clean <- dplyr::select(ds_orig, -dplyr::starts_with("X"))
    # call adacemic year function
    ay <- acad_year(fileName, tableName)
    #Convert VARNAME to VARIABLE_ID
    
    dict <- dictionary_list[[as.character(ay)]]
    
    ds <- replace_varname_ID(ds_clean,dict)
    ds[['ACAD_YEAR']] <- ay
    ds[['FILE_NAME']] <- fileName
    
    #call function to trim dates out of csv filename -- create Table Name
    ds$TABLE_TRIM <- tableName
    
    
    #store each ds in a list
    ds_list[[as.character(ay)]] <- assign(paste("ds",i,sep=""), ds)
  }
  
  #row bind the ds's together
  
  #dplyr::bind_rows was giving issues when we had mixed TYPE (e.g. some characters and some double)
  #full_ds <- dplyr::bind_rows(ds_list)
  
  full_ds <- data.table::rbindlist(ds_list, fill=TRUE)
  
  IPEDS_compiled <- list("data"=full_ds, "dictionary"=dictionary_unique, "valuesets"=valueset_unique)
  return(IPEDS_compiled)
  
}