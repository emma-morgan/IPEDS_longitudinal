

#Sample script to compile vartable and valuesets longitudinal files
#Can we do this without compiling the vartable/valuesets at the beginning?

#script_vartable <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/compile_varname_valueset.R", ssl.verifypeer = FALSE)
#script_peerList <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/peerList.R", ssl.verifypeer = FALSE)
script_filename_to_tablename <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/filename_to_tablename.R", ssl.verifypeer = FALSE)
#script_add_valuesets <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/add_valuesets.R", ssl.verifypeer = FALSE)
#script_varnames_to_titles <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/change_varnames_to_vartitles.R", ssl.verifypeer = FALSE)
script_acadyear <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/acad_yr_function.R", ssl.verifypeer = FALSE)


#eval(parse(text = script_vartable))
#eval(parse(text = script_peerList))
eval(parse(text = script_filename_to_tablename))
#eval(parse(text = script_add_valuesets))
#eval(parse(text = script_varnames_to_titles))
eval(parse(text = script_acadyear))


rm("script_filename_to_tablename","script_acadyear")

#valuesets_filepath <- "Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/Access Database/Compiling var info/R export/valuesets_compiled_rev.csv"
#vartable_filepath <- "Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/Access Database/Compiling var info/R export/vartable_compiled_rev_EMedits.csv"

# valuesets_full <- read.csv(valuesets_filepath,stringsAsFactors = F)
# vartable_full <- read.csv(vartable_filepath,stringsAsFactors = F)
# vartable_full$TABLE_TRIM <- table_from_column(vartable_full$TABLENAME)
# ipeds_tables <- unique(vartable_full[,c("TABLE_TRIM", "SURVEY")])


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
      print(paste("You must choose the ",sheetName," from the sheets below",sep=""))  
      paste(readxl::excel_sheets(fileName))
      var_sheet <- readline("What is the name of the ",sheetName," sheet? (Type EXACT)")}
    ds <- readxl::read_excel(fileName, sheet=var_sheet, na = c(".", "", " ", NA))
    names(ds) <- toupper(names(ds))
    #call function to trim dates out of csv filename -- create Table Name
    ds$TABLE_TRIM <- tableName

    # call adacemic year function
    ay <- acad_year(fileName, tableName)
    ds[['ACAD_YEAR']] <- ay
    ds[['FILENAME']] <- fileName
    
    #Add VARIABLE_ID or VALUESET_ID which will then be added to the data set to reduce confusion
    if (sheetName == "varlist") {
      ds[["LOOKUP_ID"]] <- paste(ds[['VARNAME']],ds[['VARNUMBER']],sep="_")
    } else if (sheetName == "Frequencies") {
      ds[['LOOKUP_ID']] <- paste(ds[['VARNAME']],ds[['VARNUMBER']],ds[['CODEVALUE']],sep="_")
    }  
    
    #store each ds in a list with year as name, so we can match with data
    #XX this might be problematic if survey has multiple files per year??? How often does that happen??
    lookup_list[[as.character(ay)]] <- assign(paste("ds",i,sep=""), ds)
  }
  return(lookup_list)
}


lookup_unique <- function(lookup_list, sheetName) {
  #Check to make sure we have necessary columns
  lookup_full <- dplyr::bind_rows(lookup_list)
  if (sheetName == "varlist") {
    necessary_cols <- c("VARNAME","VARTITLE","LOOKUP_ID","TABLE_TRIM","ACAD_YEAR","FILENAME")
  }
  else if (sheetName=="Frequencies") {
    necessary_cols <- c("VARNAME","CODEVALUE","VALUELABEL","LOOKUP_ID","TABLE_TRIM", "ACAD_YEAR","FILENAME")
  }
  if (! all(necessary_cols %in% names(lookup_full))) {
    stop("lookup table does not have required columns from dictionary files; please fix your data frame and try again")
  }
  
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
  
  return (lookup_unique)
}


#Within a single document, replace varname with varID
replace_varname_ID <- function(ds, dict) {
  
  vars <- dict$VARNAME[2:nrow(dict)]
  ds_new <- ds %>%
    dplyr::mutate(ROW_ID=1:nrow(ds)) %>%
    tidyr::gather("VARNAME","VALUE",!!vars) %>%
    dplyr::left_join(dplyr::select(dict, "VARNAME","LOOKUP_ID")) %>%
    dplyr::select(-VARNAME) %>%
    tidyr::spread(key=LOOKUP_ID,value=VALUE) %>%
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


########TEST###########################
#Admissions and Test Scores
#surveyFolder <- 
IPEDS_data_location_general <- "Q:\\Staff\\University-Wide\\Peer Comparison Database\\IPEDS\\Original IPEDS Data"
IPEDS_data_location <- paste(IPEDS_data_location_general,surveyFolder, sep="\\")
IPEDS_test <- merge_IPEDS_data(IPEDS_data_location)
IPEDS_data <- IPEDS_test$data
IPEDS_dictionary <- IPEDS_test$dictionary
IPEDS_valuesets <- IPEDS_test$valuesets

#Possible Survey Names:

"Academic Libraries" - works
"Completions A" - checked
"Completions B"
"Completions c"
"Directory Information"
"Employees by Assigned Position"
"Fall Enrollment A"
"Fall Enrollment B"
"Fall Enrollment C"
"Fall Enrollment CP"
"Fall Enrollment D"
"Fall Staff IS"
"Fall Staff NH"
"Fall Staff OC"
"Fall Staff SIS"
"Finance F2"
"Graduation Rates"
"Institutional Characteristics"
"Instructional Staff Salaries IS"
"Instructional Staff Salaries NIS"
"Student Charges (IC AY)"
"Student Financial Aid" - checked






