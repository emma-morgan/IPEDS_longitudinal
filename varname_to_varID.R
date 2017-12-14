

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


rm("script_vartable","script_peerList","script_filename_to_tablename",
   "script_add_valuesets","script_varnames_to_titles","pkg","pkgs","script_acadyear")

#valuesets_filepath <- "Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/Access Database/Compiling var info/R export/valuesets_compiled_rev.csv"
#vartable_filepath <- "Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/Access Database/Compiling var info/R export/vartable_compiled_rev_EMedits.csv"

# valuesets_full <- read.csv(valuesets_filepath,stringsAsFactors = F)
# vartable_full <- read.csv(vartable_filepath,stringsAsFactors = F)
# vartable_full$TABLE_TRIM <- table_from_column(vartable_full$TABLENAME)
# ipeds_tables <- unique(vartable_full[,c("TABLE_TRIM", "SURVEY")])


#Given your data location and survey name, this function will return
  # a unique compiled dictionary and a list of dictionary dfs indexed by AY

compile_dictionary_list <- function(IPEDS_data_location, surveyName) {
  
  setwd(paste(IPEDS_data_location, surveyName,"Dictionary",sep="\\"))
  tableName <- table_from_file(getwd(),1)
  dictionary_list <- list()
  
  for (i in 1:length(list.files())) {
    fileName <- list.files()[i]
    if ("varlist" %in% readxl::excel_sheets(fileName)) {
      var_sheet <- "varlist"
    } else {
      paste("You must choose the varlist from the sheets below")  
      paste(readxl::excel_sheets(fileName))
      var_sheet <- readline("What is the name of the varlist sheet? (Type EXACT)")}
    ds <- readxl::read_excel(fileName, sheet=var_sheet, na = c(".", "", " ", NA))
    names(ds) <- toupper(names(ds))
    #call function to trim dates out of csv filename -- create Table Name
    ds$TABLE_TRIM <- tableName

    # call adacemic year function
    ay <- acad_year(fileName, surveyName)
    ds[['ACAD_YEAR']] <- ay
    ds[['FILENAME']] <- fileName
    
    #Add VARIABLE_ID which will then be added to the data set to reduce confusion
    ds[["VARIABLE_ID"]] <- paste(ds[['VARNAME']],ds[['VARNUMBER']],sep="_")
    
    #store each ds in a list with year as name, so we can match with data
    dictionary_list[[as.character(ay)]] <- assign(paste("ds",i,sep=""), ds)
  }
  return(dictionary_list)
}


compile_dict_unique <- function(dictionary_list) {
  #Check to make sure we have necessary columns
  dictionary_full <- dplyr::bind_rows(dictionary_list)
  if (! all(c("VARNAME","VARTITLE","VARIABLE_ID","TABLE_TRIM","ACAD_YEAR","FILENAME") %in% names(dictionary_full))) {
    stop("vartable does not have required columns from dictionary files; please fix your data frame and try again")
  }
  
  #Sort so that we have the most recent year LAST; this will keep the most recent version of the variable
  dictionary_sorted <- data.table::setorder(dictionary_full,-ACAD_YEAR,VARIABLE_ID)
  
  dictionary_unique <- dictionary_sorted[!duplicated(dictionary_sorted['VARIABLE_ID'], fromLast = FALSE),]
  
  #Check to see if we have any repeated VARTITLE
  
  duplicate_vartitles <- which(duplicated(dictionary_unique['VARTITLE'], fromLast=FALSE) |
                                 duplicated(dictionary_unique['VARTITLE'], fromLast=TRUE))
  
  dictionary_unique[['VARTITLE_USE']] <- dictionary_unique[['VARTITLE']]

  if (length(duplicate_vartitles) > 0 ){
    dictionary_unique[['VARTITLE_USE']][duplicate_vartitles] <- sapply(duplicate_vartitles, 
                                                                       function(x) dictionary_unique[['VARTITLE_USE']][x] <- 
                                                                         paste(dictionary_unique[['VARTITLE_USE']][x]," 
                                                                        (",dictionary_unique[['VARNAME']][x],")",sep=""))
  }
  
  return (dictionary_unique)
}


#Within a single document, replace varname with varID
replace_VARNAME_VARIABLEID <- function(ds, dict) {
  
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
merge_IPEDS_data <- function (IPEDS_data_location,surveyName){
  
  dictionary_list <- compile_dictionary_list(IPEDS_data_location,surveyName)
  dictionary_unique <- compile_dict_unique(dictionary_list)
  
  setwd(paste(IPEDS_data_location, surveyName,"Data",sep="\\"))
  tableName <- table_from_file(getwd(),1)
  
  ds_list <- list()
  
  for (i in 1:length(list.files())) {
    fileName <- list.files()[i]
    ds_orig <- read.csv(fileName, check.names=FALSE, stringsAsFactors = F, na.strings = c(".", "", " ", NA))
    names(ds_orig) <- toupper(names(ds_orig))
    #Remove imputed variables
    ds_clean <- dplyr::select(ds_orig, -dplyr::starts_with("X"))
    # call adacemic year function
    ay <- acad_year(fileName, surveyName)
    #Convert VARNAME to VARIABLE_ID
    
    dict <- dictionary_list[[as.character(ay)]]
    
    ds <- replace_VARNAME_VARIABLEID(ds_clean,dict)
    ds[['ACAD_YEAR']] <- ay
    
    #call function to trim dates out of csv filename -- create Table Name
    ds$TABLE_TRIM <- tableName
    
    
    #store each ds in a list
    ds_list[[as.character(ay)]] <- assign(paste("ds",i,sep=""), ds)
  }
  
  #row bind the ds's together
  full_ds <- dplyr::bind_rows(ds_list)
  IPEDS_compiled <- list("data"=full_ds, "dictionary"=dictionary_unique)
  return(IPEDS_compiled)
  
}


########TEST###########################
#Admissions and Test Scores
IPEDS_data_location <- "Q:\\Staff\\University-Wide\\Peer Comparison Database\\IPEDS\\Original IPEDS Data"
surveyName <- 
IPEDS_test <- merge_IPEDS_data(IPEDS_data_location,surveyName)
IPEDS_data <- IPEDS_test$data
IPEDS_dictionary <- IPEDS_test$dictionary

#Possible Survey Names:

"Academic Libraries"
"Completions A"
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
"Student Financial Aid"






