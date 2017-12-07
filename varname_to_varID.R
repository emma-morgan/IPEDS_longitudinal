


#Sample script to compile vartable and valuesets longitudinal files

script_vartable <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/compile_varname_valueset.R", ssl.verifypeer = FALSE)
script_peerList <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/peerList.R", ssl.verifypeer = FALSE)
script_filename_to_tablename <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/filename_to_tablename.R", ssl.verifypeer = FALSE)
script_add_valuesets <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/add_valuesets.R", ssl.verifypeer = FALSE)
script_varnames_to_titles <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/change_varnames_to_vartitles.R", ssl.verifypeer = FALSE)
script_acadyear <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/acad_yr_function.R", ssl.verifypeer = FALSE)


eval(parse(text = script_vartable))
eval(parse(text = script_peerList))
eval(parse(text = script_filename_to_tablename))
eval(parse(text = script_add_valuesets))
eval(parse(text = script_varnames_to_titles))
eval(parse(text = script_acadyear))


rm("script_vartable","script_peerList","script_filename_to_tablename","script_add_valuesets","script_varnames_to_titles","pkg","pkgs")

valuesets_filepath <- "Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/Access Database/Compiling var info/R export/valuesets_compiled_rev.csv"
vartable_filepath <- "Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/Access Database/Compiling var info/R export/vartable_compiled_rev_EMedits.csv"

valuesets_full <- read.csv(valuesets_filepath,stringsAsFactors = F)
vartable_full <- read.csv(vartable_filepath,stringsAsFactors = F)
vartable_full$TABLE_TRIM <- table_from_column(vartable_full$TABLENAME)
ipeds_tables <- unique(vartable_full[,c("TABLE_TRIM", "SURVEY")])

#Admissions and Test Scores
survey_path <- "Q:\\Staff\\University-Wide\\Peer Comparison Database\\IPEDS\\Original IPEDS Data\\Admissions & Test Scores"

tableName <- table_from_file(paste(survey_path,"Data",sep="\\"))

ds_list <- list()

setwd(paste(survey_path,"Data",sep="\\"))

#Borrowed from Kathy's ipeds_rowbind.R - this should be functionalized
for (i in 1:length(list.files())) {
  fileName <- list.files()[i]
  ds <- read.csv(fileName, check.names=FALSE, stringsAsFactors = F, na.strings = c(".", "", " ", NA))
  names(ds) <- toupper(names(ds))
  #call function to trim dates out of csv filename -- create Table Name
  ds$TABLE_TRIM <- tableName
  #join "SURVEY" field in from ipeds_tables, and then un-factorize SURVEY
  ds <- dplyr::left_join(ds, ipeds_tables, by = "TABLE_TRIM")
  SURVEY <- as.character(first(ds$SURVEY))
  # call adacemic year function
  ay <- acad_year(fileName, SURVEY)
  ds$ACAD_YEAR <- ay
  #store each ds in a list
  ds_list[[as.character(ay)]] <- assign(paste("ds",i,sep=""), ds)
}


setwd(paste(survey_path,"Dictionary",sep="\\"))
dictionary_list <- list()

for (i in 1:length(list.files())) {
  fileName <- list.files()[i]
  if ("varlist" %in% excel_sheets(fileName)) {
      var_sheet <- "varlist"
  } else {
    paste("You must choose the varlist from the sheets below")  
    paste(excel_sheets(fileName))
    var_sheet <- readline("What is the name of the varlist sheet? (Type EXACT)")}
  ds <- read_excel(fileName, sheet=var_sheet, na = c(".", "", " ", NA))
  names(ds) <- toupper(names(ds))
  #call function to trim dates out of csv filename -- create Table Name
  ds$TABLE_TRIM <- tableName
  #join "SURVEY" field in from ipeds_tables, and then un-factorize SURVEY
  ds <- dplyr::left_join(ds, ipeds_tables, by = "TABLE_TRIM")
  SURVEY <- as.character(first(ds$SURVEY))
  # call adacemic year function
  ay <- acad_year(fileName, SURVEY)
  ds$ACAD_YEAR <- ay
  #store each ds in a list
  dictionary_list[[as.character(ay)]] <- assign(paste("ds",i,sep=""), ds)
}

