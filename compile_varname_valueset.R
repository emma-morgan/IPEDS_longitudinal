#Compile longtiudinal varTable and valueSets documents
#Created by Emma Morgan (emma.morgan@tufts.edu)
  #as part of the IPEDS World Domination collaboration between Tufts University and Smith College
  #August 10-11 2017
#This assumes that individuals have downloaded the lookup tables/dictionaries available to download from IPEDS in full
# as part of the Access database download


#Functionalize compilation

#Function to pull academic year form tablesdoc file name
ay_from_tablesdoc <- function(filename) {
  if (! (grepl("IPEDS",filename,ignore.case=TRUE) & grepl("tablesdoc",filename,ignore.case=TRUE))){
    print.warnings(paste("One or more files is not a valid IPEDS_TablesDoc download:",filename,sep=" "))
  }
  else {
    first_digit <- regexpr("\\d{6}",filename)[[1]]
    AcadYear <- paste(substr(filename,first_digit,first_digit+3),substr(filename,first_digit+4,first_digit+5),sep="-")
  }
}

#General function to read in a sheet from a file name given a path and a string we're looking for in the name
get_sheet <- function(filepath,sheetname){
  sheet_index <- which(grepl(sheetname,readxl::excel_sheets(filepath),ignore.case=TRUE))
  if (length(sheet_index)==1) {sheet_df <- readxl::read_excel(path=filepath,sheet=sheet_index)}
  else {print.warnings(paste("The following file does not contain unique sheet ",sheetname,": ",filepath,sep=""))}
  return(sheet_df)
}


#Function that will add vartitle_USE to our compiled variable title
  #This will be used in the function that compiled vartable and valuesets to create a useable longitudinal file
  #Can also be used on a single year vartable

vartable_unique_titles <- function(vartable) {
  
  #First, check to make sure we have the columns we need;
    #If not, return an error and let the user know what is wrong
  
  if (! all(c('TABLENUMBER','VARNAME','VARTITLE') %in% names(vartable))) {
    stop("vartable does not have required columns(TABLENUMBER, VARNAME, VARTITLE); please fix your data frame and try again")
    
  }
  
  #Get a list of the table numbers; these are what we need to go through
  #Check to make sure that the vartable compiled has a 
  list_of_tables <- names(table(vartable[['TABLENUMBER']]))
  
  var_by_table <- lapply(list_of_tables, function(x) vartable[which(vartable['TABLENUMBER']==x),])
  names(var_by_table) <- list_of_tables
  
  for (t in names(var_by_table)){
    table_vars <- var_by_table[[t]]
    duplicate_vartitles <- which(duplicated(table_vars['VARTITLE'], fromLast = FALSE) | duplicated(table_vars['VARTITLE'], fromLast = TRUE))
    table_vars[['VARTITLE_USE']] <- table_vars[['VARTITLE']]
    if (length(duplicate_vartitles) > 0 ){
      table_vars[['VARTITLE_USE']][duplicate_vartitles] <- sapply(duplicate_vartitles, 
                                                                  function(x) table_vars[['VARTITLE_USE']][x] <- 
                                                                    paste(table_vars[['VARTITLE_USE']][x]," (",table_vars[['VARNAME']][x],")",sep=""))
    }
    var_by_table[[t]] <- table_vars
  }
  
  vartable_uniqueTitles <- dplyr::bind_rows(var_by_table)
  
  return(vartable_uniqueTitles)
}

compile_vartable_valuesets <- function(varlookup_dir) {
  
  #Check if the varlookup_dir has been specified; if not, ask user for the lookup
  if (missing("varlookup_dir")) {
    varlookup_dir <- choose.dir(caption = "Select folder with the IPEDS_TablesDoc files")
  }
  
  #Get a list of files in this folder
  varlookup_filenames <- list.files(varlookup_dir)
  
  #Check to make sure that all files are of the name and type we need; keep only those files
  #   should be IPEDS____TablesDoc
  
  IPEDS_tablesdoc_files <- varlookup_filenames[which (grepl("IPEDS",varlookup_filenames,ignore.case = TRUE) &
                                                        grepl("tablesdoc",varlookup_filenames,ignore.case = TRUE))]
  
  tablesdoc_filepaths <- lapply(IPEDS_tablesdoc_files,function(x) paste(varlookup_dir,x,sep="\\"))

  vartables_list <- lapply(tablesdoc_filepaths,function(x) get_sheet(x,"vartable"))
  
  valuesets_list <- lapply(tablesdoc_filepaths,function(x) get_sheet(x,"valuesets"))
  #Warning when we read in valuesets_list; may need to look at this later?

  AcadYear_list <- lapply(IPEDS_tablesdoc_files,ay_from_tablesdoc)
  names(vartables_list) <- AcadYear_list
  names(valuesets_list) <- AcadYear_list
  
  #Add AY to each table; can't figure out a better way to do this;
  #   This could be incorporated when reading in the file, but I don't want to mess with "get_sheets" right now
  
  for (i in 1:length(AcadYear_list)) {
    vartables_list[[i]]['ACADYEAR'] <- names(vartables_list)[[i]]
    valuesets_list[[i]]['ACADYEAR'] <- names(valuesets_list)[[i]]
  }
  
  #Compile the longitudinal versions of vartable and valuesets
  vartable_compiled <- dplyr::bind_rows(vartables_list)
  valuesets_compiled <- dplyr::bind_rows(valuesets_list)
  
  #Want to only keep one line per variable or valueset; a variable is specified by variable name and number
    #valueset is specified by varaible name, number, and codevalue
  vartable_compiled[['variable_id']] <- paste(vartable_compiled[['varName']],vartable_compiled[['varNumber']],sep="_")
  valuesets_compiled[['valueset_id']] <- paste(valuesets_compiled[['varName']],valuesets_compiled[['varNumber']],valuesets_compiled[['Codevalue']],sep="_")
  
  #Delete duplicates based on the specification for identifying variable and valueset; 
    #Use fromLast = TRUE to keep the most recent year's variable name
  vartable_distinct <- vartable_compiled[!duplicated(vartable_compiled['variable_id'], fromLast = TRUE),]
  valuesets_distinct <- valuesets_compiled[!duplicated(valuesets_compiled['valueset_id'], fromLast = TRUE),]
  
  #Make column names all uppercase for CONSISTENCY
  names(vartable_distinct) <- toupper(names(vartable_distinct))
  names(valuesets_distinct) <- toupper(names(valuesets_distinct))
  
  #Add in vartitle_USE for unique variable titles within a given table
  vartable_distinct <- vartable_unique_titles(vartable_distinct)
  
  return(list("vartable_compiled" = vartable_distinct, "valuesets_compiled" = valuesets_distinct))
}
