#Compile longtiudinal varTable and valueSets documents
#Created by Emma Morgan (emma.morgan@tufts.edu)
#This assumes that individuals have downloaded the lookup tables/dictionaries available to download from IPEDS in full
# as part of the Access database download



#Located all files in a central location:

varlookup_dir <- "Q:\\Staff\\University-Wide\\Peer Comparison Database\\IPEDS\\Access Database\\Compiling var info"
#varlookup_dir <- choose.dir()

varlookup_filenames <- list.files(varlookup_dir)
#Check to make sure that all files are of the name and type we need
#   should be IPEDS____TablesDoc

IPEDS_tablesdoc_files <- varlookup_filenames[which (grepl("IPEDS",varlookup_filenames,ignore.case = TRUE) &
        grepl("tablesdoc",varlookup_filenames,ignore.case = TRUE))]

tablesdoc_filepaths <- lapply(IPEDS_tablesdoc_files,function(x) paste(varlookup_dir,x,sep="\\"))


#Create a function that will add in the academic year from the filename of the variable table"
#Since this is from the tablesdoc only, we've standardized index 6-11 as the year indices;
  #it may be worth it to standardize this for use with more files

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
  

vartables_list <- lapply(tablesdoc_filepaths,function(x) get_sheet(x,"vartable"))

valuesets_list <- lapply(tablesdoc_filepaths,function(x) get_sheet(x,"valuesets"))
#Warning when we read in valuesets_list; may need to look at this later?

#  In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
#                  Expecting numeric in J11307 / R11307C10: got a date
#  In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
#                  Expecting numeric in J11308 / R11308C10: got a date
#  In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
#                  Expecting numeric in J11309 / R11309C10: got a date
#  In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
#                  Expecting numeric in J11310 / R11310C10: got a date


AcadYear_list <- lapply(IPEDS_tablesdoc_files,ay_from_tablesdoc)
names(vartables_list) <- AcadYear_list
names(valuesets_list) <- AcadYear_list

#Add AY to each table; can't figure out a better way to do this;
#   This could be incorporated when reading in the file, but I don't want to mess with "get_sheets" right now

for (i in 1:length(AcadYear_list)) {
  vartables_list[[i]]['ACADYEAR'] <- names(vartables_list)[[i]]
  valuesets_list[[i]]['ACADYEAR'] <- names(valuesets_list)[[i]]
}



vartable_compiled <- dplyr::bind_rows(vartables_list)
valuesets_compiled <- dplyr::bind_rows(valuesets_list)

vartable_compiled[['variable_id']] <- paste(vartable_compiled[['varName']],vartable_compiled[['varNumber']],sep="_")
valuesets_compiled[['valueset_id']] <- paste(valuesets_compiled[['varName']],valuesets_compiled[['varNumber']],valuesets_compiled[['Codevalue']],sep="_")

vartable_distinct <- vartable_compiled[!duplicated(vartable_compiled['variable_id'], fromLast = TRUE),]
valuesets_distinct <- valuesets_compiled[!duplicated(valuesets_compiled['valueset_id'], fromLast = TRUE),]

#Make column names all uppercase for CONSISTENCY
names(vartable_distinct) <- toupper(names(vartable_distinct))
names(valuesets_distinct) <- toupper(names(valuesets_distinct))

#Choose directory to write out files; this can also be specified explicitly
outputdir <- choose.dir()
##For Emma:  outputdir <- "Q:\\Staff\\University-Wide\\Peer Comparison Database\\IPEDS\\Access Database\\Compiling var info\\R export"

write.csv(vartable_distinct,paste(outputdir,"vartable_compiled_2.csv",sep="\\"), row.names=FALSE,na="")
write.csv(valuesets_distinct,paste(outputdir,"valuesets_compiled2.csv",sep="\\"), row.names=FALSE,na="")



#***************************************************************
# Create unique vartitle in vartable compiled for our eventual lookup
#***************************************************************

#Start with a compiled vartable_distinct file

vartable <- vartable_distinct

#Get a list of the table numbers; these are what we need to go through
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

vartable_goodNames <- dplyr::bind_rows(var_by_table)

outputdir <- "Q:\\Staff\\University-Wide\\Peer Comparison Database\\IPEDS\\Access Database\\Compiling var info\\R export"

write.csv(vartable_goodNames,paste(outputdir,"vartitles_unique_within_table.csv",sep="\\"),row.names=FALSE,na="")
