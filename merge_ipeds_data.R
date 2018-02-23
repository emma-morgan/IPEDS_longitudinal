#Merge IPEDS Data
#This is the wrapper function that will compile IPEDS data
#Borrowed from Kathy's ipeds_rowbind.R file 

#Inputs: IPEDS_data_location (filepath to Data & Dictionary folders)
        #peer_filpath (filepath to peer .csv; possible to make this two variables
          # the first tells what kind of peer list (.csv, df in R, none))
          # and the second will give you the filepath to a .csv of the name of a df in R
          # I don't know whether the peer UNITID list should be created in this merge function
          # I think maybe people need to instead create a dataframe or a .csv earlier in the process
              #and then call it here

#Output
  # IPEDS_compiled <- list("data"=full_ds, "dictionary"=dictionary_unique, "valuesets"=valueset_unique)

#Borrowed from Kathy's ipeds_rowbind.R - this should be functionalized
merge_IPEDS_data <- function (IPEDS_data_location, peer_filepath){
  
  if (exists ("peer_filepath")) {
    peerList <- IPEDS_peers_from_file(peer_filepath)
    peerUNITIDs <- peerList$peers_for_IPEDS
  }
  
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
    
    #subset to peer list
    if (exists("peerUNITIDs")) {
      ds_clean <- dplyr::filter(ds_clean, UNITID %in% peerUNITIDs)
    }
    
    # call adacemic year function
    ay <- acad_year(fileName, tableName)
    #Convert VARNAME to VARIABLE_ID
    
    dict <- dictionary_list[[as.character(ay)]]
    #Issue with dictionary showing up with NA row...need to figure this out!
    dict <- dplyr::filter(dict, !(is.na(VARNUMBER)))
    
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