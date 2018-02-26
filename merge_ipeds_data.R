#Merge IPEDS Data
#This is the wrapper function that will compile IPEDS data
#Borrowed from Kathy's ipeds_rowbind.R file 

#Inputs: IPEDS_data_location (filepath to Data & Dictionary folders)
        #peer_UNITIDs (list of UNITIDs for the peer group; should also be able to be a single UNITID)
        #Kate is writing functions that will allow user to either read in a peer file (.csv or xlsx)
        # or generate a list of peers using the header characteristics file

#Output
  # IPEDS_compiled <- list("data"=full_ds, "dictionary"=dictionary_unique, "valuesets"=valueset_unique)

#Borrowed from Kathy's ipeds_rowbind.R - this should be functionalized
merge_IPEDS_data_NEW <- function (IPEDS_data_location, peerUNITIDs){
  
  if (exists ("peer_UNITIDs")) {
    n_peers <- length(peer_UNITIDs)
    if (n_peers==1) {
      paste("You have included one institution:",peer_UNITIDs)
    } else if (n_peers >1) {
      print(paste("You have included a peer list with",n_peers,"members."))
    }
  } else {print("You have chosen not to include a peer list. Consider subsetting your data to save time and memory.")}
  
  dictionary_list <- compile_lookup_list(IPEDS_data_location=IPEDS_data_location, sheetName="varlist")
  dictionary_unique <- lookup_unique(dictionary_list, sheetName ="varlist")
  
  #Check to see if valuesets exist for this survey
  
  has_valueset <- sapply(list.files(paste(IPEDS_data_location,"Dictionary",sep="\\")), function(x) "Frequencies" %in% readxl::excel_sheets(x))
  if (all (! has_valueset)) {
    print("This survey does not have valuesets")
    valueset_unique <- NULL
  } else {
    print ("Compiling valuesets")
    valueset_list <- compile_lookup_list(IPEDS_data_location=IPEDS_data_location, sheetName="Frequencies")
    valueset_unique <- lookup_unique(valueset_list, sheetName="Frequencies")
  }
  IPEDS_data_location_DATA <- (paste(IPEDS_data_location, "Data",sep="\\"))
  
  
  ds_list <- list()
  
  for (i in 1:length(list.files(path=IPEDS_data_location_DATA))) {
    
    IPEDS_data_clean <- read_clean_data(IPEDS_data_location_DATA, i, dictionary_list)

    #store each ds in a list
    ds_list[[as.character(IPEDS_data_clean[['ay']])]] <- assign(paste("ds",i,sep=""), IPEDS_data_clean[['data']])
  }  

  #row bind the ds's together
  
  #dplyr::bind_rows was giving issues when we had mixed TYPE (e.g. some characters and some double)
  #full_ds <- dplyr::bind_rows(ds_list)
  
  full_ds <- data.table::rbindlist(ds_list, fill=TRUE)
  
  IPEDS_compiled <- list("data"=full_ds, "dictionary"=dictionary_unique, "valuesets"=valueset_unique)
  return(IPEDS_compiled)
  
}