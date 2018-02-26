#Created by Emma Morgan
#2/26/2018
#Part of IPEDS World Domination project


##This is the function that will actually read our IPEDS data file in
##Rather than having a loop in merge_IPEDS_data with all the steps,
##    this will allow us to call a single function and take care of each file

read_clean_data <- function(IPEDS_data_location_DATA, i, dictionary_list){
  
  fileName <- list.files(path=IPEDS_data_location_DATA)[i]
  tableName <- table_from_file(IPEDS_data_location_DATA,i)
  ds_orig <- readr::read_csv(paste(IPEDS_data_location_DATA,fileName, sep="\\"), col_types = cols(.default = "c"), 
                             na = c(".", "", " ", NA))
  names(ds_orig) <- toupper(names(ds_orig))
  #Remove imputed variables
  ds_clean <- dplyr::select(ds_orig, -dplyr::starts_with("X"))
  
  #subset to peer list
  if (exists("peer_UNITIDs")) {
    ds_clean <- dplyr::filter(ds_clean, UNITID %in% peer_UNITIDs)
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
  ds[['TABLE_TRIM']] <- tableName
  
  IPEDS_data_clean <- list("data"=ds, "ay"=ay)
  print(paste("Returning clean",fileName))
  #return ds with academic year for naming conventions
  return(IPEDS_data_clean)
}