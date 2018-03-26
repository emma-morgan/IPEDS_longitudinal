#Called by read_clean_data
#Dependencies: dplyr


#Within a single document, replace varname with varID
replace_varname_ID <- function(ds, dict) {
  
  vars <- dict$VARNAME[2:nrow(dict)]
  ds_new <- ds %>%
    dplyr::mutate(ROW_ID=1:nrow(ds)) %>%
    tidyr::gather("VARNAME","VALUE",!!vars)  %>%
    dplyr::left_join(dplyr::select(dict, "VARNAME","VARIABLE_ID")) %>%
    dplyr::select(-VARNAME) %>%
    tidyr::spread(key=VARIABLE_ID,value=VALUE) %>%
    dplyr::select(-ROW_ID)
  
  return(ds_new)
}
