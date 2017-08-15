#add valuesets

add_values <- function(longtable, valueset, ignore_size_warning=F) {
  if (any(is.na(valueset$CODEVALUE))) {stop("Missing code values. valueset must contain CODEVALUE to proceed.")} else
    if (is.character(valueset$CODEVALUE)){warning("Coercing code value to character.")
      valueset$CODEVALUE = as.character(valueset$CODEVALUE)}
  
  if(!ignore_size_warning&nrow(longtable)>50000){stop("Large file may break things, consider using subset_peerlist() to reduce file size. Set ignore_size_warning=T to override this error.")}
  if(ignore_size_warning){warning("Large file may break things, consider using subset_peerlist() to reduce file size and compile time.")}
  
  ds <- longtable
    
    #'because some institutions have multiple values in one value set for same year need to add a unique row id for spread to work
    ds <- dplyr::mutate(ds, ROW_ID = 1:nrow(longtable))
    
    #' select only the variables in small valueset to gather
    
    ds <- tidyr::gather(ds, "VARNAME", "CODEVALUE", names(longtable)[names(longtable)%in%valueset$VARNAME]) 
    ds <- dplyr::mutate(ds, CODEVALUE = as.character(CODEVALUE))
    
    #' left join long table to small valueset
    
    ds <- dplyr::left_join(ds, select(valueset,
                            "VARNAME", 
                            "CODEVALUE", 
                            "VALUELABEL")) 
    
    #' paste variable value numeric with variable value description into a new column crazy separater hopefully never in description
    
    ds <- dplyr::mutate(ds, VALUE_CODE_LABEL = 
                    paste0(CODEVALUE, "/_/", VALUELABEL))
    #' remove CODEVALUE and VALUELABEL for spread to function correctly
    ds <- dplyr::select(ds, -CODEVALUE, -VALUELABEL)
    
    #' spread on variable fill in with var number desc
    ds <- tidyr::spread(ds, key = VARNAME, value = VALUE_CODE_LABEL)
    
    #' remove row id
    dplyr::select(-ROW_ID)
  
  #' separate the value from the description remove the original gross one
  
  for(name in names(ds)[names(ds)%in%valueset$VARNAME]) ds <- tidyr::separate_(ds, name,sep = "/_/", into = c(paste0(name, "_value"), name), remove = T)
  
  #' return dataset  
  return(ds)
    
}

#VARNAMES TO VARTITLES
change_varnames_vartitles <- function(longtable, varnames, vars, ignore_size_warning = F) {
  if(!ignore_size_warning&nrow(longtable)>50000){stop("Large file may break things, consider using subset_peerlist() to reduce file size. Set ignore_size_warning=T to override this error.")}
  if(ignore_size_warning){warning("Large file may break things, consider using subset_peerlist() to reduce file size and compile time.")}
  
  ds <- longtable 
    
    #'because of multiple years need to add a unique row id for spread to work
    ds <- dplyr::mutate(ds,ROW_ID = 1:nrow(longtable))
    
    #' gather long table with all variables that need to be changed
    ds <- tidyr::gather(ds, "VARNAME", "VALUE", vars)  
    
    #' separate the _value temporarliy
    ds <- tidyr::separate(ds, VARNAME, into = c("VARNAME", "extra_temp"), sep = "_", remove=T) 
    
    #' left join with clean var title
    ds <- dplyr::left_join(ds, select(varnames,
                            "VARNAME",
                            "VARTITLE_USE")) 
    
    #' re paste the _value column
    ds <- dplyr::mutate(ds, VARTITLE_CLEAN = ifelse(!is.na(extra_temp), 
                                          paste0(VARTITLE_USE, "_", extra_temp), VARTITLE_USE))   
    #'remove unneeded variables for spread to be happy
    ds <- dplyr::select(ds, -VARTITLE_USE, -extra_temp, -VARNAME)
    
    #' rename vartitle_clean to be consistant
    ds <- dplyr::rename(ds, VARTITLE = VARTITLE_CLEAN) 
    
    #' spread to make dataset wide again
    ds <- tidyr::spread(ds, key = VARTITLE, value = VALUE)
    
    #' remove row id
    ds <- dplyr::select(ds, -ROW_ID)
  
  #' return dataset
  return(ds)
}
    

  