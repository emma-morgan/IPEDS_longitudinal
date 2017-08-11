#' Goal: Add value labels to longitudinal file when value labels exist.
#' Contains function input dataset longitudinal table file and valueset file output  dataset longitudinal table file with value labels 
#' KMA - 8/10/17

#' subset valueset to only contain variables from the lonitudinal table 
# XXX SHOULD THIS BE IN THIS FUNCTION??


add_values <- function(longtable, valueset) {
  if (any(is.na(valueset$CODEVALUE))) {stop("Missing code values")} else
    if (is.character(valueset$CODEVALUE)){warning("Coercing code value to integer.")
      valueset$CODEVALUE = as.integer(valueset$CODEVALUE)}
  
  ds <- longtable %>%
  
  #'because some institutions have multiple values in one value set for same year need to add a unique row id for spread to work XX SHOULD THIS HAPPEN HERE????
   # mutate(row_id = 1:nrow(longtable)) %>% 
  
  #' select only the variables in small valueset to gather
    
    gather("VARNAME", "CODEVALUE", names(longtable)[names(longtable)%in%valueset$VARNAME]) %>% 
  
  #' left join long table to small valueset
  
    left_join(select(valueset,
                     "VARNAME", 
                     "CODEVALUE", 
                     "VALUELABEL")) %>% 
  
  #' paste variable value numeric with variable value description into a new column crazy separater hopefully never in description
  
    mutate(VALUE_CODE_LABEL = 
             paste0(CODEVALUE, "/_/", VALUELABEL)) %>% 
  #' remove CODEVALUE and VALUELABEL for spread to function correctly
    select(-CODEVALUE, -VALUELABEL) %>% 
  
  #' spread on variable fill in with var number desc
    spread(key = VARNAME, value = VALUE_CODE_LABEL)
  
  #' separate the value from the description remove the original gross one
  
  for(name in names(ds)[names(ds)%in%valueset$VARNAME]) ds <- separate_(ds, name,sep = "/_/", into = c(paste0(name, "_value"), name), remove = T)

  #' return dataset  
  return(ds)
}

#' testing function
#' 
test <- add_values(longtable = ds, valueset = valueset_adm)
