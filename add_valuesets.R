#' Goal: Add value labels to longitudinal file when value labels exist.
#' Contains function input dataset longitudinal table file and valueset file output  dataset longitudinal table file with value labels 
#' KMA - 8/10/17

#' subset valueset to only contain variables from the lonitudinal table 
# XXX SHOULD THIS BE IN THIS FUNCTION??


add_values <- function(longtable, valueset) {
  
  ds <- longtable %>%
  
  #'because some institutions have multiple values in one value set for same year need to add a unique row id for spread to work XX SHOULD THIS HAPPEN HERE????
   # mutate(row_id = 1:nrow(longtable)) %>% 
  
  #' select only the variables in small valueset to gather
    
    gather("varName", "Codevalue", names(longtable)[names(longtable)%in%valueset$varName]) %>% 
  
  #' left join long table to small valueset
  
    left_join(select(valueset,
                     "varName", 
                     "Codevalue", 
                     "valueLabel")) %>% 
  
  #' paste variable value numeric with variable value description into a new column crazy separater hopefully never in description
  
    mutate(value_code_label = 
             paste0(Codevalue, "/_/", valueLabel)) %>% 
  #' remove Codevalue and valueLabel for spread to function correctly
    select(-Codevalue, -valueLabel) %>% 
  
  #' spread on variable fill in with var number desc
    spread(key = varName, value = value_code_label)
  
  #' separate the value from the description remove the original gross one
  
  for(name in names(ds)[names(ds)%in%valueset$varName]) ds <- separate_(ds, name,sep = "/_/", into = c(paste0(name, "_value"), name), remove = T)

  #' return dataset  
  return(ds)
}

#' testing function
#' 
test <- add_values(longtable = ds, valueset = valueset_EFA)
