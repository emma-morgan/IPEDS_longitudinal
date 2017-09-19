#' Goal: Add value labels to longitudinal file when value labels exist.
#' Contains function input dataset longitudinal table file and valueset file output  dataset longitudinal table file with value labels 
#' KMA - 8/10/17

#' subset valueset to only contain variables from the lonitudinal table 
# XXX SHOULD THIS BE IN THIS FUNCTION??

#### Load in packages ####
# loading in complete tidyverse packages for more information: http://www.tidyverse.org/
pkgs <- c("tidyverse", "RCurl")
for(pkg in pkgs) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
  library(pkg, character.only = TRUE)
}

#### source filename_to_table ####
source("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/filename_to_tablename.R")

#### add_values function ####
add_values <- function(longtable, valueset, ignore_size_warning=F) {
  if (any(is.na(valueset$CODEVALUE))) {stop("Missing code values. valueset must contain CODEVALUE to proceed.")} else
    if (is.character(valueset$CODEVALUE)){warning("Coercing code value to character.")
      valueset$CODEVALUE = as.character(valueset$CODEVALUE)}
  
  if(!ignore_size_warning&nrow(longtable)>50000){stop("Large file may break things, consider using subset_peerlist() to reduce file size. Set ignore_size_warning=T to override this error.")}
  if(ignore_size_warning){warning("Large file may break things, consider using subset_peerlist() to reduce file size and compile time.")}
  if(is_empty(names(longtable)[names(longtable)%in%valueset$VARNAME])){warning("No variables need labels. Returning dataset as is.")
    return(longtable)
    }

  # add tablename_clean to valueset
  valueset$TABLE_TRIM <- table_from_column(valueset$TABLENAME)
  valueset <- valueset %>% filter(valueset$TABLE_TRIM%in%longtable$TABLE_TRIM)
  
  ds <- longtable %>%
  
  #'because some institutions have multiple values in one value set for same year need to add a unique row id for spread to work
   dplyr::mutate(ROW_ID = 1:nrow(longtable)) %>% 
    
  #' select only the variables in small valueset to gather
    
    tidyr::gather("VARNAME", "CODEVALUE", names(longtable)[names(longtable)%in%valueset$VARNAME]) %>% 
    dplyr::mutate(CODEVALUE = as.character(CODEVALUE)) %>% 
  
  #' left join long table to small valueset
  
    dplyr::left_join(select(valueset,
                     "VARNAME", 
                     "CODEVALUE", 
                     "VALUELABEL")) %>% 
  
  #' paste variable value numeric with variable value description into a new column crazy separater hopefully never in description
  
    dplyr::mutate(VALUE_CODE_LABEL = 
             paste0(CODEVALUE, "/_/", VALUELABEL)) %>% 
  #' remove CODEVALUE and VALUELABEL for spread to function correctly
    dplyr::select(-CODEVALUE, -VALUELABEL) %>% 
  
  #' spread on variable fill in with var number desc
    tidyr::spread(key = VARNAME, value = VALUE_CODE_LABEL) %>%
  
  #' remove row id
   dplyr::select(-ROW_ID)
  
  #' separate the value from the description remove the original gross one
  
  for(name in names(ds)[names(ds)%in%valueset$VARNAME]) ds <- tidyr::separate_(ds, name,sep = "/_/", into = c(paste0(name, "_value"), name), remove = T)

  #' return dataset  
  return(ds)
}

#' testing function
#' 
# test <- add_values(longtable = ds, valueset = valueset_adm)
