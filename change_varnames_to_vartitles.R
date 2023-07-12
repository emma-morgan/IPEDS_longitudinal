#' Goal: Change variable names to more english words.
#' Contains function input dataset longitudinal table file and variable name file output  dataset longitudinal table file with variable titles instead of variable name 
#' KMA - 8/10/17
#' KMA - Updated 3/24/2023 to replace gather and spread
#' KMA - Updated 7/12/2023 with the help of Mark and Srishti to make more efficent
#' 
#' Requires one longitudinal table and a premade subset of varname with only variables from longitudinal table
#' Assumes data file has already been run through add_value function
#' 
#### Load in packages ####
#' package dependences??
#' loading in complete tidyverse packages for more information: http://www.tidyverse.org/
#' loading tidyfast to speed up the pivot wider leveraging the power of data.tables
pkgs <- c("tidyverse", "tidyfast")
for(pkg in pkgs) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
  library(pkg, character.only = TRUE)
}
rm("pkgs","pkg")

#### change_varnames_vartitles function ####
#' start with a dictionary and long table
change_varnames_vartitles <- function(longtable, varnames, ignore_size_warning = F) {
  if(!ignore_size_warning&nrow(longtable)>50000){stop("Large file may break things, consider using subset_peerlist() to reduce file size. Set ignore_size_warning=T to override this error.")}
  if(ignore_size_warning){warning("Large file may break things, consider using subset_peerlist() to reduce file size and compile time.")}
  
  
  #'because of multiple years need to add a unique row id for spread to work
  ds <- dplyr::mutate(longtable, ROW_ID = 1:nrow(longtable))
  
  #' gather long table with all variables that need to be changed
  ds <- tidyr::pivot_longer(ds, 
                            cols = colnames(ds %>% 
                                              select(-ROW_ID, -UNITID, 
                                                     -ACAD_YEAR, -FILE_NAME, 
                                                     -TABLE_TRIM)), 
                            names_to = "VARIABLE_ID", 
                            values_to = "VALUE")
  
  #' create a copy of the variable id with no _value in order to merge with IPEDS dictionary
  ds <- ds %>% mutate(VARIABLE_ID_noval = gsub("_value", "", VARIABLE_ID))
  
  
  #' left join with clean var title
  ds <- ds %>% left_join(varnames %>% 
                           select(VARIABLE_ID, 
                                  VARTITLE_USE), by=c("VARIABLE_ID_noval"="VARIABLE_ID"))
  
  #' If original variable name has _value suffix, ensure variable label has _value suffix
  ds <- ds %>% mutate(VARTITLE = ifelse(VARIABLE_ID != VARIABLE_ID_noval, 
                      paste0(VARTITLE_USE, "_value"), VARTITLE_USE))
  
  
  #'remove unneeded variables for spread to be happy
  ds <- dplyr::select(ds, -VARTITLE_USE, -VARIABLE_ID_noval, -VARIABLE_ID)
    
  #' spread to make dataset wide again
  ds <- tidyfast::dt_pivot_wider(ds, names_from = VARTITLE, values_from = VALUE)
  
  #' remove row id
  ds <- dplyr::select(ds, -ROW_ID)
  
  #' return dataset
  return(ds)
  }
