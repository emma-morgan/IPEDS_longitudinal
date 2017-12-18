#' Goal: Change variable names to more english words.
#' Contains function input dataset longitudinal table file and variable name file output  dataset longitudinal table file with variable titles instead of variable name 
#' KMA - 8/10/17
#' EM - 12/18/17 (added to see if I can make it work for me!)
#' 
#' 
#' Requires one longitudinal table and a premade subset of varname with only variables from longitudinal table
#' Assumes data file has already been run through add_value function
#' 
#### Load in packages ####
#' package dependences??
#' loading in complete tidyverse packages for more information: http://www.tidyverse.org/
pkgs <- c("tidyverse")
for(pkg in pkgs) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
  library(pkg, character.only = TRUE)
}

#### source filename_to_table ####
source("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/filename_to_tablename.R")

#### select_vars function ####
#' first need to get a list of variable names including the ones with _value
#' XX DOES THIS REALLY BELONG HERE??? - I DONT THINK THIS IS NEEDED ANYMORE NOW WITHIN CHANGE VARNAMES
#' making this into its own function outputs a VECTOR of variable names 
select_vars <- function(longtable, varnames) {
  # add tablename_clean to vars
  varnames$TABLE_TRIM <- table_from_column(varnames$TABLENAME)
  varnames <- varnames %>% filter(varnames$TABLE_TRIM%in%longtable$TABLE_TRIM)
  vars <- list()
  for (name in names(longtable)[names(longtable)%in%varnames$VARNAME]) vars[[name]] <- names(longtable)[grepl(paste0(name, "_*"), names(longtable))]
  vars <- unlist(vars, use.names = F)
  return(vars)
}

#### change_varnames_vartitles function ####
#' start with a subset of varititle and long table
change_varnames_vartitles <- function(longtable, varnames, ignore_size_warning = F) {
  if(!ignore_size_warning&nrow(longtable)>50000){stop("Large file may break things, consider using subset_peerlist() to reduce file size. Set ignore_size_warning=T to override this error.")}
  if(ignore_size_warning){warning("Large file may break things, consider using subset_peerlist() to reduce file size and compile time.")}
  
  # XX THIS SHOULD NOT BE HERE THIS SHOULD BE RUN IN COMPILING VARNAMES BUT IT IS HERE NOW SO THIS WORKS
#  varnames$TABLE_TRIM <- table_from_column(varnames$TABLENAME)
  
  # filter to only varnames that are in longtable based on table trim - this is problematic if the trim file name is different than the trim name in varnames
#  varnames <- varnames %>% filter(varnames$TABLE_TRIM%in%longtable$TABLE_TRIM)
  vars <- list()
  for (name in names(longtable)[names(longtable)%in%varnames$VARIABLE_ID]) vars[[name]] <- names(longtable)[grepl(paste0(name, "_*"), names(longtable))]
  vars <- unlist(vars, use.names = F)
  
  ds <- longtable %>% 
    
    #'because of multiple years need to add a unique row id for spread to work
    dplyr::mutate(ROW_ID = 1:nrow(longtable)) %>% 
    
    #' gather long table with all variables that need to be changed, previous versions of tidyr need to have !!var in order to work
    tidyr::gather("VARIABLE_ID", "VALUE", !!vars) %>% 
    
    #' separate the _value temporarliy
    tidyr::separate(VARIABLE_ID, into = c("VARIABLE_ID", "extra_temp"), sep = "_val", remove=T) %>% 
    
    #' left join with clean var title
    dplyr::left_join(select(varnames,
                     "VARIABLE_ID",
                     "VARTITLE_USE")) %>% 
    
    #' re paste the _value column
    dplyr::mutate(VARTITLE_CLEAN = ifelse(!is.na(extra_temp), 
                                   paste0(VARTITLE_USE, "_val", extra_temp), VARTITLE_USE)) %>%   
    
    #'remove unneeded variables for spread to be happy
    dplyr::select(-VARTITLE_USE, -extra_temp, -VARIABLE_ID) %>% 
    
    #' rename vartitle_clean to be consistant
    dplyr::rename(VARTITLE = VARTITLE_CLEAN) %>% 
    
    #' spread to make dataset wide again
    tidyr::spread(key = VARTITLE, value = VALUE) %>%
    
    #' remove row id
    dplyr::select(-ROW_ID)
  
#' return dataset
return(ds)
  }
