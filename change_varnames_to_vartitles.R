#' Goal: Change variable names to more english words.
#' Contains function input dataset longitudinal table file and variable name file output  dataset longitudinal table file with variable titles instead of variable name 
#' KMA - 8/10/17
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
rm("pkgs","pkg")

#### change_varnames_vartitles function ####
#' start with a dictionary and long table
change_varnames_vartitles <- function(longtable, varnames, ignore_size_warning = F) {
  if(!ignore_size_warning&nrow(longtable)>50000){stop("Large file may break things, consider using subset_peerlist() to reduce file size. Set ignore_size_warning=T to override this error.")}
  if(ignore_size_warning){warning("Large file may break things, consider using subset_peerlist() to reduce file size and compile time.")}
  
  
  # filter to only varnames that are in longtable based on dictionary VARIABLE_ID
  # XX I DONT KNOW IF THIS LINE IS NECESSARY IF SOMETHING BREAKS CHECK THIS LINE FIRST
  #varnames <- varnames %>% filter(varnames$TABLE_TRIM%in%longtable$TABLE_TRIM)
  
  
  vars <- list()
  for (name in names(longtable)[names(longtable)%in%varnames$VARIABLE_ID]) vars[[name]] <- names(longtable)[grepl(paste0(name, "_*"), names(longtable))]
  vars <- unlist(vars, use.names = F)
  
  #'because of multiple years need to add a unique row id for spread to work
  ds <- dplyr::mutate(longtable, ROW_ID = 1:nrow(longtable))
  
  #' gather long table with all variables that need to be changed, previous versions of tidyr need to have !!var in order to work
  ds <- tidyr::gather(ds, "VARIABLE_ID", "VALUE", !!vars)
  
  #' separate the _value temporarily
  ds <- tidyr::separate(ds, VARIABLE_ID, into = c("VARIABLE_ID", "extra_temp"), sep = "_val", remove=T)
  
  #' left join with clean var title
  ds <- dplyr::left_join(ds, select(varnames,
                                    "VARIABLE_ID",
                                    "VARTITLE_USE"))
  
  ds <- dplyr::mutate(ds, VARTITLE_CLEAN = ifelse(!is.na(extra_temp),
                                                    paste0(VARTITLE_USE, "_val", extra_temp), VARTITLE_USE))   
  #'remove unneeded variables for spread to be happy
  ds <- dplyr::select(ds, -VARTITLE_USE, -extra_temp, -VARIABLE_ID)
    
  #' rename vartitle_clean to be consistant
  ds <- dplyr::rename(ds, VARTITLE = VARTITLE_CLEAN)
    
  #' spread to make dataset wide again
  ds <- tidyr::spread(ds, key = VARTITLE, value = VALUE)
    
  #' remove row id
  ds <- dplyr::select(ds, -ROW_ID)
    
  
#' return dataset
return(ds)
  }
