#Updated 4/4/2018 by Emma Morgan (emma.morgan@tufts.edu)
#'This function takes a dictionary file and, where necessary, replaces stated years in the vartitles with
#'year indices for consistency. For instance, the 2016-17 file will replace 2016-17 with "current year"
#'and 2015-16 with "prior year".
#'Currently, this is designed to function with Student Charges (IC_AY) and Student Financial Aid (SFA)


dictionary_years_to_index <- function(dictionary_unique) {
  dictionary_unique_modified <- dictionary_unique %>%  
    
    #Add a year index for variables that have the year codes within them
    mutate(year_index = case_when(str_detect(VARTITLE, "[:digit:]{4}-[:digit:]{2}") & 
                                    str_detect(VARNAME, "[:digit:]$") ~ str_extract(VARNAME, "[:digit:]$"),
                                  str_detect(VARTITLE, "[:digit:]{4}-[:digit:]{2}") & str_detect(VARNAME, "GTD$") ~ "99")) %>%
    
    mutate(VARTITLE_ORIG = VARTITLE_USE) %>%
    
    #Now we need to specify for each of the dictionay files we are trying to clean
    
    #Student Financial Aid has current and 2 years prior
    mutate(VARTITLE_USE = pmap_chr(list(VARTITLE_USE, year_index, TABLE_TRIM),
                               #Student charges
                               function(x,y,z) case_when (y=="3" & z=="IC_AY" ~ str_replace(x, "[:digit:]{4}-[:digit:]{2}", "(current year)"),
                                                          y=="2" & z=="IC_AY" ~ str_replace(x, "[:digit:]{4}-[:digit:]{2}", "(prior year)"),
                                                          y=="1" & z=="IC_AY"  ~ str_replace(x, "[:digit:]{4}-[:digit:]{2}", "(2 years prior)"),
                                                          y=="0" & z=="IC_AY" ~ str_replace(x, "[:digit:]{4}-[:digit:]{2}", "(3 years prior)"),
                                                          y=="99" & z=="IC_AY" ~ str_replace(x, "[:digit:]{4}-[:digit:]{2}\\s", ""),
                                                          #Student Financial Aid
                                                          y=="2" & z=="SFA" ~ str_replace(x, "[:digit:]{4}-[:digit:]{2}", "(current year)"),
                                                          y=="1" & z=="SFA"  ~ str_replace(x, "[:digit:]{4}-[:digit:]{2}", "(prior year)"),
                                                          y=="0" & z=="SFA" ~ str_replace(x, "[:digit:]{4}-[:digit:]{2}", "(2 years prior)"),
                                                          TRUE ~ x))) %>%
    select(-VARTITLE_ORIG, year_index)
  
  return(dictionary_unique_modified)
}

# 
# 
# 
# IPEDS_data_location_general <- "Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/Original IPEDS Data"
# surveyFolder <- "Student Financial Aid"
# surveyFolder <- "Student Charges (IC AY)"
# 
# IPEDS_data_location <- paste(IPEDS_data_location_general,surveyFolder, sep="/")
# 
# dictionary_list <- compile_lookup_list(IPEDS_data_location=IPEDS_data_location, sheetName="varlist")
# dictionary_unique <- lookup_unique(dictionary_list, sheetName ="varlist")
# 
# 
# data.table::fwrite(dictionary_unique_modified, paste("Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/Compiled Dictionary/",surveyFolder, "_Dictionary.csv", sep=""))
# 
