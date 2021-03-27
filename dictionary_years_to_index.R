#Updated 3/27/2021 by Emma Morgan (emm.c.morgan@gmail.com)
#'This function takes a dictionary file and, where necessary, replaces stated years in the vartitles with
#'year indices for consistency. For instance, the 2016-17 file will replace 2016-17 with "current year"
#'and 2015-16 with "prior year".
#'Currently, this is designed to function with Student Charges (IC_AY) and Student Financial Aid (SFA),
#'Student Charges for vocation programs (IC_PY), and 12-month instructional activity


dictionary_years_to_index <- function(dictionary_unique) {
  
  dictionary_unique_modified <- dplyr::mutate(dictionary_unique, 
                                       reference_ay = dplyr::if_else(str_detect(VARTITLE,"[:digit:]{4}-[:digit:]{2}"),
                                                              as.integer(str_extract(VARTITLE,"[:digit:]{4}(?=-[:digit:]{2})"))+1,
                                                              -99))
  dictionary_unique_modified <- dplyr::mutate(dictionary_unique_modified,
                                       reference_index = dplyr::if_else(reference_ay != -99, as.integer(ACAD_YEAR) - reference_ay, -99))
  
  #Now use the reference index to replace as needed.
  dictionary_unique_modified <- dplyr::mutate(dictionary_unique_modified,
                                       VARTITLE_USE = purrr::map2_chr(reference_index, 
                                                               VARTITLE_USE, 
                                                               ~ dplyr::case_when(.x==0 ~ str_replace(.y, "[:digit:]{4}-[:digit:]{2}", "(current year)"),
                                                                           .x == 1 ~ str_replace(.y, "[:digit:]{4}-[:digit:]{2}", "(prior year)"),
                                                                           .x >= 2 ~ str_replace(.y, "[:digit:]{4}-[:digit:]{2}",
                                                                                                 paste0("(",as.character(.x)," years prior)")),
                                                                           .x==-1 ~ str_replace(.y, "[:digit:]{4}-[:digit:]{2}", "(next year)"),
                                                                           .x == -99 ~ .y)))
  
  dictionary_unique_modified <-  dplyr::select(dictionary_unique_modified -reference_ay,-reference_index)

  return(dictionary_unique_modified)
}


#Creating a function that will specifically replace the date references in Outcomes Measures file
om_dates_to_index <- function(dictionary_unique) {
  dictionary_unique_modified <- dictionary_unique %>%
    #multiple references that specify year index and also specific date; check for this and remove the date
    mutate(VARTITLE_USE = map_chr(VARTITLE, ~ str_replace(.x, "(?<=[:digit:] years)\\s\\(August 31, [:digit:]{4}\\)",""))) %>%
    mutate(VARTITLE_USE = map_chr(VARTITLE_USE, ~ str_replace(.x, "^[:digit:]{4}-[:digit:]{2} cohort", "Cohort"))) %>%
    mutate(VARTITLE_USE = map_chr(VARTITLE_USE, ~ str_replace(.x, "[:digit:]{4}-[:digit:]{2}\\s(?=cohort)|[:digit:]{4}\\s(?=cohort)", ""))) %>%
    #This is very specific and I don't love the specificity, but this is a very odd survey!
    mutate(VARTITLE_USE = map_chr(VARTITLE_USE, ~ str_replace(.x, "Additional exclusions \\(September 1\\, 2014 through August 31\\, 2016\\)",
                                                              "Additional exclusions \\(6-8 years\\)")))
}

