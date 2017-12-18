#Updated by Emma Morgan (emma.morgan@tufts.edu)
#12/18/2017

#Sample script to compile vartable and valuesets longitudinal files
#Can we do this without compiling the vartable/valuesets at the beginning?

#script_vartable <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/compile_varname_valueset.R", ssl.verifypeer = FALSE)
#script_peerList <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/peerList.R", ssl.verifypeer = FALSE)
script_filename_to_tablename <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/filename_to_tablename.R", ssl.verifypeer = FALSE)
#script_add_valuesets <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/add_valuesets.R", ssl.verifypeer = FALSE)
#script_varnames_to_titles <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/change_varnames_to_vartitles.R", ssl.verifypeer = FALSE)
script_acadyear <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/acad_yr_function.R", ssl.verifypeer = FALSE)


#eval(parse(text = script_vartable))
#eval(parse(text = script_peerList))
eval(parse(text = script_filename_to_tablename))
#eval(parse(text = script_add_valuesets))
#eval(parse(text = script_varnames_to_titles))
eval(parse(text = script_acadyear))


rm("script_filename_to_tablename","script_acadyear")

########TEST###########################
#Admissions and Test Scores
#surveyFolder <- 
IPEDS_data_location_general <- "Q:\\Staff\\University-Wide\\Peer Comparison Database\\IPEDS\\Original IPEDS Data"
IPEDS_data_location <- paste(IPEDS_data_location_general,surveyFolder, sep="\\")
IPEDS_test <- merge_IPEDS_data(IPEDS_data_location)
IPEDS_data <- IPEDS_test$data
IPEDS_dictionary <- IPEDS_test$dictionary
IPEDS_valueset <- IPEDS_test$valuesets

#Subset to 25 schools for testing purposes
IPEDS_data_subset <- IPEDS_data[IPEDS_data$UNITID %in% names(table(IPEDS_data$UNITID))[1:25],]

data_add_valuesets <- add_values(longtable=IPEDS_data_subset, valueset = IPEDS_valueset)
data_add_vartitles <- change_varnames_to_vartitles(longtable=data_add_valuesets, varnames=IPEDS_dictionary)

#Possible Survey Names:

"Academic Libraries"
"Completions A"
"Completions B"
"Completions c"
"Directory Information"
"Employees by Assigned Position"
"Fall Enrollment A"
"Fall Enrollment B"
"Fall Enrollment C"
"Fall Enrollment CP"
"Fall Enrollment D"
"Fall Staff IS"
"Fall Staff NH"
"Fall Staff OC"
"Fall Staff SIS"
"Finance F2"
"Graduation Rates"
"Institutional Characteristics"
"Instructional Staff Salaries IS"
"Instructional Staff Salaries NIS"
"Student Charges (IC AY)"
"Student Financial Aid"

