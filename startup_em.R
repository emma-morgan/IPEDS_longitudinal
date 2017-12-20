#Updated by Emma Morgan (emma.morgan@tufts.edu)
#12/19/2017

#Sample script to compile vartable and valuesets longitudinal files
#Can we do this without compiling the vartable/valuesets at the beginning?

script_peerList <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/peerList.R", ssl.verifypeer = FALSE)
script_filename_to_tablename <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/filename_to_tablename.R", ssl.verifypeer = FALSE)
script_add_valuesets <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/add_valuesets.R", ssl.verifypeer = FALSE)
script_varnames_to_titles <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/change_varnames_to_vartitles.R", ssl.verifypeer = FALSE)
script_acadyear <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/acad_yr_function.R", ssl.verifypeer = FALSE)
script_varname_to_varID <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/varname_to_varID.R", ssl.verifypeer = FALSE)


eval(parse(text = script_peerList))
eval(parse(text = script_filename_to_tablename))
eval(parse(text = script_add_valuesets))
eval(parse(text = script_varnames_to_titles))
eval(parse(text = script_acadyear))
eval(parse(text = script_varname_to_varID))


rm("script_peerList","script_filename_to_tablename","script_acadyear",
   "script_add_valuesets", "script_varname_to_varID","script_varnames_to_titles",
   "pkg","pkgs")

########TEST###########################

#Run this line to clear everything except functions
rm("data_add_valuesets","data_add_vartitles","data_final","IPEDS_data",
   "IPEDS_data_subset","IPEDS_dictionary","IPEDS_valueset","IPEDS_data_location",
   "IPEDS_data_location_general","IPEDS_test","peer_filepath","peerList","surveyFolder",
   "output_dir","IPEDS_data_Carnegie")

#surveyFolder <- 
IPEDS_data_location_general <- "Q:\\Staff\\University-Wide\\Peer Comparison Database\\IPEDS\\Original IPEDS Data"
IPEDS_data_location <- paste(IPEDS_data_location_general,surveyFolder, sep="\\")
IPEDS_test <- merge_IPEDS_data(IPEDS_data_location)
IPEDS_data <- IPEDS_test$data
IPEDS_dictionary <- IPEDS_test$dictionary
IPEDS_valueset <- IPEDS_test$valuesets

#Subset to Peer List
peer_filepath <- "Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/UndergradPeers_IDandNames.csv"
peerList <- IPEDS_peers_from_file(peer_filepath)
IPEDS_data_subset <- subset(IPEDS_data,IPEDS_data$UNITID %in% peerList$peers_for_IPEDS)

#Subset to 25 schools for testing purposes
#IPEDS_data_subset <- IPEDS_data[IPEDS_data$UNITID %in% names(table(IPEDS_data$UNITID))[1:25],]

data_add_valuesets <- add_values(longtable=IPEDS_data_subset, valueset = IPEDS_valueset)
data_add_vartitles <- change_varnames_vartitles(longtable=data_add_valuesets, varnames=IPEDS_dictionary)

#Add Institution Names
data_final <- dplyr::left_join(data_add_vartitles, peerList$peerdf,"UNITID","INSTITUTION.NAME")

output_dir <- "Q:/Staff/President, Provost, Trustees/TAAC Dashboard/Data/AY 2017-18 TAAC DB Data/Adm Enroll Grad Fin Aid (Feb 18)/IPEDS Peer Comparison"

data.table::fwrite(data_final,paste(output_dir,"/",surveyFolder,"_",Sys.Date(),".csv",sep=""))

##Testing for Dana without subset
#If we're testing with no subset...
data_add_valuesets <- add_values(longtable=IPEDS_data, valueset = IPEDS_valueset, ignore_size_warning = T)
data_add_vartitles <- change_varnames_vartitles(longtable=data_add_valuesets, varnames=IPEDS_dictionary)
output_dir <- "P:/Peer comparison Database/IPEDS database/For Dana Aug 2017"
data.table::fwrite(data_add_vartitles,paste(output_dir,"/",surveyFolder,"_",Sys.Date(),".csv",sep=""))

#Admissions
surveyFolder <- "Admissions"

#Fall Enrollment A
surveyFolder <- "Fall Enrollment A"

#Fall Enrollment D (retention rates)
surveyFolder <- "Fall Enrollment D"

#Graduation Rates
surveyFolder <- "Graduation Rates"

#Student Financial Aid
surveyFolder <- "Student Financial Aid"

#Directory Information (IC Header File)
surveyFolder <- "Directory Information"

  #Subset to include only the most recent year - we want each entry only once!
directory_info_recent <- IPEDS_data[IPEDS_data$ACAD_YEAR==max(IPEDS_data$ACAD_YEAR),]
naCols <- names(which(sapply(directory_info_recent, function(x) all(is.na(x)))))
directory_info_recent_CLEAN <- dplyr::select(directory_info_recent, -which(names(directory_info_recent) %in% naCols))
data_add_valuesets <- add_values(longtable=directory_info_recent_CLEAN, valueset = IPEDS_valueset)
data_add_vartitles <- change_varnames_vartitles(longtable=data_add_valuesets, varnames=IPEDS_dictionary)

output_dir <- "Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/IPEDS World Domination compiled"
data.table::fwrite(data_add_vartitles,paste(output_dir,"/",surveyFolder,"_",as.character(max(directory_info_recent$ACAD_YEAR)-1),".csv",sep=""))

#######Preliminary subsetting using directory information###############

headFile <- read.csv("Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/IPEDS World Domination compiled/Directory Information_2016.csv", stringsAsFactors = FALSE)

#Subset to only Bachelors, Masters, and Doctoral institutions; remove associates and special focus
IPEDS_data_Carnegie <- dplyr::left_join(IPEDS_data, dplyr::select(headFile, "UNITID","Carnegie.Classification.2015..Basic"))
IPEDS_data_subset <- dplyr::filter(IPEDS_data_Carnegie, 
                                   #startsWith(x=IPEDS_data_Carnegie$Carnegie.Classification.2015..Basic, prefix="Bacc") |
                                    # startsWith(x=IPEDS_data_Carnegie$Carnegie.Classification.2015..Basic, prefix="Mast") |
                                     startsWith(x=IPEDS_data_Carnegie$Carnegie.Classification.2015..Basic, prefix="Doct")
                                   )

data_add_valuesets <- add_values(longtable=IPEDS_data_subset, valueset = IPEDS_valueset)
data_add_vartitles <- change_varnames_vartitles(longtable=data_add_valuesets, varnames=IPEDS_dictionary)
output_dir <- "Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/IPEDS World Domination compiled"
data.table::fwrite(data_add_vartitles,paste(output_dir,"/",surveyFolder,".csv",sep=""))
data.table::fwrite(data_add_vartitles,paste(output_dir,"/","Dated files","/",surveyFolder,"_",Sys.Date(),".csv",sep=""))

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
"Finance F1A"
"Finance F2"
"Finance F3"
"Graduation Rates"
"Institutional Characteristics"
"Instructional Staff Salaries IS"
"Instructional Staff Salaries NIS"
"Student Charges (IC AY)"
"Student Financial Aid"

