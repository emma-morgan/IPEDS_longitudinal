#Updated by Emma Morgan (emma.morgan@tufts.edu)
#4/2/2018

#Sample script to compile vartable and valuesets longitudinal files

#First load in all scripts; this can be cleaned up!
#Trying to load in which of the scripts we need; we shouldn't have to load all at the beginning. Rather, they should load in sequence!

script_peerList <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/peerList.R", 
                                 ssl.verifypeer = FALSE)
eval(parse(text = script_peerList))
rm("script_peerList")

script_filename_to_tablename <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/filename_to_tablename.R", ssl.verifypeer = FALSE)
script_add_valuesets <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/add_valuesets.R", ssl.verifypeer = FALSE)
script_varnames_to_titles <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/change_varnames_to_vartitles.R", ssl.verifypeer = FALSE)
script_acadyear <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/acad_yr_function.R", ssl.verifypeer = FALSE)

script_read_clean_data <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/develop_em/read_clean_data.R", ssl.verifypeer = FALSE)
script_merge_IPEDS_data <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/develop_em/merge_ipeds_data.R", ssl.verifypeer = FALSE)


eval(parse(text = script_filename_to_tablename))
eval(parse(text = script_add_valuesets))
eval(parse(text = script_varnames_to_titles))
eval(parse(text = script_acadyear))

eval(parse(text = script_read_clean_data))
eval(parse(text = script_merge_IPEDS_data))

rm("pkg","pkgs", "script_add_valuesets", "script_lookup_helper", "script_merge_IPEDS_data", "script_peerList",
   "script_read_clean_data","script_varnames_to_titles")

########TEST###########################

#Run this line to clear everything except functions
suppressWarnings(rm("data_add_valuesets","data_add_vartitles","data_final","IPEDS_data",
   "IPEDS_data_subset","IPEDS_dictionary","IPEDS_valueset","IPEDS_data_location",
   "IPEDS_data_location_general","IPEDS_test","peer_filepath","peerList","surveyFolder",
   "output_dir","IPEDS_data_Carnegie"))


#TRying this test with the NEW version of merge_IPEDS_data.
#Eventually, the merge_IPEDS_data_NEW will replace merge_IPEDS_data, but for now we might need
#   to use them both
#   this uses the peer file as the subset; the peer file will be a data frame in R

#Tufts only TEST
  peer_UNITIDs <- c("168148")

#Tufts standard peers TEST
  peer_filepath <- "Q:\\Staff\\University-Wide\\Peer Comparison Database\\IPEDS\\UndergradPeers_IDandNames.csv"
  IPEDS_peers <- IPEDS_peers_from_file(peer_filepath)
  peer_UNITIDs <- IPEDS_peers$peers_for_IPEDS
  
#surveyFolder <- 
all_IPEDS_folders <- list.files(path="Q:\\Staff\\University-Wide\\Peer Comparison Database\\IPEDS\\Original IPEDS Data")
surveyFolder <- all_IPEDS_folders[[i]]
IPEDS_data_location_general <- "Q:\\Staff\\University-Wide\\Peer Comparison Database\\IPEDS\\Original IPEDS Data"
IPEDS_data_location <- paste(IPEDS_data_location_general,surveyFolder, sep="\\")
IPEDS_test <- merge_IPEDS_data_NEW(IPEDS_data_location, peer_UNITIDs)
IPEDS_data <- IPEDS_test$data
IPEDS_dictionary <- IPEDS_test$dictionary
IPEDS_valueset <- IPEDS_test$valuesets

#We have integrated peer subsetting into IPEDS_merge_data
data_add_valuesets <- add_values(longtable=IPEDS_data, valueset = IPEDS_valueset, ignore_size_warning = T)
data_add_vartitles <- change_varnames_vartitles(longtable=data_add_valuesets, varnames=IPEDS_dictionary, ignore_size_warning = T)

#Add Institution Names
data_final <- dplyr::left_join(data_add_vartitles, IPEDS_peers$peerdf,"UNITID","INSTITUTION.NAME")
View (data_final)

output_dir <- "Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/IPEDS World Domination compiled"
data.table::fwrite(data_final,paste(output_dir,"/",surveyFolder,".csv",sep=""))
data.table::fwrite(data_final,paste(output_dir,"/","Dated files","/",surveyFolder,"_",Sys.Date(),".csv",sep=""))


################################################
#This is the old version; probably will be deleted soon.
#surveyFolder <- 
IPEDS_data_location_general <- "Q:\\Staff\\University-Wide\\Peer Comparison Database\\IPEDS\\Original IPEDS Data"
IPEDS_data_location <- paste(IPEDS_data_location_general,surveyFolder, sep="\\")
peer_filepath <- "Q:\\Staff\\University-Wide\\Peer Comparison Database\\IPEDS\\UndergradPeers_IDandNames.csv"
IPEDS_peers <- IPEDS_peers_from_file(peer_filepath)
IPEDS_test_2 <- merge_IPEDS_data(IPEDS_data_location)
IPEDS_data_2 <- IPEDS_test_2$data
IPEDS_dictionary_2 <- IPEDS_test_2$dictionary
IPEDS_valueset_2 <- IPEDS_test_2$valuesets

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

headFile_subset <- dplyr::filter(headFile,
                                 startsWith(x=headFile$Carnegie.Classification.2015..Basic, prefix="Doct") &
                                   Control.of.institution=="Private not-for-profit")
data.table::fwrite(headFile_subset,paste(output_dir,"/",surveyFolder,"_","2016","privateDoct",".csv",sep=""))

#######Preliminary subsetting using directory information###############

headFile <- read.csv("Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/IPEDS World Domination compiled/Directory Information_2016privateDoct.csv", stringsAsFactors = FALSE)

#Subset header file to only include Doctoral granting institutions that are private not-for-profit


#Subset to only Bachelors, Masters, and Doctoral institutions; remove associates and special focus
IPEDS_data_Carnegie <- dplyr::left_join(IPEDS_data, dplyr::select(headFile, "UNITID","Carnegie.Classification.2015..Basic"))
IPEDS_data_subset <- dplyr::filter(IPEDS_data_Carnegie, !is.na(IPEDS_data_Carnegie$Carnegie.Classification.2015..Basic))
# IPEDS_data_subset <- dplyr::filter(IPEDS_data_Carnegie, 
#                                    #startsWith(x=IPEDS_data_Carnegie$Carnegie.Classification.2015..Basic, prefix="Bacc") |
#                                     # startsWith(x=IPEDS_data_Carnegie$Carnegie.Classification.2015..Basic, prefix="Mast") |
#                                      startsWith(x=IPEDS_data_Carnegie$Carnegie.Classification.2015..Basic, prefix="Doct")
#                                    )

data_add_valuesets <- add_values(longtable=IPEDS_data_subset, valueset = IPEDS_valueset, ignore_size_warning = T)
data_add_vartitles <- change_varnames_vartitles(longtable=data_add_valuesets, varnames=IPEDS_dictionary, ignore_size_warning=T)
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

