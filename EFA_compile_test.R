#Fall Enrollment A compile
#Emma reference only; others cannot reliably use!
#However, the change to valuesets is IMPORTANT

#Sample script to compile vartable and valuesets longitudinal files

script_vartable <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/compile_varname_valueset.R", ssl.verifypeer = FALSE)
script_peerList <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/peerList.R", ssl.verifypeer = FALSE)
script_filename_to_tablename <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/filename_to_tablename.R", ssl.verifypeer = FALSE)
script_add_valuesets <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/add_valuesets.R", ssl.verifypeer = FALSE)
script_varnames_to_titles <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/change_varnames_to_vartitles.R", ssl.verifypeer = FALSE)


eval(parse(text = script_vartable))
eval(parse(text = script_peerList))
eval(parse(text = script_filename_to_tablename))
eval(parse(text = script_add_valuesets))
eval(parse(text = script_varnames_to_titles))

rm("script_vartable","script_peerList","script_filename_to_tablename","script_add_valuesets","script_varnames_to_titles","pkg","pkgs")


#*************************************************************************
# Compiling longitudinal IPEDS files - a sample
#******************************************************************************

#Sample using IC_AY (Student charges by Academic Year)
#Get ready to compile your file! To begin, you'll want to select your peer group.

#Read in your peer file; this will contain the dataframe from the .csv and a list of schools that can be used to subset the longtiudinal file
peerInfo <- IPEDS_peers_from_file("Q:/Staff/President, Provost, Trustees/TAAC Dashboard/Data/AY 2015-16 TAAC DB data/Misc - Peer comparison/IPEDs Data Center/UndergradPeers_IDandNames.csv")
peers_UNITID <- as.character(peerInfo[['peers_for_IPEDS']])

#Code below assumes that you already have a longitudinal table compiled from Access or another method
#See alternative instructions for compiling longitudinal file if necessary
#NOTE: Will need to add the compilation instructions in R at a later date when they are complete

#longtable_filepath is the filepath to your longtiudinal table

longtable_filepath <- "Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/Access Database/Compiling longitudinal/EF_A_compiled.csv"
valuesets_filepath <- "Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/Access Database/Compiling var info/R export/valuesets_compiled_rev.csv"
vartable_filepath <- "Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/Access Database/Compiling var info/R export/vartable_compiled_rev_EMedits.csv"

longtable_full <- read.csv(longtable_filepath,stringsAsFactors = F)
longtable_full[['SURVEYNAME']] <- "EFA"
surveyName <- names(table(longtable_full['SURVEYNAME']))

#If you have uploaded a list of peers, segment longtable
if (exists("peers_UNITID")) {
  print("Subsetting IPEDS table to the peer group that has been provided.")
  longtable <- longtable_full[longtable_full[['UNITID']] %in% peers_UNITID,]
}

if (! exists("peers_UNITID")) {
  print("You have not provided a peer group, so you are using the entire IPEDS table.")
  longtable <- longtable_full
}

#Read in full dictionary for valuesets and variable names/titles
#Subset valuesets and vartable to only use the data needed for the current longtable
valuesets_full <- read.csv(valuesets_filepath,stringsAsFactors = F)
vartable_full <- read.csv(vartable_filepath,stringsAsFactors = F)

valuesets_full[['SURVEYNAME']] <- table_from_column(valuesets_full[['TABLENAME']])
valuesets <- valuesets_full[valuesets_full['SURVEYNAME']==surveyName,]

#Check to see if all valuesets VARNAME_CODEVALUE are unique
valuesets['VALUESET_REF'] <- paste(valuesets[['VARNAME']],"_",valuesets[['CODEVALUE']],sep="")
if (any(duplicated(valuesets['VALUESET_REF']))) {
  print("Duplicate VARNAME+CODEVALUE; cleaning to keep most recent, but please check for consistency in data")
  valuesets_tableOrig <- valuesets
  valuesets <- valuesets[order(valuesets$TABLENAME, decreasing=TRUE),]
  #Drop duplicates - TEST ONLY - WILL NEED TO CREATE A BETTER METHOD FOR THIS
  valuesets <- valuesets[-which(duplicated(valuesets$VALUESET_REF)),]
  valuesets <- valuesets[order(valuesets$CODEVALUE),]
}

vartable_full[['SURVEYNAME']] <- table_from_column(vartable_full[['TABLENAME']])
vartable <- vartable_full[vartable_full['SURVEYNAME']==surveyName,]


if (any(duplicated(vartable['VARNAME']))) {
  print("Dulicate VARNAMES within table; cleaning to keep most recent, but please check for consistency.")
  vartable_tableOrig <- vartable
  vartable <- vartable[order(vartable$TABLENAME, decreasing=TRUE),]
  #Drop duplicates - TEST ONLY - WILL NEED TO CREATE A BETTER METHOD FOR THIS
  vartable <- vartable[-which(duplicated(vartable$VARNAME)),]
  vartable <- vartable[order(valuesets$VARNAME),]
}


#If this survey has valuesets, add values
#Sample IC_AY does not have values, so this will be ignored

if (is_empty(valuesets[['CODEVALUE']])) {
  print(paste("Table",surveyName,"","does not have any valuesets. Proceed with initial longtable."))
}
if (! is_empty(valuesets[['CODEVALUE']])){
  longtable <- add_values(longtable,valuesets)
}

#Variable names to variable titles

longtable_preserved <- longtable

vars_of_interest <- select_vars(longtable,varnames = vartable)

longtable_with_names <- change_varnames_vartitles(longtable = longtable, varnames=vartable, vars = vars_of_interest)

outputdir <- choose.dir()

write.csv (longtable_with_names,paste(outputdir,"\\",surveyName,"_compiled.csv",sep=""),row.names=FALSE,na="")
