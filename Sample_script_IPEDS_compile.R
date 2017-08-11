#Sample script to compile vartable and valuesets longitudinal files

script_vartable <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/develop_em/compile_varname_valueset.R", ssl.verifypeer = FALSE)
script_peerList <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/develop_em/peerList.R", ssl.verifypeer = FALSE)


eval(parse(text = script_vartable))
eval(parse(text = script_peerFile))





#***************************************************************************
#Create longitudinal compiled vartable and valuesest
#   using IPEDS access database table documentation
#***************************************************************************

compiled_var_info <- compile_vartable_valuesets()

#Specify your output directory to write your files
outputdir <- "Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/Access Database/Compiling var info/R export"

#Write compiled vartable
write.csv(compiled_var_info[['vartable_compiled']],paste(outputdir,"vartable_compiled_rev.csv",sep="/"),row.names=FALSE,na="")
write.csv(compiled_var_info[['valuesets_compiled']],paste(outputdir,"valuesets_compiled_rev.csv",sep="/"),row.names=FALSE,na="")

#*************************************************************************
# Compiling longitudinal IPEDS files - a sample
#******************************************************************************

#Get ready to compile your file! To begin, you'll want to select your peer group.

#Read in your peer file; this will contain the dataframe from the .csv and a list of schools that can be used to subset the longtiudinal file
peerInfo <- IPEDS_peers_from_file()
peers_IPEDS <- as.character(peerInfo[['peers_for_IPEDS']])
