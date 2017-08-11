#Sample script to compile vartable and valuesets longitudinal files

script <- RCurl::getURL("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/develop_em/compile_varname_valueset.R", ssl.verifypeer = FALSE)

eval(parse(text = script))

compiled_var_info <- compile_vartable_valuesets()

#Specify your output directory to write your files
outputdir <- "Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/Access Database/Compiling var info/R export"

#Write compiled vartable
write.csv(compiled_var_info[['vartable_compiled']],paste(outputdir,"vartable_compiled_NEW.csv",sep="/"),row.names=FALSE,na="")
write.csv(compiled_var_info[['valuesets_compiled']],paste(outputdir,"valuesets_compiled_NEW.csv",sep="/"),row.names=FALSE,na="")
