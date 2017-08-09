library(readxl)

#Compile variable names and var values

#First, want to create a unique vartable_long

#Located all files in a central location:

varlookup_dir <- "Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/Access Database/Compiling var info"
#varlookup_dir <- choose.dir()

#Isolate year; create list with file name and year
#This should be automated, but for now just do it manually; eventually we can pull the year fall from the list

lookupFile_list <- list.files(varlookup_dir)
fall_list <- c("07","08","09","10","11","12","13","14","15")

varfiles_list <- list()
vartables_list <- list()
valuesets_list <- list()
for (i in 1:length(lookupFile_list)){
  varinfo <- list()
  lookup_filepath <- paste(varlookup_dir,lookupFile_list[i],sep="\\")
  varinfo[['filepath']] <- paste(varlookup_dir,lookupFile_list[i],sep="\\")
  varinfo[['year']] <- fall_list[i]
  vartable <- read.xlsx(lookup_filepath,sheetName = paste("varTable",fall_list[i],sep=""))
  valuesets <- read.xlsx(lookup_filepath,sheetName = paste("valueSets",fall_list[i],sep=""))
  varinfo[['vartable']] <- vartable
  varinfo[['valueSets']] <- valuesets
  varfiles_list[[i]] <- varinfo
  vartables_list[[i]] <- vartable
  valuesets_list[[i]] <- valuesets
}

#This took WAY TOO LONG, so I want to try and make it shorter

valuesets_list_ORIG <- valuesets_list
vartables_list_ORIG <- vartables_list

for (i in 1:9){
  df <- valuesets_list[[i]]
  df_short <- df[substr(df$TableName,1,2) %in% c("AD","IC","EF"),]
  valuesets_list[[i]] <- df_short
}

#Warnings with factors - need to look into this later...
vartable_compiled <- dplyr::bind_rows(vartables_list)
valuesets_compiled <- dplyr::bind_rows(valuesets_list)

vartable_compiled[['varname_num']] <- paste(vartable_compiled$varName,vartable_compiled$varNumber,sep="_")
valuesets_compiled[['varset_id']] <- paste(valuesets_compiled$varName,valuesets_compiled$varNumber,valuesets_compiled$Codevalue,sep="_")

vartable_distinct <- vartable_compiled[!duplicated(vartable_compiled$varname_num),]
valuesets_distinct <- valuesets_compiled[!duplicated(valuesets_compiled$varset_id),]

write.csv(vartable_distinct,"Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/Access Database/Compiling var info/R export/vartable_compiled.csv", row.names=FALSE,na="")
write.csv(valuesets_distinct,"Q:/Staff/University-Wide/Peer Comparison Database/IPEDS/Access Database/Compiling var info/R export/valuesets_compiled.csv", row.names=FALSE,na="")
