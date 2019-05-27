# KF's eensy teensy contribution to the IPEDS CHANGE THE WORLD project - 8.10.17
# modified May 27 2019 to handle two digit dates on the ends of GR200 filenames.
# creates two functions that make "table name" field based on either csv filename or "TableName" from IPEDS valuesets
# assumes individual IPEDS csvs have been downloaded but not compiled yet......call this fucntion as part of that compilation script
# assumes only IPEDS files for a single given survey are saved in the WD (example: EFA)
# assumes that compiled csv cleanup will be done using IPEDS TableDoc.xls that comes down with IPEDS Access files.



#this function turns csv file names into table names without dates.
#run this one one indiviual csvs when compiling

table_from_file <- function(x,i){
  setwd(x) 
  filename <- list.files()
  #uppercase the filename -- just use the first one in the folder.
  tablename <- toupper(filename[i])
  # remove ".csv" from file name - get rid of everthing after and including the "."
  tablename <-gsub("\\..*" ,"", tablename)
  #remove "_RV" from file name
  tablename <- gsub("_RV" ,"", tablename)
  #remove the year digits, but keep any other digits
  tablename <- sub( '[[:digit:]]{4}' ,"", tablename)
  #remove the last underscore and two digits after it -- for the GR200 survey - and hope that this doesnt mess any other surveys up
  tablename2 <- sub("\\_.*" , "", tablename)
  
  return(tablename)
}
