# KF's eensy teensy contribution to the IPEDS CHANGE THE WORLD project - 8.10.17
# creates two functions that make "table name" field based on either csv filename or "TableName" from IPEDS valuesets
# assumes individual IPEDS csvs have been downloaded but not compiled yet......call this fucntion as part of that compilation script
# assumes only IPEDS files for a single given survey are saved in the WD (example: EFA)

#this function turns csv file names into table names without dates.
#run this one one indiviual csvs when compiling
table_from_file <- function(dir = wd){
  setwd(dir) 
  filename <- list.files()
  #uppercase the filename -- just use the first one in the folder.
  tablename <- toupper(filename[1])
  # remove ".csv" from file name - get rid of everthing after and including the "."
  tablename <-gsub("\\..*" ,"", tablename)
  #remove "_RV" from file name
   tablename <- gsub("_RV" ,"", tablename)
  #remove the year digist, but keep any other digits
   tablename <- sub( '[[:digit:]]{4}' ,"", tablename)
  return(tablename)
}

#this version take the ipeds column "TableName" from the value lables file and makes a new version to match csvs
#run this one on compiled "valuesets" file
table_from_column <- function(x = ds$TableName){
  #uppercase the ipeds data just in case it isnt already
  tablename <- toupper(x)
  #remove the year digits, but keep any other digits
  tablename <- sub( '[[:digit:]]{4}' ,"", tablename)
  return(tablename)
}


#FOR TESTING kf reads in ONE fall enrollment csv off of the Smith College S drive.  can delete this later.
path <- ifelse(file.exists("S:/"), "S:/", "/Volumes/files/Shared/")
df <- read.csv(paste0(path, "IRO/Resources/IPEDS/(OLD) csv file compilation/Fall Enrollment/input/ef2014a.csv"))
wd <-(paste0(path, "IRO/Resources/IPEDS/(OLD) csv file compilation/Fall Enrollment/input"))
df2 <- read.csv(paste0(path, "IRO/Resources/IPEDS/(OLD) csv file compilation/Finance/input/f1415_f2.csv"))
wd2 <- (paste0(path, "IRO/Resources/IPEDS/(OLD) csv file compilation/Finance/input"))

#more testing -- second function

df3 <- read_excel(paste0(path, "IRO/resources/IPEDS/Access File Compilation/documentation/IPEDS201415TablesDoc.xlsx"), 
                  sheet="valueSets14")



df$table <- table_from_file(wd)
table(df$table)


df2$table <- table_from_file(wd2)
table(df2$table)

df3$cleantable <- table_from_column(df3$TableName)
table(df3$cleantable)
