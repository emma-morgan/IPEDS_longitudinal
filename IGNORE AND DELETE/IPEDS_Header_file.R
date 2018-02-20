#IPEDS Header File

library(readxl)
library(plyr)

#Download data file
temp <- tempfile()
download.file("http://nces.ed.gov/ipeds/datacenter/data/HD2014.zip",temp)
data <- read.csv(unz(temp, "hd2014.csv"))
unlink(temp)

#Download dictionary file
excel_sheets("Q:/Staff/OIRE/OLD STUFF/Old Staff Folders/Emma/Peer Database/IPEDS/Institutional Characteristics/hd2014_dictionary.xlsx")



for (i in 1:nrow (peerlist_)) {
  peerlist_$RUVH[[i]] <- peerlist_$unitid[[i]] %in% ruvh$UNITID 
}



#Compile number of metrics

setwd("Q:/Staff/OIRE/OLD STUFF/Old Staff Folders/Emma/Peer Database/IPEDS/Dictionary Full Files")
mylist <- list()
for (i in 1:length(list.files())) {
  filename <- list.files()[i]
  if ("varlist" %in% excel_sheets(filename)) {
    sheetname <- "varlist"
  }
  else {
    print(excel_sheets(filename))
    sheetname <- readline(prompt="Which of the sheets should I read in? ")
  }
  ds <- read_excel(filename,sheetname)
  
  if ("varnumber" %in% names(ds)) {
    varcol <- "varnumber"
  }
  else {
    print (names(ds))
    varcol <- readline(prompt="Which of the columns has variable names? ")
  }
  if (length(which(is.na(ds[varcol]))) > 0) {
    ds_good <- ds[-(which(is.na(ds[varcol]))),]  
  }
  else {ds_good <- ds}
  
  mylist[[i]] <- assign(paste("ds",i,sep=""), ds_good)
}

varlist_full <- rbind.fill(mylist)
var_ful <- unique(varlist_full)
  