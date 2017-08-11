#function to assign academic (fiscal) year to IPEDS csvs based on survey type and filename
# KFM
# 8.11.17



#first, install and require all necessary packages
pkgs <- c("stringr")
for(pkg in pkgs) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# checks whether year in file name is 2 digits or 4, pads with zeros if 2.
# then replaces first two digits with "20" to handle padded 00s and also to handle 1415 versus 2015

acad_year <- function(fileName, SURVEY){
  yr <- (stringr::str_extract(fileName, '[[:digit:]]{2,4}'))
  ifelse(nchar(yr)==2, str_pad(yr,4,side="left", pad="0"), yr)
  yr <- stringr::str_replace(yr, str_sub(yr, 1,2), "20")
  acadyr <- ifelse(SURVEY %in% c("Admission", "Fall Enrollment", "Institutional Characteristics",
                       "Human Resources"), (as.integer(yr) +1), yr)
  return(acadyr)
}


