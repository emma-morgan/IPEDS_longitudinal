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
rm("pkg","pkgs")

# checks whether year in file name is 2 digits or 4, pads with zeros if 2.
# then replaces first two digits with "20" to handle padded 00s and also to handle 1415 versus 2015

acad_year <- function(fileName, TABLE_TRIM){
  yr <- ifelse(grepl("200_", fileName), stringr::str_extract(fileName, '(?<=\\_)[[:digit:]]{2,4}'),  
               stringr::str_extract(fileName, '[[:digit:]]{2,4}'))
  yr <- ifelse(nchar(yr)==2, str_pad(yr,4,side="left", pad="0"), yr)
  yr <- stringr::str_replace(yr, stringr::str_sub(yr, 1,2), "20")
  acadyr <- ifelse(TABLE_TRIM %in% c("ADM", "DRVADM", "EF", "EFA", "EFB", "EFC", "EFD", "EFCP", "EFA_DIST", "DRVEF", 
                       "HD","IC", "IC_AY", "IC_PY", "DRVIC", "ICMISSION", "CUSTOMCGIDS",
                       "EAP", "SAL_B", "SAL_A", "SAL_IS", "SAL_FACULTY", "SAL_NIS", "SAL_A_LT9",
                      "S_OC", "S_ABD", "S_SIS", "S_F", "S_IS", "S_G", "S_NH", "S_CN", "DRVHR"), (as.integer(yr) +1), yr)
  return(acadyr)
}

