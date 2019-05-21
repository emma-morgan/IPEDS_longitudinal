############################################################################################################
# NEW version of dealing with changes in race variables for IPEDS Fall Enrollment #
# assumes an already row-bound data file has been run through both the "values" and "var titles" functions
# UPDATED TO USE NEW METHOD IWD FILES
# KMF
# 9.21.17
#############################################################################################################




#### Setup ####

#first, install and require all necessary packages
pkgs <- c("plyr", "dplyr", "mosaic", "tidyr", "stringr", "data.table", "readxl", "RCurl")
for(pkg in pkgs) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
  library(pkg, character.only = TRUE)
}


#set directory path based on Mac or PC - for reading in IPEDS csvs
path <- ifelse(file.exists("S:/"), "S:/", "/Volumes/files/Shared/")
setwd(path)


####Read in Data####
#read in CLEAN BOUND EFA file
EFA <- read.csv(paste0(path, "IRO/resources/IPEDS/All Surveys/Fall Enrollment/efa/compiled/efa_compiled.csv"), stringsAsFactors = F, check.names = F)

#can't name multiple columns the same thing, even though "Hispanic Men" and "Hispanic or Latino men" are the same for my purposes.
#but i CAN rename things all the same if they are in rows.  I pivot these data for by-race analysses in tableau anyway
# so................


### Gather File####
#first, move non-race vars to start of the file, to make the gather easier
EFA <- EFA %>% select("UNITID", "TABLE_TRIM", "FILE_NAME", "ACAD_YEAR", `Grand total men(EFRACE15_20241)`, `Grand total men(EFTOTLM_20241)`, 
                        `Grand total women(EFRACE16_20246)`, `Grand total women(EFTOTLW_20246)`, 
                        `Grand total(EFRACE24_20286)`, `Grand total(EFTOTLT_20286)`, `Level of student (original line number on survey form)`, `Level of student (original line number on survey form)_value`, 
                        `Level of student(EFALEVEL_20166)`, `Level of student(EFALEVEL_20166)_value`, 
                        `Level of student(LSTUDY_21991)`, `Level of student(LSTUDY_21991)_value`, everything())

dput(names(EFA))

EFA_tall <- EFA %>% 
  gather(IPEDS_Race_and_Gender, Student_Count, c("American Indian or Alaska Native men": "White/White non-Hispanic women - derived" ))

# THEN rename similar race categories to be the same
EFA_tall <- EFA_tall %>% mutate(clean_ipeds_race = 
    ifelse( (grepl("alien", IPEDS_Race_and_Gender) & grepl("total", IPEDS_Race_and_Gender)), "Nonresident alien total", 
            ifelse ((grepl("alien", IPEDS_Race_and_Gender) & grepl(" men", IPEDS_Race_and_Gender)), "Nonresident alien men",
                    ifelse ((grepl("alien", IPEDS_Race_and_Gender) & grepl("women", IPEDS_Race_and_Gender)), "Nonresident alien women",
                    ifelse ((grepl("unknown", IPEDS_Race_and_Gender) & grepl("total", IPEDS_Race_and_Gender)), "Race/ethnicity unknown total",
                    ifelse ((grepl("unknown", IPEDS_Race_and_Gender) & grepl(" men", IPEDS_Race_and_Gender)), "Race/ethnicity unknown men",
                    ifelse ((grepl("unknown", IPEDS_Race_and_Gender) & grepl("women", IPEDS_Race_and_Gender)), "Race/ethnicity unknown women",
                    ifelse ((grepl("White", IPEDS_Race_and_Gender) & grepl("total", IPEDS_Race_and_Gender)), "White total",
                    ifelse ((grepl("White", IPEDS_Race_and_Gender) & grepl(" men", IPEDS_Race_and_Gender)), "White men",
                    ifelse ((grepl("White", IPEDS_Race_and_Gender) & grepl("women", IPEDS_Race_and_Gender)), "White women",
                    ifelse ((grepl("Black", IPEDS_Race_and_Gender) & grepl("total", IPEDS_Race_and_Gender)), "Black or African American total",
                    ifelse ((grepl("Black", IPEDS_Race_and_Gender) & grepl(" men", IPEDS_Race_and_Gender)), "Black or African American men",
                    ifelse ((grepl("Black", IPEDS_Race_and_Gender) & grepl("women", IPEDS_Race_and_Gender)), "Black or African American women",
                    ifelse ((grepl("Hispanic", IPEDS_Race_and_Gender) & grepl("total", IPEDS_Race_and_Gender)), "Hispanic total",
                    ifelse ((grepl("Hispanic", IPEDS_Race_and_Gender) & grepl(" men", IPEDS_Race_and_Gender)), "Hispanic men",
                    ifelse ((grepl("Hispanic", IPEDS_Race_and_Gender) & grepl("women", IPEDS_Race_and_Gender)), "Hispanic women",
                    ifelse ((grepl("Asian", IPEDS_Race_and_Gender) & grepl("women", IPEDS_Race_and_Gender)), "Asian or Pacific Islander women",
                    ifelse ((grepl("Asian", IPEDS_Race_and_Gender) & grepl(" men", IPEDS_Race_and_Gender)), "Asian or Pacific Islander men",
                    ifelse ((grepl("Asian", IPEDS_Race_and_Gender) & grepl("total", IPEDS_Race_and_Gender)), "Asian or Pacific Islander total",
                    ifelse ((grepl("Hawaiian.or", IPEDS_Race_and_Gender) & grepl(" men", IPEDS_Race_and_Gender)), "Native Hawaiian or Other Pacific Islander men_after2010",
                    ifelse ((grepl("Hawaiian.or", IPEDS_Race_and_Gender) & grepl("women", IPEDS_Race_and_Gender)), "Native Hawaiian or Other Pacific Islander women_after2010",
                    ifelse ((grepl("Hawaiian.or", IPEDS_Race_and_Gender) & grepl("total", IPEDS_Race_and_Gender)), "Native Hawaiian or Other Pacific Islander total_after2010",
                    ifelse ((grepl("Indian", IPEDS_Race_and_Gender) & grepl(" men", IPEDS_Race_and_Gender)), "American Indian or Alaska Native men",
                    ifelse ((grepl("Indian", IPEDS_Race_and_Gender) & grepl("women", IPEDS_Race_and_Gender)), "American Indian or Alaska Native women",
                    ifelse ((grepl("Indian", IPEDS_Race_and_Gender) & grepl("total", IPEDS_Race_and_Gender)), "American Indian or Alaska Native total", IPEDS_Race_and_Gender))))))))))))))))))))))))
)



# add flags needed for tableau

EFA_tall <- EFA_tall %>% mutate(SOC_women = 
                                  ifelse ((grepl("Black", clean_ipeds_race) & grepl("women", clean_ipeds_race)), "1",
                                          ifelse((grepl("Hispanic", clean_ipeds_race) & grepl("women", clean_ipeds_race)), "1",
                                            ifelse((grepl("Asian", clean_ipeds_race) & grepl("women", clean_ipeds_race)), "1",
                                              ifelse((grepl("Hawaiian", clean_ipeds_race) & grepl("women", clean_ipeds_race)), "1",
                                                ifelse(( grepl("Indian", clean_ipeds_race) & grepl("women", clean_ipeds_race)), "1",
                                                       ifelse((grepl("Two", clean_ipeds_race) & grepl("women", clean_ipeds_race)), "1","0")) )))))


EFA_tall <- EFA_tall %>% mutate(SOC_total = 
                                  ifelse ((grepl("Black", clean_ipeds_race) & grepl("total", clean_ipeds_race)), "1",
                                          ifelse((grepl("Hispanic", clean_ipeds_race) & grepl("total", clean_ipeds_race)), "1",
                                                 ifelse((grepl("Asian", clean_ipeds_race) & grepl("total", clean_ipeds_race)), "1",
                                                        ifelse((grepl("Hawaiian", clean_ipeds_race) & grepl("total", clean_ipeds_race)), "1",
                                                               ifelse(( grepl("Indian", clean_ipeds_race) & grepl("total", clean_ipeds_race)), "1",
                                                                      ifelse((grepl("Two", clean_ipeds_race) & grepl("total", clean_ipeds_race)), "1","0")) )))))
EFA_tall <- EFA_tall %>% mutate(URM_total = 
                                  ifelse ((grepl("Black", clean_ipeds_race) & grepl("total", clean_ipeds_race)), "1",
                                          ifelse((grepl("Hispanic", clean_ipeds_race) & grepl("total", clean_ipeds_race)), "1",
                                                        ifelse((grepl("Hawaiian", clean_ipeds_race) & grepl("total", clean_ipeds_race)), "1",
                                                               ifelse(( grepl("Indian", clean_ipeds_race) & grepl("total", clean_ipeds_race)), "1","0")) )))

EFA_tall <- EFA_tall %>% mutate(URM_women = 
                                  ifelse ((grepl("Black", clean_ipeds_race) & grepl("women", clean_ipeds_race)), "1",
                                          ifelse((grepl("Hispanic", clean_ipeds_race) & grepl("women", clean_ipeds_race)), "1",
                                                 ifelse((grepl("Hawaiian", clean_ipeds_race) & grepl("women", clean_ipeds_race)), "1",
                                                        ifelse(( grepl("Indian", clean_ipeds_race) & grepl("women", clean_ipeds_race)), "1","0")) )))

EFA_tall <- EFA_tall %>% mutate(intl_total = 
                                  ifelse((grepl("alien", clean_ipeds_race) & grepl("total", clean_ipeds_race)), "1", "0"))
EFA_tall <- EFA_tall %>% mutate(intl_women = 
                                  ifelse((grepl("alien", clean_ipeds_race) & grepl("women", clean_ipeds_race)), "1", "0"))



EFA_tall <- EFA_tall %>% mutate(all_UG = 
                                  ifelse(`Level of student(EFALEVEL_20166)`== "All students, Undergraduate total", "1", "0"))
EFA_tall <- EFA_tall %>%  mutate(first_years = 
                                   ifelse (`Level of student(EFALEVEL_20166)`== "All students, Undergraduate, Degree/certificate-seeking, First-time", "1", "0")) 
EFA_tall <- EFA_tall %>% mutate(total_flag = 
                                  ifelse((grepl("total", clean_ipeds_race)), "1", "0"))
EFA_tall <- EFA_tall %>% mutate(women_flag = 
                                  ifelse((grepl("women", clean_ipeds_race)), "1", "0"))


#remove "derived" race rows because they DUPLICATE the "old" one and thngs are gettign double counted
EFA_tall <- EFA_tall %>% mutate(derived = 
                                  ifelse(grepl("derived", IPEDS_Race_and_Gender), "1", "0"))

EFA_tall <-EFA_tall %>% filter(derived == "0")

# write out a PIVOTED version of EFS for by-race analyses.

write.csv(EFA_tall, paste0(path, "IRO/resources/IPEDS/All Surveys/Fall Enrollment/efa/compiled/efa_compiled_racepivot.csv"), row.names = F)


