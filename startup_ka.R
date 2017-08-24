#' reads in mock data for testing
#' KMA 8/10/17
#' 
path <- ifelse(file.exists("S:/"), "S:/", "/Volumes/files/Shared/")

#' function for loading multiple packages will assess whether a package needs to be installed before loading
#' the argument pkg takes a list of package names in quotes e.g. pkg = c("dplyr","tidyr")

pkgs <- c("tidyverse", "readxl")
for(pkg in pkgs) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
  library(pkg, character.only = TRUE)
}

longtable <- read.csv("C:/Users/kaloisio/Documents/IPEDS data/EF_A_compiled.csv", stringsAsFactors = F)
names(longtable)
set.seed(12345)
ds <- longtable %>% mutate(ROW_ID = 1:nrow(longtable)) %>%  sample_n(10000)

longtable <- read.csv("C:/Users/kaloisio/Documents/IPEDS data/ADM_compiled.csv", stringsAsFactors = F)
names(longtable)
ds <- longtable %>% mutate(ROW_ID = 1:nrow(longtable)) 
set.seed(12345)
ds <- longtable %>% mutate(ROW_ID = 1:nrow(longtable)) %>%  sample_n(10000)
rm(longtable)

valuesets <- read.csv("C:/Users/kaloisio/Documents/IPEDS data/valuesets_compiled2.csv", stringsAsFactors = F)
names(valuesets)
table(valuesets$VARNAME)
valueset_EFA <- valuesets %>% 
  filter(TABLENUMBER==21,
         VARNAME%in%names(longtable)[names(longtable)%in%valuesets$VARNAME]
)

valueset_adm <- valuesets %>% 
  filter(TABLENUMBER==120,
         VARNAME%in%names(longtable)[names(longtable)%in%valuesets$VARNAME])

source("C:/Users/kaloisio/Documents/GitHub/IPEDS_longitudinal/add_valuesets.R")

ds_clean <- add_values(longtable = ds, valueset = valueset_EFA)

varnames <- read.csv("C:/Users/kaloisio/Documents/IPEDS data/vartable_compiled_uniqueTitles.csv", stringsAsFactors = F)
names(varnames)

varnames_efa <- varnames %>% 
  filter(TABLENUMBER%in%c(21),
         VARNAME%in%names(ds_clean)[names(ds_clean)%in%varnames$VARNAME])

varnames_adm <- varnames %>% 
  filter(TABLENUMBER%in%c(15, 120),
         VARNAME%in%names(ds_clean)[names(ds_clean)%in%varnames$VARNAME])

header <- read.csv("C:/Users/kaloisio/Documents/IPEDS data/hd2014.csv", stringsAsFactors = F)
valueset_header <- valuesets %>% 
  filter(TABLENUMBER==10,
         VARNAME%in%names(header)[names(header)%in%valuesets$VARNAME]
  )

varnames_header <- varnames %>% 
  filter(TABLENUMBER%in%c(10),
         VARNAME%in%names(ds)[names(ds)%in%varnames$VARNAME])


ds <- add_values(longtable = header, valueset = valueset_header)


source("C:/Users/kaloisio/Documents/GitHub/IPEDS_longitudinal/change_varnames_to_vartitles.R")

vars <- select_vars(longtable = ds, varnames = varnames_header)

ds_clean <- change_varnames_vartitles(longtable = ds, varnames = varnames_header, vars = vars)

write.csv(ds_clean, "C:/Users/kaloisio/Documents/IPEDS data/hd2014_CLEAN.csv", row.names = F)


#### test using r compiled datasets ####
path <- ifelse(file.exists("S:/"), "S:/", "/Volumes/files/Shared/")

longtable <- read.csv(paste0(path, "IRO/resources/IPEDS/(OLD) csv file compilation/Institutional Characteristics/compiled/Institutional Characteristics_compiled.csv"), stringsAsFactors = F)

valuesets <- read.csv("C:/Users/kaloisio/Documents/IPEDS data/valuesets_compiled2.csv", stringsAsFactors = F)
valueset_test <- valuesets %>% 
  filter(TABLENUMBER==12,
         VARNAME%in%names(longtable)[names(longtable)%in%valuesets$VARNAME])

source("C:/Users/kaloisio/Documents/GitHub/IPEDS_longitudinal/add_valuesets.R")

ds_clean <- add_values(longtable = longtable, valueset = valueset_test)

source("C:/Users/kaloisio/Documents/GitHub/IPEDS_longitudinal/change_varnames_to_vartitles.R")

varnames <- read.csv("C:/Users/kaloisio/Documents/IPEDS data/vartable_compiled_rev.csv", stringsAsFactors = F)

varnames_test <- varnames %>% 
  filter(TABLENUMBER%in%c(13),
         VARNAME%in%names(ds_clean)[names(ds_clean)%in%varnames$VARNAME])

vars <- select_vars(longtable = ds_clean, varnames = varnames_test)

ds_clean_2 <- change_varnames_vartitles(longtable = ds_clean, varnames = varnames_test, vars = vars)
