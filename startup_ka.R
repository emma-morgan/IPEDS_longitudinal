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

valuesets <- read.csv("C:/Users/kaloisio/Documents/IPEDS data/valuesets_compiled2.csv", stringsAsFactors = F)
names(valuesets)
table(valuesets$VARNAME)
valueset_EFA <- valuesets %>% 
  filter(TABLENUMBER==21,
         VARNAME%in%c("EFALEVEL", "LINE", "SECTION", "LSTUDY"))

names(longtable)[names(longtable)%in%valuesets$varName]

valueset_EFA <- valuesets %>% 
  filter(Tablenumber==21,
         varName%in%c("EFALEVEL", "LINE", "SECTION", "LSTUDY"))

names(longtable)

set.seed(12345)
ds <- longtable %>% mutate(num = 1:nrow(longtable)) %>%  sample_n(10000) %>% gather("varName", "Codevalue", names(longtable)[names(longtable)%in%valueset_EFA$varName])

rm(longtable)

ds_join <- left_join(ds, select(valueset_EFA, "varName", "Codevalue", "valueLabel"))

rm(ds)

ds_join$value_code_label <- paste0(ds_join$Codevalue, "/_/", ds_join$valueLabel)

ds_spread <- ds_join %>% select(-Codevalue, -valueLabel) %>% spread(varName, value_code_label)

names(ds_spread)

ds_spread_test <- ds_spread %>% separate(c(EFALEVEL, LSTUDY), c("EFALEVEL", "EFALEVEL_desc", "LSTUDY", "LSTUDY_desc"), sep="/_/")


for(name in names(ds_spread)[names(ds_spread)%in%valueset_EFA$varName]) ds_spread <- separate_(ds_spread, name,sep = "/_/", into = c(paste0(name, "_value"), paste0(name, '_desc')), remove = T)

