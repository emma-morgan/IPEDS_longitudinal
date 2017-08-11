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

vars <- list()
for (name in names(ds_clean)[names(ds_clean)%in%varnames$VARNAME]) vars[[name]] <- names(ds_clean)[grepl(paste0(name, "_*"), names(ds_clean))]
vars <- unlist(vars, use.names = F)

ds <- ds_clean %>% gather("VARNAME", "VALUE", vars) %>% separate(VARNAME, into = c("VARNAME", "extra_temp"), sep = "_", remove=T) %>% 
  left_join(select(varnames_adm,
                   "VARNAME",
                   "VARTITLE")) %>% 
  mutate(VARTITLE_CLEAN = ifelse(!is.na(extra_temp), paste0(VARTITLE, "_", extra_temp), VARTITLE)) %>% 
  select(-VARTITLE, -extra_temp, -VARNAME) %>% 
  dplyr::rename(VARTITLE = VARTITLE_CLEAN) %>% 
  spread(key = VARTITLE, value = VALUE)

