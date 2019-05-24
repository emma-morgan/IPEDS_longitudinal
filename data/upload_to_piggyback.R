# Script to initialize and upload data to piggyback

install.packages("piggyback")


devtools::install_github("ropensci/piggyback")

library("piggyback")

#Sys.setenv(GITHUB_TOKEN="6807c1c8f073215bc9416966295e67b951c3f8de")


Sys.setenv(GITHUB_TOKEN="6a93bb0a77a874dd03e4ca3a69263108b54b8b3f")

Sys.setenv(GITHUB_TOKEN="9f9f2246610a78466ff5d15fb0aa60427632a990")

pb_new_release("kaloisio/IPEDS_data", "v0.0.3")


readr::write_tsv(mtcars, "mtcars.tsv.gz")

pb_upload("mtcars.tsv.gz", 
          repo = "kaloisio/IPEDS_data", 
          tag = "v0.0.2")


pb_delete(file = "mtcars.tsv.gz", 
          repo = "kaloisio/IPEDS_data", 
          tag = "v0.0.1")


pb_download("mtcars.tsv.gz", 
            repo = "kaloisio/IPEDS_data",
            tag = "v0.0.2",
            dest = "C:/Users/kaloisio/Desktop/",
            .token="")








test <- read.table(gzfile(paste0(tempdir(), "/mtcars.tsv.gz")))


dir('C:/Users/kaloisio/Desktop/c_a/Data/')

files2zip <- dir('C:/Users/kaloisio/Desktop/c_a/Data/', full.names = TRUE)
zip(zipfile = 'testZip', files = files2zip)


file.info(c('testZip.zip', 'testDir/test.csv'))['size']


install.packages("rvest")
library('rvest')

#Specifying the url for desired website to be scraped
url <- 'https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx'

#Reading the HTML code from the website
webpage <- read_html(url)

require(devtools)
install_github('jbryer/ipeds')


temp <- tempfile()
download.file("https://nces.ed.gov/ipeds/datacenter/data/ADM2017.zip",temp)
con <- unz(temp, "adm2017.csv")
data <- matrix(scan(con),ncol=4,byrow=TRUE)
unlink(temp)

year = 2016
survey = "Adm"

zipfile = paste0("https://nces.ed.gov/ipeds/datacenter/data/", toupper(survey), year, ".zip")
csvfile = paste0(tolower(survey), year, ".csv")


temp <- tempfile()
download.file(zipfile,temp)
data <- read.csv(unz(temp, csvfile))
unlink(temp)

names(data)


#https://nces.ed.gov/ipeds/datacenter/data/ADM2017_Dict.zip


#cant compile on my laptop