---
title: "Years to Indices"
author: "Emma Morgan"
date: "2/6/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r dictionary-iterate}
dictionary_dir <- paste(here::here(),"IPEDS-Data-Ignore/November 2019 Dictionaries from App", sep = "/")

#Now iterate through the dictionaries to see where we have any variables with numeric pattern in the name
dictionary_years <- list()
dictionary_files <- list.files(dictionary_dir)

for (f in dictionary_files) {
  dictionary <- read_csv(paste(dictionary_dir,f,sep="/"), col_types = cols(.default ="c")) %>%
    filter(!is.na(VARTITLE))
  vartitle_years <- dictionary %>%
    select(VARTITLE) %>%
    unlist(use.names = FALSE) %>%
    map_lgl(~ str_detect(.x,"[:digit:]{4}-[:digit:]{2}")) %>%
    any()
  if(vartitle_years) {
    print(paste0(f,": ",as.character(vartitle_years)))
    vartitles <- filter(dictionary,str_detect(VARTITLE,"[:digit:]{4}-[:digit:]{2}"))
    dictionary_years[[f]] <- vartitles
  } else(next)
  
}

rm("f","dictionary","vartitles","dictionary_files","vartitle_years")


```

##Revising the dictionaries

Now we are going to revise the dictionaries and see if we still have any issues

```{r revise-dictionary}

IPEDS_data_location_general <- paste0(here::here(),"/IPEDS-Data-Ignore/Original IPEDS Data")

revise_dictionaries <- c("12-month Instructional Activity",
                         "Student Charges (IC AY)",
                         "Student Charges Vocational (IC PY)",
                         "Student Financial Aid",
                         "Outcome Measures")



for (d in revise_dictionaries) {
  IPEDS_data_location <- paste0(IPEDS_data_location_general,"/",d)
  dictionary_list <- compile_lookup_list(IPEDS_data_location=IPEDS_data_location, sheetName="varlist")
  dictionary_unique <- lookup_unique(dictionary_list, sheetName ="varlist")
  write_csv(dictionary_unique,paste0(here::here(),"/IPEDS-Data-Ignore/revised-dictionaries/",d,".csv"))
}


  


```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
