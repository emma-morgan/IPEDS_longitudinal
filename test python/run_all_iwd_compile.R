


pkgs <- c("tidyverse", "knitr", "rmarkdown", "readr","styler", "kableExtra", "janitor", "likert", "readxl", "googlesheets4", "gt", "vroom", "ggtext", "DT","lubridate", "stringr", "scales", "gt", "htmltools")
for(pkg in pkgs) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "http://cran.us.r-project.org")
    library(pkg, character.only = TRUE)
  }
  library(pkg, character.only = TRUE)
}






# read in "lookup" file of all surveys and subsurveys
surveys <-  googlesheets4::read_sheet(ss="https://docs.google.com/spreadsheets/d/1rJH29SnAB3g8jwrNV6qovKyyRHlOXw6jZOBjx0Gbbs4/edit#gid=0",
                    sheet="inventory") %>% 
  dplyr::select(c(`IPEDS Survey`, `IPEDS Sub-Survey`, `in app`)) %>% 
  dplyr::filter(`in app`=="y" )%>% 
  dplyr::mutate(params = purrr::map2(`IPEDS Survey`, `IPEDS Sub-Survey`,
                                    \(x, y) list(survey = x,
                                              subsurvey = y )))


# loop though ipeds surveys and run iwd compilation for each

#system.time(
  
#  for (i in 1:nrow(surveys)){
i=1
    print(i)
    print(surveys$params[[i]])
    rmarkdown::render(
      input = "GitHub/IPEDS_longitudinal/test python/IWD_full_compile_MIT.Rmd",
      params = surveys$params[[i]],
      envir = new.env())
    
#  }
  
#)


