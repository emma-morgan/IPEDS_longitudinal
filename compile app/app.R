
#### add useful packages ####
library(tidyverse)
library(shiny)
library(piggyback)
library(shinycssloaders)
library(DT)
library(curl)
library(shinythemes)
library("htmltools")
library("bsplus")

#### version from IPEDS_data ####
version <- "v.2020.jan"


#### ui ####

ui <- fluidPage(
  theme = shinytheme("flatly"), 
  tags$head(
    tags$style(HTML("
    .shiny-output-error-validation {
                    color: orange;
font-size: 22px;
                    }
                    "))
  ),
  
  #### Application title ####
  titlePanel("IPEDS Data Compiler"),
  
  mainPanel(
    
    br(),
    
    tabsetPanel(
      tabPanel("Compiler",
               
               h3("Welcome to the IPEDS Data Compiler!"),
h4("Follow the steps below to generate and download a csv file of longitudinal IPEDS data for a custom group of institutions."),

h5("This app is still a work in progress and we hope your experience runs smoothly, however, if an error message appears, please refer to the FAQ tab for more information. We can also be contacted at ",tags$a(href = "mailto:iwdapplication@gmail.com?subject=IPEDS%20Data%20Compiler",
                                                                                                                                                                                                                                           "iwdapplication@gmail.com",".")),
br(),
               
               #### Step 1: Peerlist ####
               h4(tags$b("Step 1:"), "Upload your list of peers as a csv. Please make sure it contains NCES ID in a column called UNITID. Please note, any columns that you have included in your uploaded file will be joined onto the resulting csv that you download."),
               p("See the FAQ for an example peer list."), 
               
               
               br(),
               
               # peerlist upload
               fileInput("peerlist", "Choose csv file:",
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
               ), # closes fileinput
               
               br(),
               
               #### Peerlist preview table ####
               dataTableOutput("preview_peerlist") %>% withSpinner(color="#0dc5c1"),
               
               br(),
               
               #### Number of peer institutions #### 
               span(tags$b(textOutput("numpeers")), style="color:green; font-size: 20px;"),
               
               br(),
               
               #### Step 2: Select Survey ####
               
               h4(tags$b("Step 2:"), "Select an IPEDS survey from the dropdown list."),
               
               # select survey
               selectInput('survey', 'Select survey:', choices = list(
                 
                 `Institutional Characteristics` = c(`Directory information` = 'hd',
                                                     `Educational offerings, organization, services and athletic associations` = 'ic',
                                                     `Student charges for academic year programs` = 'ic_ay',
                                                     `Student charges by program (vocational programs)` = 'ic_py'),
                 
                 `Admissions` = c(`Applications, admissions, enrollees and test scores` = 'adm'),
                 
                 `Fall Enrollement` = c(`Race/ethnicity, gender, attendance status, and level of student`= "efa", 
                                        `Age category, gender, attendance status, and level of student` = "efb", 
                                        `Residence and migration of first-time freshman` = "efc",
                                        `Total entering class, retention rates, and student-to-faculty ratio` = "efd",
                                        `Distance education status and level of student` = "efa_dist"),
                 
                 `12-Month Enrollment` = c(`12-month unduplicated headcount` = 'effy',
                                           `12-month instructional activity` = 'efia'),
                 
                 `Completions` = c(`Awards/degrees conferred by program, award level, race/ethnicity, and gender`= 'c_a',
                                   `Number of students receiving awards/degrees, by race/ethnicity and gender` = 'c_b',
                                   `Number of students receiving awards/degrees, by level, gender, race/ethnicity, and age` = 'c_c',
                                   `Number of programs offered and number offered via distance education, by level` = 'cdep'),
                 
                 `Graduation Rates` = c(`Graduation rate data, 150% of normal time - 2 and 4 year institutions` = 'gr',
                                        `Graduation rate data, 150% of normal time - less-than-2-year institutions` = 'gr_l2',
                                        `Graduation rate data, 200% of normal time - 4 year and less-than-4-year institutions` = 'gr200',
                                        `Graduation rate data for Pell and Subsidized Stafford Loan recipients, 150% of normal time` = 'gr_pell_ssl'),
                 
                 `Outcomes Measures` = c(`Award and enrollment data at four, six and eight years of entering, by Pell status` = 'om'),
                 
                 `Student Financial Aid & Net Price` = c(`Student financial aid and net price` = 'sfa',
                                                         `Military Servicemembers and Veteran's Benefits` = 'sfav'),
                 
                 `Finance` = c(`Public institutions - GASB 34/35` = 'f_f1a',
                               `Private not-for-profit institutions or Public institutions using FASB` = 'f_f2',
                               `Private for-profit institutions` = 'f_f3'),
                 
                 `Instructional Staff/Salaries` = c(`Number and salary outlays for full-time nonmedical instructional staff, by gender and rank` = 'sal_is',
                                                    `Number and salary outlays for full-time nonmedical noninstructional staff by occupation` = 'sal_nis'),
                 
                 `Fall Staff` = c(`Full- and part-time staff by occupational category, race/ethnicity, and gender` = 's_oc',
                                  `Full-time instructional staff, by faculty and tenure status, and academic rank` = 's_sis',
                                  `Full-time instructional staff, by faculty and tenure status, rank, race/ethnicity, and gender` = 's_is',
                                  `New hires by occupational category, race/ethnicity, and gender` = 's_nh'),
                 
                 `Employees by Assigned Position` = c(`Number of staff by occupational category, faculty and tenure status` = 'eap'),
                 
                 `Academic Libraries` = c(`Academic Libraries` = 'al')
                 
                 
               ), selectize = FALSE), 
               
               br(),
               
               #### Step 3: Run Button ####
               h4(tags$b("Step 3:"), "Press the button below to see a preview of the first 10 columns of the dataset."),
               
               h5("It may take a couple of minutes for your file to load."),
               
               actionButton("goButton", "Dominate the World!", icon("paper-plane")), # closes actionbutton
               

               br(), br(),
               
               
               #### Data preview table ####
               div(dataTableOutput("preview") %>% withSpinner(color="#0dc5c1"), style = "font-size:95%") ,
               
               br(),

              #### Information about the Survey #### 
              span(tags$b(textOutput("surveyinfo")), style="color:green; font-size: 20px;"),
               
              br(),
               
               #### Step 4: Download ####
               h4(tags$b("Step 4:"), "Once the table renders, press the button below to download your compiled csv file."),
               
               downloadButton("download", "Download CSV"),
br(),

               h5(" Please note: IPEDS data are not adjudicated and the contents of the file you download here represent the data available from NCES as of January 2020.", tags$p("This project is a collaboration between the", tags$a(href = "https://provost.tufts.edu/institutionalresearch/", "Tufts University Office of Institutional Research", target="blank"), "and the", tags$a(href = "https://www.smith.edu/about-smith/institutional-research", "Smith College Office of Institutional Research.", target="blank"))) # closes h5
      ), # closes compiler tabPanel
      
      #### FAQ ####     
      tabPanel("FAQ",
               
               h3("Welcome to the IPEDS Data Compiler FAQ!"),
               br(),
        h4("We hope that you find our app useful and easy to use. In the event that you have any trouble with it please check the FAQ below to see if your issue is listed. If you continue to have trouble, please feel free to contact us at ",tags$a(href = "mailto:iwdapplication@gmail.com?subject=IPEDS%20Data%20Compiler",
                                                                                                                                                                                                                                                               "iwdapplication@gmail.com",".")),
        br(),
               
               p("Select Download Peer List Template for an example peer list."),
               
               downloadButton("download_peerlist", "Download Peer List Template"), 
      br(),
      br(),
      
      h4("Questions About Using the App"),
      br(),
      bs_accordion(id = "faq_app") %>%
        bs_append(title = "What does this app do?", content = "The IPEDS data compiler app reads in longitudinal files of IPEDS surveys containing data for every participating institution and uses the peer list that you provide to subset the IPEDS data to just the institutions in which you are interested and then allows you to download the resulting data file to your computer for your own use.") %>%
        
        bs_append(title = "Who can use this app?", content = "The short answer is anyone.  There is no cost to use this app and the data files are publicly available.  This app is intended for use by those who regularly makes use of IPEDS data.  Although the creators of the app are institutional researchers this app may be useful for other types of higher education professionals.") %>%
        
        bs_append(title = "Where do the longitudinal IPEDS files come from?", content = tags$div("The files being accessed by this app were created by institutional researchers at Smith College and Tufts University.  The goal of this joint project was to provide access to multi-year IPEDS files with user-friendly column names and value labels. The work of compiling and cleaning the data was done using R.  For more specifics on how these files were created, you may contact ",
                  tags$a(href = "mailto:iwdapplication@gmail.com",
                         "iwdapplication@gmail.com","."))) %>%
        
        
        
        bs_append("Why am I am getting this error: Please upload a csv with UNITID. See FAQ for an example template.", "This error will show if the peer list you uploaded does not contain a column called UNITID.  Please check your peer list and try again.  Press the Download Peer List Template button above to download a peer list template for your use.") %>% 
        
        bs_append("Why am I unable to upload my peer list?", "Please check that your peer file is saved as a .csv file and contains a column called UNITID that contains the NCES IDs for each institution you want included in the data file.  Press the Download Peer List Template button above to download a peer list template for your use.") %>% 
        bs_append("Can I upload an NCES .uid file as my peer list?", 
                  content = tags$div("Although our app cannot read .uid files, you can turn your existing .uid file into a .csv by following these steps:",
                                     tags$br(), tags$br(),
                                     tags$ol(
                                       tags$li(" Open the .uid file using excel"), 
                                       tags$li(" Highlight the single column of data and perform excel's text to columns function, indicating that the fields are delimited with the | symbol"),  
                                       tags$li(" Add a header row and be sure to name the column of IDs as 'UNITID'"), 
                                       tags$li(" Save the file as  .csv format"))))%>% 
        
        bs_append(title = "What if my peer list contains additional fields besides UNITID?", 
                  content = "All columns in your peer list file will be joined to the resulting csv that you download.  This will not adversely affect the IPEDS data contained in the file, and may increase the useability of your resulting data file, but if you prefer that extra fields are not joined to the data then you can create a new peer file with just UNITID (and, optionally, institution name).") %>% 
        
        bs_append(title = "What if I don't have a peer list?", 
                  content = tags$div(tags$p("This app is intended to run with a peer list.  If you don't have one you can create one.  Press the Download Peer List Template button above to download a peer list template.  You can populate this file with UNITIDs for any institution you like.  You can quickly create a list of institutions and their UNITDs based on institutional characteristics here:  ",
                                            tags$a(href = "https://nces.ed.gov/ipeds/datacenter/InstitutionByGroup.aspx", 
                                                   "https://nces.ed.gov/ipeds/datacenter/InstitutionByGroup.aspx",
                                                   target = "blank")), 
                                     tags$p("For example this will allow you to get a list of all institutions for a given Carnegie classification."), 
                                     tags$p("To get the UNITIDs for a specific set of institutions, you can search for institutions by name here: ",
                                            tags$a(href = "https://nces.ed.gov/ipeds/datacenter/InstitutionByName.aspx", 
                                                   "https://nces.ed.gov/ipeds/datacenter/InstitutionByName.aspx.",
                                                   target = "blank")),
                                     tags$p("There is additional information about creating lists of peer institutions available here: ",
                                            tags$a(href = "https://nces.ed.gov/Ipeds/Help/View/103",
                                                   "https://nces.ed.gov/Ipeds/Help/View/103",
                                                   target ="blank")))) %>% 
        
        bs_append(title = "Why is my data preview showing zero rows of data?", 
                  content = tags$div(tags$p("This will happen if the app is unable to match the UNITIDs in your peer list to the UNITIDs in the data file.  Please double check that the UNITID column in your csv peer file does indeed contain NCES UNITIDs for institutions."), 
                                     tags$p("If you need to acquire the UNITIDs for your list of institutions, you can download those IDs from IPEDS by searching for specific institutions here: ",
                                            tags$a(href = "https://nces.ed.gov/ipeds/datacenter/InstitutionByName.aspx",
                                                   "https://nces.ed.gov/ipeds/datacenter/InstitutionByName.aspx.",
                                                   target = "blank")), 
                                     tags$p("If your peer list does contain UNITIDs and you are still seeing zero rows of data, please double check that the institutions in your peer list have data in the IPEDS survey you chose.  For example, if you chose an IPEDS survey relevant for only public institutions, please ensure that your peer list contains public institutions."))) %>% 
        
        bs_append(title = "Can I get data for more than one IPEDS survey?", 
                  content = "Yes! You can download custom subset files for as many of the IPEDS surveys as you like, one at a time.  Each each survey will come down as its own csv file and if you choose to join them together for analysis you may do so.  After you have run through all the steps of the app and downloaded a data file, you can scroll back up to step 2 and select a different survey.  You should then see a new preview table reflecting this change.  Proceed to steps 3 and 4.  You can repeat this process as many times as you like and there is no need to go back to step 1 unless you wish to use a different peer list file. Occationally we have found that GitHub, where the data is stored, has download rate limits. We are working on a solution but in the meantime, if you are experiencing an error please wait about an hour and then try again.") %>% 
        
        bs_append(title ="Can I have access to the files containing all 7,000 institutions?", 
                  content = tags$div("Yes, though they are very large and often difficult to work with.  The complete longitudinal IPEDS files are available for download at ",
                                     tags$a(href = "https://github.com/kaloisio/IPEDS_data/releases",
                                            "https://github.com/kaloisio/IPEDS_data/releases.",
                                            target = "blank")))%>% 
        bs_append(title = "Why I am getting this error: An error has occured. Check your logs or contact the app author for clarification.", 
                  content = tags$div("Occationally we have found that GitHub, where the data is stored, has download rate limits. We are working on a solution but in the meantime, if you are experiencing this error please wait about an hour and then try again. If you continue to get this error please email ", 
                                     tags$a(href= "mailto:iwdapplication@gmail.com?subject=IPEDS%20Compiler%20error", 
                                            "iwdapplication@gmail.com."),  
                                     "We will investigate the situation and do our very best to get the app working for you.")) %>% 
        bs_append(title = "I am getting an error message, what should I do?", 
                  content = tags$div("Please take a screenshot of the error you are receiving and send it to ", 
                                     tags$a(href= "mailto:iwdapplication@gmail.com?subject=IPEDS%20Compiler%20error", 
                                            "iwdapplication@gmail.com."),  
                                     "We will investigate the situation and do our very best to get the app working for you.")) %>% 
        
        bs_append(title = "To whom can I send feedback on this app?", 
                  content = tags$div("This project is still a work in progress and we value the feedback of our users.",
                                     "If you have ideas for how this app could be more useful, please contact ",
                                     tags$a(href= "mailto:iwdapplication@gmail.com",
                                            "iwdapplication@gmail.com."))),
      
      h4("Questions About the Resulting File"),
      br(),
      bs_accordion(id = "faq_data") %>% 
        bs_append("What years of data will be included in my file?", "For each survey we have compiled all years of data that are available for accompanying csv download with dictionary files.  At this point, the resulting csv contains all years of data.  Future releases of the app will likely include the ability for the user to select specific years.") %>% 
        bs_append("When are new data going to be added?",  "IPEDS releases new data at several points throughout the year.  We will update the compiled files accessed by this app twice per year, at the start of fall and spring semesters.  Each time we update the app we will make a note of the 'as of' date on the site so you can easily see the last time we refreshed the data.") %>% 
        bs_append("What will the output file look like?", "When you click the download button you will get a .csv file that you can then open in Excel or read into R, SAS, or other programs.  The file will contain columns for UNITID and ACAD_YEAR to indicate which school and which academic year the data apply. In some survey files, a school may have multiple rows of data per year, depending on the organization of the IPEDS data files.  If your peer file contained additional columns besides UNITID those fields will also be in your resulting download file.") %>% 
        bs_append("How are the column names created?", "Each year of IPEDS data was downloaded with an accompanying dictionary file.  The code that compiles and cleans the data uses the dictionary files to rename the raw IPEDS variables to their more human-readable labels.") %>%
        bs_append("Why are some columns blank in the output file?", "Because this is a longitudinal file it will include fields collected in any of the years, but they not have been collected in all years, which can result in empty cells. Additionally, there may be cases when there were no data for a specific field for that school and year.") %>% 
        bs_append("What is the column called ACAD_YEAR?", "This is a field that we created, which indicates the academic (fiscal) year to which the data apply.  For example ACAD_YEAR of 2018 means the data pertain to school year 2017-18.  This may differ from the year the data were submitted to IPEDS.") %>% 
        bs_append("What is the column called FILE_NAME", "This is a field that we created.  It is the name of the csv that was originally downloaded from the IPEDS website and may look familiar to you if you have downloaded IPEDS data before.  Please note that the year in the file name will not always match ACAD_YEAR.  For example, fall enrollment data comes down with a filename that includes the calendar year of the fall semester (ef2017a.csv is data pertaining to 2017-18, and will have an ACAD_YEAR value of 2018).") %>% 
        bs_append("What is the column called TABLE_TRIM", "This is a field that we created and serves as an indication of which IPEDS survey the data are form, with no reference to the year of the data.  For example, 2017 fall enrollment data files come down as ef2017a.csv and we trim this to EFA to indicate that it is the fall enrollment 'a' survey.  ") %>% 
        bs_append(title = "Where can I direct additional questions about the contents of my data file?", 
                  content = tags$div("Please send an email to",
                                     tags$a(href= "mailto:iwdapplication@gmail.com",
                                            "iwdapplication@gmail.com"),
                                     "with your questions and we will do our very best to help you out.")),
      
      h4("Questions About IPEDS"),
      br(),
      bs_accordion(id = "faq_ipeds") %>%

      bs_append(title = "What is IPEDS data?", 
                content = tags$div("IPEDS stands for Integrated Postsecondary Educational Data System and is data submitted to the National Center for Education Statistics by all institutions of higher education that receive federal funding on a variety of topics including admission, enrollment, financial aid, graduation rates, faculty, and staff. For more information, please see the NCES website: ", 
                                   tags$a(href = "https://nces.ed.gov/ipeds/about-ipeds",
                                          "https://nces.ed.gov/ipeds/about-ipeds",
                                          target = "blank"))) %>%   
        bs_append("How do I know which IPEDS Survey I want?", 
                  content = tags$div("Details on each IPEDS surveys and what data they contain can be found here:",
                                     tags$a(href="https://nces.ed.gov/ipeds/use-the-data/survey-components", 
                                            "https://nces.ed.gov/ipeds/use-the-data/survey-components",
                                            target = "blank"))) %>% 
      
      bs_append("Does the app use Prelimiarny, Provisional, or Final IPEDS data?", "Our app provides Provisional data when it is avalable and Final data otherwise.  The app never uses Preliminary data." ) %>% 
      bs_append("Does subsequent revisions of IPEDS data get picked up by the app?", "The app uses revised IPEDS files when they are avalable.  We update the app with new IPEDS data twice a year (fall and winter).  At these times, we check for both new data and revised versions of previous data files." ), 
      
      
      ) # closes tabpanel
      
    ) # closes tabsetPanel
    
  )# closes mainpanel
  
)# closes fluidpage


#### Server ####

server <- function(input, output){
  
  #download peerlist template file from github
  #write out template peerlist 
  
  #### peerlist template ####
  output$download_peerlist <-  
    downloadHandler(
      filename ="peerlist_template.csv"
      ,
      content = function(file) {
        
        temp <- tempfile()
        pb_download("peerlist_template.csv",
                    repo = "kaloisio/IPEDS_data",
                    tag = version,
                    dest = temp,
                    .token="")
        
        ds_peerlist_template <- read_csv(paste0(temp, "/", "peerlist_template.csv"), col_types = cols(.default="c"))
        unlink(temp, recursive = T)
        
        write_csv(ds_peerlist_template, file, na = "")
      } # closes content function
    ) # closes download handler
  
  #### read in peer file ####
  values <- reactiveValues(unitid_test=data.frame())
  ds_peerlist <- reactive({
    
    req(input$peerlist)
    
    temp <- read_csv(input$peerlist$datapath, col_types = cols(.default="c"))
    names(temp) <- toupper(names(temp))
    
    validate(
      need(!is.null(temp$UNITID),"Please upload a csv with UNITID. See FAQ for an example template."))
    
    temp
  })
  
  observeEvent(ds_peerlist(), {
    if(!is.null(ds_peerlist())) {
      values$unitid_test <- ds_peerlist()
    } else {
      values$unitid_test <- NULL
    }
  })
  
  #### preview peerlist ####
  output$preview_peerlist <- DT::renderDataTable({
  
    
    DT::datatable(values$unitid_test,
                  options = list(
                    pageLength = 5
                  ))
  }) # closes peer renderdatatable
  
  
  output$numpeers <- renderText({
    
    paste("Your peer list contains", prettyNum(n_distinct(ds_peerlist()["UNITID"]), big.mark = ",") ,"institutions.", sep=" ")
    
  }) #closes renderText
  
  #### compile survey ####
  ds_filtered <- eventReactive(input$goButton, {
    
    survey_file <- paste0(input$survey, "_compiled_full.zip")
    
    temp <- tempfile()
    pb_download(survey_file,
                repo = "kaloisio/IPEDS_data",
                tag = version,
                dest = temp,
                .token="")
    
    
    
    ds_full <- read_csv(paste0(temp, "/", survey_file), col_types = cols(.default="c"))
    unlink(temp, recursive = T)
    
    if(is.null(ds_peerlist()$INSTITUTION)){
      ordered_names <- c("UNITID", names(ds_full), names(ds_peerlist()))
      sort_col <- ordered_names[6]
    } else {
      ordered_names <- c("UNITID", "INSTITUTION", names(ds_full), names(ds_peerlist()))
      sort_col <- ordered_names[7]}
    
    # filter data based on peerlist
    if(length(ds_peerlist()$UNITID)==0) {ds <- ds_full}
    else{
      ds <- ds_full %>% filter(UNITID%in%ds_peerlist()$UNITID) %>% left_join(ds_peerlist(), by = "UNITID") %>% 
        select(ordered_names) %>% arrange(!!sym(sort_col))
    }
    return(ds)
  }) # closes eventReactive reading and filtering data
  
  #### render data table ####
  output$preview <- renderDataTable(ds_filtered()[1:10],
                                    options = list(
                                      pageLength = 10
                                    )
  ) # closes renderDataTable
  
  #### Years included in file ####
  output$surveyinfo <- renderText({ 
    paste0("This survey contains academic (fiscal) years from ", min(ds_filtered()$ACAD_YEAR), " to ", max(ds_filtered()$ACAD_YEAR), ". Please see the FAQ for more information about the years contained in this file.")
  })
  
  #### sort file for writing out pretty ####
  ds_sorted <- reactive(ds_filtered() %>% arrange(UNITID, ACAD_YEAR))
  
  #### write out file ####
  output$download <-  
    downloadHandler(
      filename = function() {paste0(input$survey,"_compiled.csv")}
      ,
      content = function(file) {
        write_csv(ds_sorted(), file, na = "")
      }
    ) # closes download handler
}# closes server


# this runs the app
shinyApp(ui = ui, server = server)