
#### add useful packages ####
library(tidyverse)
library(shiny)
library(piggyback)
library(shinycssloaders)
library(DT)
library(curl)
library(shinythemes)

#### version from IPEDS_data ####
version <- "v0.0.2"


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
               
               h3("Welcome to the IPEDS Data Compiler. Follow the steps below to Dominate the World. "),
               br(),
               
               #### Step 1: Peerlist ####
               h4(tags$b("Step 1:"), "Upload your peerlist as a csv. Please make sure it contains NCES ID in a column called UNITID."),
               p("See the FAQ for an example peerlist."), 
               
               
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
                                   `Number of programs offered and number offered via distance education, by level` = 'c_dep'),
                 
                 `Graduation Rates` = c(`Graduation rate data, 150% of normal time - 2 and 4 year institutions` = 'gr',
                                        `Graduation rate data, 150% of normal time - less-than-2-year institutions` = 'gr_l2',
                                        `Graduation rate data, 200% of normal time - 4 year and less-than-4-year institutions` = 'gr_200',
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
               
               #### Step 4: Download ####
               h4(tags$b("Step 4:"), "Once the table renders, press the button below to download your compiled csv file."),
               
               downloadButton("download", "Download CSV")
               
      ), # closes compiler tabPanel
      
      #### FAQ ####     
      tabPanel("FAQ",
               
               h3("What is this app? What does it do?"),
               p("Select Download Peerlist Template for an example peerlist."),
               
               downloadButton("download_peerlist", "Download Peerlist Template")) 
      
      
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
        
        ds_peerlist_template <- read_csv(paste0(temp, "/", "peerlist_template.csv"))
        unlink(temp, recursive = T)
        
        write_csv(ds_peerlist_template, file, na = "")
      } # closes content function
    ) # closes download handler
  
  #### read in peer file ####
  values <- reactiveValues(unitid_test=data.frame())
  ds_peerlist <- reactive({
    
    req(input$peerlist)
    
    temp <- read_csv(input$peerlist$datapath)
    
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
  })
  
  
  
  #### text for number of institutions ####
  # output$numpeers <- renderText({
  #   if(is.null(ds_peerlist())){
  #     return("Currently there are over 7,000 institutions submitting to IPEDS. Please upload a peer list to filter the survey.")}
  #   else {
  #     paste("Your peer list contains", prettyNum(n_distinct(ds_peerlist()["UNITID"]), big.mark = ",") ,"institutions.", sep=" ")
  #   }
  # })
  
  output$numpeers <- renderText({
    
    paste("Your peer list contains", prettyNum(n_distinct(ds_peerlist()["UNITID"]), big.mark = ",") ,"institutions.", sep=" ")
    
  })
  
  #### compile survey ####
  ds_filtered <- eventReactive(input$goButton, {
    
    survey_file <- paste0(input$survey, ".csv.zip")
    
    temp <- tempfile()
    pb_download(survey_file,
                repo = "kaloisio/IPEDS_data",
                tag = version,
                dest = temp,
                .token="")
    
    
    
    ds_full <- read_csv(paste0(temp, "/", survey_file))
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
  )
  
  output$selected_survey <- renderText({ 
    paste("You have selected", input$survey)
  })
  
  
  #### write out file ####
  output$download <-  
    downloadHandler(
      filename = function() {paste0(input$survey,"_compiled.csv")}
      ,
      content = function(file) {
        write_csv(ds_filtered(), file, na = "")
      }
    ) # closes download handler
}# closes server


# this runs the app
shinyApp(ui = ui, server = server)