# Shiny App Template for IR #

# add useful packages
pkgs <- c("tidyverse", "shiny", "piggyback", "shinycssloaders")
for(pkg in pkgs) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
  library(pkg, character.only = TRUE)
}


# UI defines what the end user sees.
# Fluid Page is where you set the layout of the page
# indicate what user controls to show and where to render the results

ui <- fluidPage(
   # Application title
  titlePanel("IPEDS Data Compiler"),
  
  # indicate layout (sidebar example -- can choose other styles)
  #inside sidebar layout -- sidebar panel and main panel
  sidebarLayout(
    sidebarPanel(
      h3("Step 1: Select your peerlist"),
      # peerlist upload
      fileInput("peerlist", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ), # closes fileinput
      
      h3("Step 2: Select a Survey"),
      # select survey
      selectInput('survey', 'Choose a Survey:', choices = list(
        Admission = c(`Applications, admissions, enrollees and test scores` = 'adm'),
        `Fall Enrollement` = c(`Race/ethnicity, gender, attendance status, and level of student`= "efa", 
                               `Age category, gender, attendance status, and level of student` = "efa", 
                               `Residence and migration of first-time freshman` = "efc",
                               `Total entering class, retention rates, and student-to-faculty ratio` = "efc",
                               `Distance education status and level of student` = "efdist"),
        Completions = c(`Awards/degrees conferred by program (6-digit CIP code), award level, race/ethnicity, and gender`= 'c_a',
                        `Number of students receiving awards/degrees, by race/ethnicity and gender` = 'c_b',
                        `Number of students receiving awards/degrees, by award level and by gender, race/ethnicity and age categories` = 'c_c',
                        `Number of programs offered and number of programs offered via distance education, by award level` = 'c_dep')
      ), selectize = FALSE), 
      
      # run button
      h3("Step 3: Compile the table"),
      
      actionButton("goButton", "Dominate the World!"
      ), # closes actionbutton
     
      h3("Step 4: Wait patiently =)"),
      # download button
      
      h3("Step 5: Once the table shows download the clean CSV"),
      
      downloadButton("download", "Download CSV")
      
      ),# closes sidebarPanel
   
    #also inside sidebarlayout -- main panel contains outputs, separated by commas
     mainPanel(
       
       # number of peer institutions 
       textOutput("numpeers"),
       # table of results, unitid, year, surveyname, sampling of variables
       
       # use html to add line breaks, etc
       br(),
       
       #### show the user a preview table of the first XX rows of data ####
       dataTableOutput("preview") %>% withSpinner(color="#0dc5c1") #,
       
       # where should this save
       
       # # download button
       # 
       # downloadButton("download", "Download CSV")
       # 
       # notes
       
       # information button to take them to NCES - for survey descriptions
       
       )# closes mainPanel
    
    )# closes sidebarLayout
  
  )# closes fluidpage



#server function is where you do all the stuff -- define text, create plot, or filter data etc.
# define items as output$ named the things you call up in the ui
# use input$ in creation of outputs

server <- function(input, output){
  
  # read in peer file
  

  
  # if (is.null(ds_peerlist))
  #   return(NULL)
  
  ds_peerlist <- reactive({
    
    if (is.null(input$peerlist))
      return(NULL)
    
  read_csv(input$peerlist$datapath) 
  
    #  names(temp) <- toupper(names(temp))
    #  if (!("UNITID"%in%names(ds_peerlist()))) { "Peerlist must include a column labeled UNITID."}
  })
  
  
   # text for number of institutions
 output$numpeers <- renderText({
   if(is.null(input$peerlist)){
     return("Currently there are over 7,000 institutions submitting to IPEDS. Please upload a peer list to filter the survey.")}
   else {
   paste("Your peer list contains", prettyNum(n_distinct(ds_peerlist()["UNITID"]), big.mark = ",") ,"institutions.", sep=" ")}
   })
 
  
  
  
  ds_filtered <- eventReactive(input$goButton, {

  survey_file <- paste0(input$survey, ".csv")
  version <- "v0.0.2"

  temp <- tempfile()
  pb_download(survey_file,
              repo = "kaloisio/IPEDS_data",
              tag = version,
              dest = temp,
              .token="")
  


  ds_full <- read_csv(paste0(temp, "/", survey_file))
  unlink(temp, recursive = T)
  
  # filter data based on peerlist
  ds <- ds_full %>% filter(UNITID%in%ds_peerlist()$UNITID)

  }) # closes eventReactive reading and filtering data

  # render data table
 
  output$preview <- renderDataTable(ds_filtered(),
                                    options = list(
                                      pageLength = 5)
  )
  
  # write out file 
  output$download <- ## renderUI({
    ## if(!is.null(input$goButton)) {
      downloadHandler(
    filename = paste0(input$survey,"_compiled.csv")
    ,
    content = function(file) {
      write_csv(ds_filtered(), file)
    }
      ) # closes download handler
   ##   } # closes if statment
  ##}) # closes renderUI

}# closes server


# this runs the app
shinyApp(ui = ui, server = server)