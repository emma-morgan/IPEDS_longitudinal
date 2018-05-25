# Shiny app for selecting IPEDS peer group#

# add useful packages
pkgs <- c("tidyverse", "shiny")
for(pkg in pkgs) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
  library(pkg, character.only = TRUE)
}

#### read in IPEDS header file from github ####
header <- read.csv("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/data/header_compiled.csv", check.names = F, stringsAsFactors = F)


# UI defines what the end user sees.
# Fluid Page is where you set the layout of the page
# indicate what user controls to show and where to render the results

ui <- fluidPage(
  
  # Application title
  titlePanel("IPEDS Peer File Selection", windowTitle = "IPEDS Peers"),
  
  # indicate layout (sidebar example -- can choose other styles)
  #inside sidebar layout -- sidebar panel and main panel
  sidebarLayout(
    sidebarPanel(
      #add user input controls here - allow them to select their peers. also a "download data" button
      #function(inputID = "IN2", ...),
      #function(inputID = "IN3", ...)
      ),
   
    #also inside sidebarlayout -- main panel contains outputs, separated by commas
     mainPanel(
       
       # show the user a preview table of the first XX rows of data
       tableOutput(outputID = "preview"),
      
       # use html to add line breaks, etc
       br(),
       
       # example output dynamic text
       textOutput("greeting")
       #function(outputID = "OUT2", ...),
       #function(outputID = "OUT3", ...)
       )
    
    )
  
  )



#server function is where you do all the stuff -- define text, create plot, or filter data etc.
# define items as output$ named the things you call up in the ui
# use input$ in creation of outputs

server <- function(input, output){
  # preview of data table -- HOW to limit to XX rows??
  output$preview <- renderTable(input$xxxxx)
  
  # example dynamic text object
  output$greeting <- renderText(paste("Hello, ", input$name))
  
  #output$OUT2 <- function(input$IN2)
  #output$OUT3 <- function(input$IN3)
  
  
}


#this runs the app
shinyApp(ui = ui, server = server)