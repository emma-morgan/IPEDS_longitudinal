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
  
  # Layout 
  #inside sidebar layout -- sidebar panel and main panel
  sidebarLayout(
    sidebarPanel(
      # inputs - 
          # select peers either by name or inst characteristics
         
      #### download csv button creation ####
       downloadButton("download", "Download CSV")
      #function(inputID = "IN3", ...)
      ),
   
    #also inside sidebarlayout -- main panel contains outputs, separated by commas
     mainPanel(
       
      

       #outputs --
          # list of resulting data in the subset
          # test output total number in schools selected = xx

       # show the user a preview table of the first XX rows of data
       # dataTableOutput(outputID = "preview"),

       # use html to add line breaks, etc
       # br()

       #function(outputID = "OUT2", ...),
       #function(outputID = "OUT3", ...)
       )
    
    )
  
  )



#server function is where you do all the stuff -- define text, create plot, or filter data etc.
# define items as output$ named the things you call up in the ui
# use input$ in creation of outputs

server <- function(input, output) {
  
  #### filter header given choices ####
  # filter data based on inputs
  header_subset <- header #%>%
    #filter(input$choices)

  # # preview of data table (limit items per page)
  # #output$preview <- renderDataTable(input$xxxxx)
  # 
  # # example dynamic text object
  # #output$greeting <- renderText(paste("Hello, ", input$name))
  
  #### output file write out after hitting button #### 
  output$download <- downloadHandler(
    filename = "peerlist.csv"
    ,
    content = function(file) {
      write_csv(header_subset, file)
    }
  )

  
  
  #output$OUT3 <- function(input$IN3)
  
  
}


#this runs the app
shinyApp(ui = ui, server = server)