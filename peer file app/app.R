# Shiny App Template for IR #

# add useful packages
pkgs <- c("tidyverse", "shiny")
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
  titlePanel("title here"),
  
  # indicate layout (sidebar example -- can choose other styles)
  #inside sidebar layout -- sidebar panel and main panel
  sidebarLayout(
    sidebarPanel(
      #add user input controls here - slider, radio, selectinput -- separate them with commas
      
      # example user text input
      textInput(inputId = "name", "Enter your name here")
      
      #function(inputID = "IN2", ...),
      #function(inputID = "IN3", ...)
      ),
   
    #also inside sidebarlayout -- main panel contains outputs, separated by commas
     mainPanel(
       
       # example output static text
       textOutput("testtext"),
      
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
  # example static text object
  output$testtext <- renderText("this is our test shiny app")
  
  # example dynamic text object
  output$greeting <- renderText(paste("Hello, ", input$name))
  
  #output$OUT2 <- function(input$IN2)
  #output$OUT3 <- function(input$IN3)
  
  
}


#this runs the app
shinyApp(ui = ui, server = server)