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
  titlePanel("IPEDS Data Compiler"),
  
  # indicate layout (sidebar example -- can choose other styles)
  #inside sidebar layout -- sidebar panel and main panel
  sidebarLayout(
    sidebarPanel(
      #add user input controls here - slider, radio, selectinput -- separate them with commas
      
      # peerlist upload
      
      # select survey
      
      # run button
    
      ),# closes sidebarPanel
   
    #also inside sidebarlayout -- main panel contains outputs, separated by commas
     mainPanel(
       
       # number of peer institutions 
       
       # table of results
       
       # notes

       )# closes mainPanel
    
    )# closes sidebarLayout
  
  )# closes fluidpage



#server function is where you do all the stuff -- define text, create plot, or filter data etc.
# define items as output$ named the things you call up in the ui
# use input$ in creation of outputs

server <- function(input, output){
  
  # read in peer file
  
  # text for number of institutions
  
  # survey links based on user inputs
  
  # complie data
  
  # render table
  
  
  
}# closes server


# this runs the app
shinyApp(ui = ui, server = server)