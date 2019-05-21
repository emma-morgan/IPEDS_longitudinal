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
all <- read.csv("https://raw.githubusercontent.com/emmamorgan-tufts/IPEDS_longitudinal/master/data/header_compiled.csv", check.names = F, stringsAsFactors = F)
header <- all %>% mutate(`Carnegie Classification 2000` = ifelse(`Carnegie Classification 2000` == '{Item not available}', "Not Available", `Carnegie Classification 2000`))

# UI defines what the end user sees.
# Fluid Page is where you set the layout of the page
# indicate what user controls to show and where to render the results

ui <- fluidPage(
  
  
  # Application title
  titlePanel("IPEDS Peer File Selection", windowTitle = "IPEDS Peers"),
  
  fluidRow(
    column(3, 
           #### select institutions based on classification 2000 ####
           checkboxGroupInput("classification", label = "Choose classification(s) (Carnegie 2000):",
                              choices = levels(as.factor(header$`Carnegie Classification 2000`))),
           
           ####  select based on geographic region   ####
           checkboxGroupInput("region", label = "Choose region(s):",
                              choices = levels(as.factor(header$`Bureau of Economic Analysis (BEA) regions`))) 
           
           ),
    
    column(3,
           #### select based on institution size ####
           checkboxGroupInput("size", label = "Choose institution size(s):",
                              choices = c("Under 1,000", "1,000 - 4,999", "5,000 - 9,999", "10,000 - 19,999", "20,000 and above",
                                          "Not applicable", "Not reported"))
           
           
           ),
    
    column(6,
           #### tell user how many rows are in dataset ####
           # test output total number in schools selected = xx 
           h3(textOutput("numrows")),
           
           # use html to add line breaks, etc
           br(),
           
           #### show the user a preview table of the first XX rows of data ####
           dataTableOutput("preview"),
           
           #### download csv button creation ####
           downloadButton("download", "Download CSV")
           
           )
    
  )
  
 
      
      # inputs - 
          # select peers either by name or inst characteristics
      
      
      
      
      
      
      ## "Control of institution"
      
      
      
        
      
   
    #also inside sidebarlayout -- main panel contains outputs, separated by commas
     
       
      

       #outputs --
          # list of resulting data in the subset
      
       
       #function(outputID = "OUT2", ...),
       #function(outputID = "OUT3", ...)
   


    )
    
   



#server function is where you do all the stuff -- define text, create plot, or filter data etc.
# define items as output$ named the things you call up in the ui
# use input$ in creation of outputs

server <- function(input, output) {
  
  #### filter header given choices ####
  
  # if input$classification <1 then 
  
  
  
  # filter data based on inputs
  header_subset <- reactive({
    sel_class <- if (is.null(input$classification)) c(levels(as.factor(header$`Carnegie Classification 2000`))) else c(input$classification)
    sel_regions <- if (is.null(input$region)) c(levels(as.factor(header$`Bureau of Economic Analysis (BEA) regions`))) else c(input$region)
    sel_size <- if (is.null(input$size)) c(levels(as.factor(header$`Institution size category`))) else c(input$size)
    
    #req(input$classification)
    header %>%
    dplyr::filter(`Carnegie Classification 2000`%in%sel_class, 
                  `Bureau of Economic Analysis (BEA) regions`%in%sel_regions,
                  `Institution size category` %in% sel_size) %>% 
      dplyr::select(UNITID, `Institution (entity) name`, `Institution size category`, `City location of institution`, `FIPS state code`, `Bureau of Economic Analysis (BEA) regions`,
                    `Carnegie Classification 2000`, `Control of institution`)
  })

  #### preview of data table (limit items per page) ####
  output$preview <- renderDataTable(header_subset(),
                                    options = list(
                                      pageLength = 5)
  )
  
  
  
 
  #### output sentence with number of rows in peerlist ####
  output$numrows <- renderText(paste("Your peer list contains", prettyNum(nrow(header_subset()), big.mark = ",") ,"institutions.", sep=" "))
  
  #### output file write out after hitting button #### 
  output$download <- downloadHandler(
    filename = "peerlist.csv"
    ,
    content = function(file) {
      write_csv(header_subset(), file)
    }
  )

  
  
  
}


#this runs the app
shinyApp(ui = ui, server = server)