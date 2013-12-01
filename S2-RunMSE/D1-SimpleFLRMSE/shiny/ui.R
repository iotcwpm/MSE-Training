
library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("IOTC MSE - CPUE HCR"),
  
  # Sidebar with controls 
  sidebarPanel(
      # Select Params 
      # SECOND VALUE IS HOW IT SHOWS ON THE SCREEN. USE FIRST VALUE FOR INTERNAL USE IN SERVER.R
      selectInput("om", "om",
                list("rc" = "rc", 
                     "ow" = "ow", 
                     "ed" = "ed")),
      
      selectInput("sresid", "SResid",
                  list("10" = "10", 
                       "20" = "20", 
                       "40" = "40")),
      
      selectInput("beta", "beta",
                  list("0.1" = "0.1", 
                       "0.2" = "0.2", 
                       "0.4" = "0.4")),
      
      selectInput("cpuenoise", "CPUE noise",
                  list("0" = "0", 
                       "10" = "10", 
                       "20" = "20"))      
  
    ), # END sidebarPanel()
  

#  conditionalPanel(
#    condition = "input.om == 'rc'",    
#    h4(" ... ")
#    ...  
#  ), # END conditionalPanel 
  
  
    mainPanel(
      # main panel with 4 tabs
      tabsetPanel(
        tabPanel("Stock Trajectory", 
            h4("Stock Trajectory"), 
            plotOutput("plotONE", height="500px", width="700px")
        ),
        tabPanel("Probabilities", 
                 h4("Probailities for Target and Limits"), 
                 plotOutput("plotTWO", height="500px", width="500px")
        ),
        tabPanel("plotTHREE", 
                 h4("Label for plotTHREE"), 
                 plotOutput("plotTHREE", height="500px", width="700px")
        ),
        tabPanel("Catch stability", 
                 h4("Yearly changes in TAC"), 
                 plotOutput("plotFOUR", height="500px", width="700px")
        )
    )# END tabsetPanel()
   )# END mainPanel()
  ) # END pageWithSidebar() 
  
) # END shinyUI()
        
