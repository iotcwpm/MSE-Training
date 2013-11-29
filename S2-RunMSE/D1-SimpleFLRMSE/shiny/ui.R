
library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Iago give a header"),
  
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
      
      selectInput("slopeyears", "slope Years",
                  list("5" = "5", 
                       "10" = "10")),
      
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
        tabPanel("plot ONE", 
            h4(" label for plot ONE"), 
            plotOutput("plotONE", height="250px")
        ),
        tabPanel("plot TWO", 
                 h4(" label for plot TWO"), 
                 plotOutput("plotTWO", height="250px")
        ),
        tabPanel("plot THREE", 
                 h4(" label for plot THREE"), 
                 plotOutput("plotTHREE", height="250px")
        ),
        tabPanel("plot FOUR", 
                 h4(" label for plot FOUR"), 
                 plotOutput("plotFOUR", height="250px")
        )
    )# END tabsetPanel()
   )# END mainPanel()
  ) # END pageWithSidebar() 
  
) # END shinyUI()
        