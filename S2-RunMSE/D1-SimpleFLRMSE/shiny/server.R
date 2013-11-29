
library(shiny)
#load("./res.RData")

shinyServer(function(input, output) {
  
  
# create a subset based on the user selections
  sub <- subset(res, OM=input$om, SResid=as.numeric(input$sresid), beta=as.numeric(input$beta),
                slopeYears=as.numeric(input$slopeyears), cpueNoise=as.numeric(input$cpuenoise))

# use a function to plot  
 plot.ONE <- function(df){
    ggplot(df, aes(year, data)) + geom_point()
 } 

# Render plots here  
    #  plot ONE 
    output$plotONE <- renderPlot({
      print(plot.ONE(sub))
       
    }) # END # plot ONE


    #  plot TWO 
    output$plotTWO <- renderPlot({
  
  
    }) # END # plot TWO


    #  plot THREE 
    output$plotTHREE <- renderPlot({
  
  
    }) # END # plot THREE


    #  plot FOUR 
    output$plotFOUR <- renderPlot({
  
  
    }) # END # plot FOUR


}) # END shinyServer()


