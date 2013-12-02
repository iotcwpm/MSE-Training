
library(shiny)
library(ggplot2)

load("plots.RData")

shinyServer(function(input, output) {
  
  # create a subset based on the user selections
	# TEST input <- list(om='rc', sresid=10, beta=0.1, cpuenoise=0)

idx <- reactive({
		idx <- subset(pgridf, OM==input$om & SResid==as.numeric(input$sresid) &
			beta==as.numeric(input$beta) & cpueNoise==as.numeric(input$cpuenoise))
  })
	

	# Render plots here  
    #  plot ONE 
    output$plotONE <- renderPlot({
      print(plots[[idx()$number]]$plotONE)
    }) # END # plot ONE


    #  plot TWO 
    output$plotTWO <- renderPlot({
      print(plots[[idx()$number]]$plotTWO)
    }) # END # plot TWO


    #  plot THREE 
    output$plotTHREE <- renderPlot({
      print(plots[[idx()$number]]$plotTHREE)
    }) # END # plot THREE


    #  plot FOUR 
    output$plotFOUR <- renderPlot({
      print(plots[[idx()$number]]$plotFOUR)
    }) # END # plot FOUR

}) # END shinyServer()


