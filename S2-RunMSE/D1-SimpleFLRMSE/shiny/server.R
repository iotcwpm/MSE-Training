
library(shiny)

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
      print(pres[[idx()$number]]$plotONE)
    }) # END # plot ONE


    #  plot TWO 
    output$plotTWO <- renderPlot({
      print(pres[[idx()$number]]$plotTWO)
    }) # END # plot TWO


    #  plot THREE 
    output$plotTHREE <- renderPlot({
      print(pres[[idx()$number]]$plotTHREE)
    }) # END # plot THREE


    #  plot FOUR 
    output$plotFOUR <- renderPlot({
      print(pres[[idx()$number]]$plotFOUR)
    }) # END # plot FOUR

}) # END shinyServer()


