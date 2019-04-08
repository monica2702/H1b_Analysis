#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
# #


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$interaction_slider <- renderUI({
    compData <- computeData(h1b,input$status,input$employer)
    minSal <- min(compData$PREVAILING_WAGE)
    maxSal <- max(compData$PREVAILING_WAGE)
    
    sliderInput("salary",label = h4("Salary Range: "), 
                min = minSal,
                max = maxSal,
                value = c(minSal,maxSal))
  })
  
  
  
  
  output$yearPlot <- renderPlot({
    
    # Render a barplot
    
    plotedData <<- plotData(h1b,input$status,input$employer,input$salary[1],
                           input$salary[2])
    # years <<- row.names(plotedData)
    # compData <<- computeData(h1b,input$status,input$employer,input$salary[1],
    #                         input$salary[2])
    # View(compData)
    if(is.table(plotedData) & nrow(plotedData) != 0 & length(names(plotedData)) != 0){
      years <<- row.names(plotedData)
      compData <<- computeData(h1b,input$status,input$employer,input$salary[1],
                               input$salary[2])
      output$debug <- renderText({
        "Result Computed"
      })
      return(barplot(plotedData,col = "pink", main ="Year Wise Count of Applicants",
                     xlab = "Year",
                     ylab = "Number of Applicants"))
      } else {
      output$debug <- renderText({
        "No value available"
      })
      print("returning empty table")
      return(barplot(table(0)))
    }
    
  })
  
  # 
  output$hoverText<- renderText({
    if(!is.null(input$plotHover)){
      hoverData <- round(input$plotHover$x)
      if(hoverData > nrow(plotedData)){
        output$year_table <- renderDataTable(
          dataForUIFrame(compData,years[hoverData-1]),
          options = list(
            pageLength = 5
          )
        )
        return(paste("Val : ",plotedData[hoverData-1]))
        
      }else if(hoverData ==0 & nrow(plotedData) != 0){
        output$year_table <- renderDataTable(
          dataForUIFrame(compData,years[1]),
          options = list(
            pageLength = 5
          )
        )
        return(paste("Val: ", plotedData[1]))
      }
      output$year_table <- renderDataTable(
        dataForUIFrame(compData,years[hoverData]),
        options = list(
          pageLength = 5
        )
      )
      output$jobPiePLot <- renderPlot(
        {
          return(pie(
            jobPieData(
              compData,
              years[hoverData]
              ), col = rainbow(10) )
          )
          }
        )
      return(paste("Val: ",plotedData[hoverData]))
      
    }
  })
  output$testText <- renderText("GIS Representation of Total Filing in US States")
  
  ### prediction codes
  pRes <- eventReactive(input$predict, {
    return(predict_chances(input$pEmployer,input$pSoc,input$pSal,input$pLoc))
  })
  
  output$predictionResult <- renderText({
    pRes()
  })
  
  output$sucessGauge <- renderGauge({
    val <- pRes()
    val <- round(val)
    val <- as.integer(val)
    cat("Val :", val, " Type: ", typeof(val))
    gauge(val,0,100)
    }) 
  
  ###################Map codes
  
  output$heatMap <-renderPlot({
    compData <- computeData(h1b,input$status,NULL,input$salary[1],
                            input$salary[2])
    # heatMap(compData)
    HeatMap_Fun(compData)
  })
  output$bubbleMap <-renderLeaflet({
    compData <- computeData(h1b,input$status,NULL,input$salary[1],
                            input$salary[2])
    bubbMap(compData)
    })
}

)


