
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(reshape)

source("nightlights.R")

shinyServer(function(input, output) {

    ctryAdmLevels <- reactive({
      if (length(input$countries) == 0)
        return()

      temp <- read.csv(getCtryNlDataFnamePath(input$countries), nrows = 1, header = T)
      
      cols <- names(temp)
      cols <- cols[grep("name|code", cols)]
      cols <- gsub("_.*", "", cols)
    })
    
    ctryNlData <- reactive({
      if (is.null(ctryAdmLevels))
        return()
      
      ctryData <- NULL

      for (ctryCode in input$countries)
      {
        temp <- read.csv(getCtryNlDataFnamePath(ctryCode))
        ctryData <- rbind(ctryData, temp)
      }
    })

  
    output$intraCountry <- renderUI({
      if(is.null(ctryAdmLevels()))
        return()
      
      radioButtons(inputId = "admLevel", 
                     label = "Admin Level", 
                     choices = ctryAdmLevels()
                   )
    })
    

    output$plotInterCountry <- renderPlot({
      if (is.null(ctryNlData()))
        return()
      
      ctryData <- ctryNlData()
      
      meltMeasureVars <- names(ctryData)[grep("NL_", names(ctryData))]
      
      meltVarNames <- gsub("[^[:digit:]]", "", meltMeasureVars)
      
      ctryData <- melt(ctryData, measure.vars=meltVarNames)
      
      plotData <- aggregate(value ~ country_code+variable, data=ctryData, mean)

      ggplot(data=plotData, aes(x=country_code, y=value)) + geom_boxplot()
    })
  
    output$dataset <- renderTable({
      if(is.null(ctryNlData()))
        return()
      
      ctryNlData()
    })
    
    
#   output$interCountry <- renderUI({
#     if (length(input$countries) > 1)
#     {
#       admLevels <- getCtryPolyAdmLevelNames(input$countries)
#       
#       radioButtons(inputId = "admLevels", choices = admLevels, selected = admLevels[1])
#     }
#   })
})
