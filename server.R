
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
      if (length(input$countries) != 1)
        return()

      temp <- read.csv(getCtryNlDataFnamePath(input$countries), nrows = 1, header = T)
      
      cols <- names(temp)
      
      cols <- cols[-grep("area|NL_", cols)]
      
      #cols <- gsub("_.*", "", cols)
    })
    
    ctryNlData <- reactive({
      ctryData <- NULL

      if (length(input$countries) == 1)
      {
        ctryData <- read.csv(getCtryNlDataFnamePath(input$countries))
      }
      else if(length(input$countries) > 1)
      {
        for (ctryCode in input$countries)
        {
          temp <- read.csv(getCtryNlDataFnamePath(ctryCode))
          
          ctryCols <- grep("country|NL_", names(temp))
          
          temp <- temp[ , ctryCols]
          
          if (is.null(ctryData))
          {
            ctryData <- temp
          }else
          {
            ctryData <- rbind(ctryData, temp)
          }
        }
      }
      return(ctryData)
    })

    output$intraCountry <- renderUI({
      if(is.null(ctryAdmLevels()))
        return()
      
      radioButtons(inputId = "admLevel", 
                     label = "Admin Level", 
                     choices = ctryAdmLevels()
                   )
    })

    output$plotNightLights <- renderPlot({
      if (is.null(ctryNlData()))
        return()
      
      if (length(input$countries) == 1)
      {
        ctryData <- ctryNlData()
        
        meltMeasureVars <- names(ctryData)[grep("NL_", names(ctryData))]
        
        meltVarNames <- gsub("[^[:digit:]]", "", meltMeasureVars)
        
        ctryData <- melt(ctryData, measure.vars=meltMeasureVars)
        
        ggplot(data=ctryData, aes(x=ctryData[,input$admLevel], y=value)) + geom_boxplot() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + facet_wrap(~ variable, ncol = 1)
      }
      else if (length(input$countries) > 1)
      {
        ctryData <- ctryNlData()
        
        meltMeasureVars <- names(ctryData)[grep("NL_", names(ctryData))]
        
        meltVarNames <- gsub("[^[:digit:]]", "", meltMeasureVars)
        
        ctryData <- melt(ctryData, measure.vars=meltMeasureVars)
        
        plotData <- aggregate(value ~ country_code+variable, data=ctryData, mean)
        
        ggplot(data=plotData, aes(x=variable, y=value, group=country_code, col=country_code))+ geom_line()
      }
      else
      {
        return()
      }
    })
    
    output$dataset <- renderTable({
      if(is.null(ctryNlData()))
        return("NO DATA")
      
      ctryNlData()
    })
    
    output$message <- renderText({
      input$countries
    })
})
