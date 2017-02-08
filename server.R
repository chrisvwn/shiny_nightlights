
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

  yrs <- getAllNlYears("VIIRS")
    
    ctryAdmLevels <- reactive({
      if (length(input$countries) != 1)
        return()

      temp <- read.csv(getCtryNlDataFnamePath(input$countries), nrows = 1, header = T)
      
      cols <- names(temp)
      
      cols <- cols[-grep("area|NL_", cols)]
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
          
          ctryCols <- grep("country|area|NL_", names(temp))
          
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

    output$sliderNlYearMonthRange <- renderUI({
      if (is.null(ctryNlData()))
      {
        sliderInput(inputId = "nllYearMonthRange",
                    label = "Time",
                    min = as.Date("2012-04-01", "%Y-%m-%d"),
                    max = as.Date("2016-12-31", "%Y-%m-%d"),
                    timeFormat = "%Y-%m",
                    step = 1,
                    value = c(as.Date("2012-01-01","%Y-%m-%d"),as.Date("2016-12-31","%Y-%m-%d")),
        )
      }
      else
      {
        nlCols <- names(ctryNlData())
        nlYearCols <- gsub("[^[:digit:]]", "", nlCols[grep("NL_VIIRS", nlCols)])
        nlYearCols <- sapply(nlYearCols, function(x) paste0(x,"01"))
        nlYearCols <- as.Date(nlYearCols, "%Y%m%d")
        
        
        minDate <- min(nlYearCols)
        maxDate <- max(nlYearCols)
                           
        sliderInput(inputId = "nlYearMonthRange",
                    label = "Time",
                    min = minDate,
                    max = maxDate,
                    timeFormat = "%Y-%m",
                    step = 1,
                    value = c(minDate, maxDate)#,
        )
      }
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

        ctryData$variable <- sapply(ctryData$variable, function(x) {paste0(gsub("[^[:digit:]]","", x),"01")})

        ctryData$variable <- as.Date(ctryData$variable, format="%Y%m%d")
        
        ctryData <- subset(ctryData, variable >= input$nlYearMonthRange[1] & variable <= input$nlYearMonthRange[2])
        
        if (input$norm_area)
          ctryData$value <- (ctryData$value*10e4)/ctryData$area_sq_km
        
      }
      else if (length(input$countries) > 1)
      {
        ctryData <- ctryNlData()
        
        meltMeasureVars <- names(ctryData)[grep("NL_", names(ctryData))]
        
        meltVarNames <- gsub("[^[:digit:]]", "", meltMeasureVars)
        
        ctryData <- melt(ctryData, measure.vars=meltMeasureVars)

        ctryData$variable <- sapply(ctryData$variable, function(x) {paste0(gsub("[^[:digit:]]","", x),"01")})

        ctryData$variable <- as.Date(ctryData$variable, format="%Y%m%d")
        
        ctryData <- subset(ctryData, variable >= input$nlYearMonthRange[1] & variable <= input$nlYearMonthRange[2])
        
        if (input$norm_area)
          ctryData$value <- (ctryData$value*10e4)/ctryData$area_sq_km
      }
      else
      {
        return()
      }


      if (input$graphtype == "boxplot")
      {
        if (length(input$countries)==1)
        {
          g <- ggplot(data=ctryData, aes(x=factor(variable), y=value, col=ctryData[,input$admLevel])) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + labs(col=input$admLevel)
        }
        else
        {
          g <- ggplot(data=ctryData, aes(x=factor(variable), y=value, col=country_code))
        }
        
        g <- g + geom_boxplot()# +facet_grid(.~variable)
      }
      else if (input$graphtype == "line")
      {
        if (length(input$countries)==1)
        {
          ctryData <- setNames(aggregate(ctryData$value, by=list(ctryData[,input$admLevel], ctryData[,"variable"]), mean, na.rm=T), c(input$admLevel, "variable", "value"))
          
          g <- ggplot(data=ctryData, aes(x=variable, y=value, col=ctryData[,input$admLevel])) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + labs(col=input$admLevel)
        }
        else
        {
          ctryData <- aggregate(value ~ country_code+variable, data=ctryData, mean)
          g <- ggplot(data=ctryData, aes(x=variable, y=value, col=country_code))
        }

        g <- g+ geom_line(size=1.1) + geom_point()
      }
      else if (input$graphtype == "histogram")
      {
        #ctryData <- aggregate(value ~ country_code+variable, data=ctryData, mean)
        
        g <- ggplot(data=ctryData, aes(x=value))
        
        g <- g + geom_histogram(aes(y=..density..), bins = 100, colour="black", fill="white") + geom_density(alpha=.2, fill="#FF6666") + facet_grid(country_code~.) # Overlay with transparent density plot

      }

      if (input$scale_y_log)
        g <- g + scale_y_log10()
      
      g
    })
    
    output$dataset <- DT::renderDataTable({
      if(is.null(ctryNlData()))
        return("NO DATA")
      
      ctryNlData()
      },
      
      options = list(scrollX = TRUE, scrolly = TRUE)
    )
    
    output$message <- renderText({
      input$countries
    })
})
