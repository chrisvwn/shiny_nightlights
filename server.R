
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

if (!require("pacman")) install.packages('pacman', repos='http://cran.r-project.org')

pacman::p_load(shiny, ggplot2, leaflet, reshape, rgdal, RColorBrewer)

library(shiny)
library(ggplot2)
library(leaflet)
library(reshape)
library(rgdal)
library(RColorBrewer)

source("nightlights.R")

shinyServer(function(input, output, session) {

  yrs <- getAllNlYears("VIIRS")
  
  #isolate({updateTabItems(session, "inputs", "plotNightLights")})
    
    ctryAdmLevels <- reactive({
      if (length(input$countries) != 1)
        return()

      temp <- read.csv(getCtryNlDataFnamePath(input$countries), nrows = 1, header = T)
      
      cols <- names(temp)
      
      cols <- cols[-grep("area|NL_", cols)]
    })
    
    ctryNlData <- reactive({
      
      input$btnDone
      
      countries <- isolate(input$countries)

      if (length(countries)<=0)
        return()
            
      ctryData <- NULL

      if (length(countries) == 1)
      {
        ctryData <- read.csv(getCtryNlDataFnamePath(countries))
      }
      else if(length(countries) > 1)
      {
        for (ctryCode in countries)
        {
          temp <- read.csv(getCtryNlDataFnamePath(ctryCode))
          
          ctryCols <- grep("country|area|NL_", names(temp))
          
          temp <- temp[ , ctryCols]
          
          if (is.null(ctryData))
          {
            ctryData <- temp
          }else
          {
            ctryData <- merge(ctryData, temp, all=TRUE)
          }
        }
      }
      
      meltMeasureVars <- names(ctryData)[grep("NL_", names(ctryData))]
      
      meltVarNames <- gsub("[^[:digit:]]", "", meltMeasureVars)
      
      ctryData <- melt(ctryData, measure.vars=meltMeasureVars)
      
      ctryData$variable <- sapply(ctryData$variable, function(x) {paste0(gsub("[^[:digit:]]","", x),"01")})
      
      ctryData$variable <- as.Date(ctryData$variable, format="%Y%m%d")
      
      print("here:ctrydata")
      
      return(ctryData)
    })

    output$intraCountry <- renderUI({
      if(length(input$countries) != 1)
        return()
      
      radioButtons(inputId = "admLevel", 
                     label = "Admin Level", 
                     choices = ctryAdmLevels()
                   )
    })

    output$sliderNlYearMonthRange <- renderUI({
      
      ctryData <- ctryNlData()
      
      if (is.null(ctryData))
      {
        sliderInput(inputId = "nlYearMonthRange",
                    label = "Time",
                    min = as.Date("2012-04-01", "%Y-%m-%d"),
                    max = as.Date("2016-12-31", "%Y-%m-%d"),
                    timeFormat = "%Y-%m",
                    step = 1,
                    value = c(as.Date("2012-01-01","%Y-%m-%d"),as.Date("2016-12-31","%Y-%m-%d"))
        )
      }
      else
      {
        minDate <- min(ctryData$variable)
        maxDate <- max(ctryData$variable)
                           
        sliderInput(inputId = "nlYearMonthRange",
                    label = "Time",
                    min = minDate,
                    max = maxDate,
                    timeFormat = "%Y-%m",
                    step = 1,
                    value = c(minDate, maxDate)
        )
      }
    })
    
    output$sliderNlYearMonth <- renderUI({
      
      ctryData <- ctryNlData()
      
      if (is.null(ctryData))
      {
        sliderInput(inputId = "nlYearMonth",
                    label = "Time",
                    min = as.Date("2012-04-01", "%Y-%m-%d"),
                    max = as.Date("2016-12-31", "%Y-%m-%d"),
                    timeFormat = "%Y-%m",
                    step = 1,
                    value = as.Date("2012-04-01", "%Y-%m-%d")
        )
      }
      else
      {
        minDate <- min(ctryData$variable)
        maxDate <- max(ctryData$variable)
        
        sliderInput(inputId = "nlYearMonth",
                    label = "Time",
                    min = minDate,
                    max = maxDate,
                    timeFormat = "%Y-%m",
                    step = 1,
                    value = minDate
        )
      }
    })
    
    
    output$plotNightLights <- renderPlot({
      
      input$btnDone
      
      if (is.null(ctryNlData()))
        return()
      
      countries <- isolate(input$countries)
      scale <- input$scale
      
      ctryData <- ctryNlData()
      
      print ("here: renderplot")
      
      ctryData <- subset(ctryData, variable >= input$nlYearMonthRange[1] & variable <= input$nlYearMonthRange[2])
      
      if ("norm_area" %in% scale)
        ctryData$value <- (ctryData$value*10e4)/ctryData$area_sq_km

      if (input$graphtype == "boxplot")
      {
        if (length(countries)==1)
        {
          g <- ggplot(data=ctryData, aes(x=factor(variable), y=value, col=ctryData[,input$admLevel])) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + labs(col=input$admLevel)
        }
        else
        {
          g <- ggplot(data=ctryData, aes(x=factor(variable), y=value, col=country_code)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + labs(col=input$admLevel)
        }
        
        g <- g + geom_boxplot()# +facet_grid(.~variable)
      }
      else if (input$graphtype == "line")
      {
        if (length(countries)==1)
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
        
        g <- g + geom_histogram(aes(y=..density..), bins = 100, colour="black", fill="white") + geom_density(alpha=.2, fill="#FF6666") + facet_wrap(~ variable+country_code, ncol = length(countries)) # Overlay with transparent density plot

      }

      if ("scale_y_log" %in% scale)
        g <- g + scale_y_log10()
      
      if ("scale_x_log" %in% scale)
        g <- g + scale_x_log10()
      
      g
    })
    
    output$dataset <- DT::renderDataTable({
      if(is.null(ctryNlData()))
        return("NO DATA")
      
      ctryNlData()
      },
      
      options = list(scrollX = TRUE, scrolly = TRUE)
    )
    
    #output$message <- renderText({
    #  input$countries
    #})
    
    ##map output ##
    
#     observe({
#       if(!exists("nlYearMonth"))
#         return()
#       
#       nlYm <- substr(gsub("-", "", nlYearMonth[1]), 1, 6)
#       ctryYearMonth <- paste0(countries, "_", nlYm)
#       
#       leafletProxy("map") %>%
#         clearTiles("nlRaster") %>%
#         addWMSTiles(baseUrl = "http://localhost/cgi-bin/mapserv?map=test.map", layers = ctryYearMonth, options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity=0.5), layerId="nlRaster")
#     })
    
    observeEvent(input$admLevel, {
      admLevel <- input$admLevel
      countries <- input$countries
      
      if (input$drawMap == 0)
        return()
      
      lyrs <- ctryAdmLevels()
      
      lyrNum <- which(lyrs == admLevel) - 1
      
      ctryPoly <- readOGR(getPolyFnamePath(countries), ifelse(is.null(admLevel),  yes = getCtryShpLyrName(countries,0), no = getCtryShpLyrName(countries,lyrNum)))
      
      proxy <- leafletProxy("map", data=ctryPoly)
      
      print("drawing leaflet proxy")
      proxy %>% 
        clearShapes() %>% 
        addPolygons(fill = FALSE, stroke = TRUE, weight=2, smoothFactor = 0.2, opacity = 0.5, color="green")

    })
    
    output$map <- renderLeaflet({
      # Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      
      input$drawMap
      input$countries
      
      countries <- isolate(input$countries)
      nlYearMonth <- isolate(input$nlYearMonth)
      admLevel <- isolate(input$admLevel)
      
      if (is.null(countries) || is.null(nlYearMonth) || is.null(admLevel))
        return()
      
      if (length(countries) != 1)
      {
        renderText("Please select only one country/region")
        return()
      }
      
      lyrs <- ctryAdmLevels()
      
      lyrNum <- which(lyrs == admLevel) - 1
      
      ctryPoly <- readOGR(getPolyFnamePath(countries), ifelse(is.null(admLevel),  getCtryShpLyrName(countries,0), getCtryShpLyrName(countries,lyrNum)))
      
      nlYm <- substr(gsub("-", "", nlYearMonth[1]), 1, 6)
      
      ctryPoly <- spTransform(ctryPoly, wgs84)
      
      print("drawing leaflet")
      
      ctryYearMonth <- paste0(countries, "_", nlYm)
      
      message(ctryYearMonth)
      
      leaflet(data=ctryPoly) %>%
        #addTiles("http://a.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png") %>%
        addTiles %>%
        addWMSTiles(layerId="nlRaster", baseUrl = "http://localhost/cgi-bin/mapserv?map=nightlights_wms.map", layers = "nightlights_201204", options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity=1)) %>%
        #addRasterImage(ctryRast, project=FALSE, colors=colorNumeric(pal, domain=NULL, na.color="#00000000")) %>%
        #addRasterImage(x = ctryRast, colors=pal, layerId = "rasterLayer", opacity = 0.8) %>%
        addPolygons(fill = FALSE, stroke = TRUE, weight=3, smoothFactor = 0.2, opacity = 0.5, color="green") %>%
        addLayersControl(options = layersControlOptions(collapsed = FALSE))
    })
    
})
