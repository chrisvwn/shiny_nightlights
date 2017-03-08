
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

if (!require("pacman")) install.packages('pacman', repos='http://cran.r-project.org')

pacman::p_load(shiny, ggplot2, plotly, leaflet, reshape, rgdal, RColorBrewer)

library(shiny)
library(ggplot2)
library(plotly)
library(leaflet)
library(reshape)
library(rgdal)
library(RColorBrewer)

source("nightlights.R")

shinyServer(function(input, output, session){

  #yrs <- getAllNlYears("VIIRS")
  
  #isolate({updateTabItems(session, "inputs", "plotNightLights")})

    ctryAdmLevels <- reactive({
      if (length(input$countries) != 1)
        return()

      temp <- read.csv(getCtryNlDataFnamePath(input$countries), nrows = 1, header = T)
      
      cols <- names(temp)
      
      cols <- cols[-grep("area|NL_", cols)]
    })
    
    ctryAdmLevelNames <- reactive({
      if (length(input$countries) != 1)
        return()

      data <- read.csv(getCtryNlDataFnamePath(input$countries), header = T)
      
      cols <- names(data)
      
      cols <- cols[-grep("area|NL_", cols)]
      
      data[,cols]
    })
    
    ctryNlData <- reactive({
      
      input$btnCtry
      
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

#     output$intraCountry <- renderUI({
#       if(length(input$countries) != 1)
#         return()
#       
#       radioButtons(inputId = "admLevel", 
#                      label = "Admin Level", 
#                      choices = ctryAdmLevels()
#                    )
#     })

    values <- reactiveValues(
      lastUpdated = NULL
    )
    
    observe({
      lapply(names(input), function(x) {
        observe({
          input[[x]]
          if (length(grep("selectAdm", x))>0)
            values$lastUpdated <- x
        })
      })
    })
    
    observe({
      
      #input$btnIntraCtry

      admLvlCtrlsNames <- names(input)
      
      x <- admLvlCtrlsNames[grep("selectAdm", admLvlCtrlsNames)]
      
      if(length(x)==0)
        return()
      
      #if (input$btnIntraCtry > 0)
      #isolate({
        #lapply(1:length(x), function(i) eval(parse(text=paste0("isolate(\"input[[",x[i],"]]\")"))))
  
      ctryAdmLevelNames <- ctryAdmLevelNames()
      
      ctryAdmLevelNamesFilter <- ctryAdmLevelNames
      
      ctryAdmLevels <- ctryAdmLevels()
      
      lvlNum <- gsub("[^[:digit:]]", "",values$lastUpdated) #gsub("[^[:digit:]]", "", x)
      
      if(length(lvlNum)==0)
        return()
      
      print(paste0("lastupdated:",values$lastUpdated))
      
      print(paste0("x:",x))
      print(paste0("lvlnum:",lvlNum))
      
      for (lvlIdx in 2:length(ctryAdmLevels))
      {
        lvlSelect <- ""
        top10 <- ""
        
        if (lvlIdx < lvlNum)
        {
          print(paste0("lvlIdx:",lvlIdx,"lvlNum:",lvlNum))
          
          if (length(input[[paste0("selectAdm", lvlIdx-1)]]) == 1)
          {
          ctryAdmLevelNamesFilter <- subset(ctryAdmLevelNamesFilter,ctryAdmLevelNamesFilter[[ctryAdmLevels[[lvlIdx-1]]]]==input[[paste0("selectAdm", lvlIdx-1)]])
          lvlSelect <- unique(ctryAdmLevelNamesFilter[[ctryAdmLevels[lvlIdx]]])
          }
          else
          {
            lvlSelect <- unique(ctryAdmLevelNamesFilter[[ctryAdmLevels[lvlIdx]]])
          }
          print(paste0("lvlSelect:",lvlSelect))
          
          #print(paste0("lvlselect: ", lvlSelect))
          #print(paste0("top10: ", top10))
          
          #updateCheckboxInput(session, paste0("radioAdm", lvlIdx),value = TRUE)
          
          updateSelectInput(session, paste0("selectAdm",lvlIdx), choices = lvlSelect, selected = input[[paste0("selectAdm",lvlIdx)]])
        }
        else if(lvlIdx == lvlNum)
        {
          print(paste0("lvlIdx:",lvlIdx,"lvlNum:",lvlNum))
          
          if (length(input[[paste0("selectAdm", lvlIdx-1)]]) == 1)
          {
            ctryAdmLevelNamesFilter <- subset(ctryAdmLevelNamesFilter,ctryAdmLevelNamesFilter[[ctryAdmLevels[[lvlIdx-1]]]]==input[[paste0("selectAdm", lvlIdx-1)]])
            lvlSelect <- unique(ctryAdmLevelNamesFilter[[ctryAdmLevels[lvlIdx]]])
            
            #top10 <- if(length(lvlSelect) > 1) lvlSelect[1] else no = lvlSelect
            
            #print(paste0("lvlselect: ", lvlSelect))
            #print(paste0("top10: ", top10))
            
            #updateCheckboxInput(session, paste0("radioAdm", lvlIdx),value = TRUE)
            
            # if (length(input[[paste0("radioAdm", lvlIdx)]])==0)
              updateSelectizeInput(session, paste0("selectAdm",lvlIdx), choices = lvlSelect, selected = input[[paste0("selectAdm",lvlIdx)]])
          }else
          {
            updateSelectizeInput(session, paste0("selectAdm", lvlIdx), choices = NULL, selected = NULL)
          }
        }
        else
        {
          print(paste0("lvlIdx:",lvlIdx,"lvlNum:",lvlNum))
          
          if(length(input[[paste0("selectAdm",lvlIdx-1)]]) == 1)
          {
            ctryAdmLevelNamesFilter <- subset(ctryAdmLevelNamesFilter,ctryAdmLevelNamesFilter[[ctryAdmLevels[[lvlIdx-1]]]]==input[[paste0("selectAdm", lvlIdx-1)]])
            lvlSelect <- unique(ctryAdmLevelNamesFilter[[ctryAdmLevels[lvlIdx]]])
            #updateCheckboxInput(session, paste0("radioAdm", lvlIdx),value = FALSE)

            updateSelectizeInput(session, paste0("selectAdm", lvlIdx), choices = lvlSelect, selected = NULL)
          }
          else if(length(input[[paste0("selectAdm",lvlIdx-1)]]) == 0 && length(input[[paste0("selectAdm", lvlNum)]])==1)
          {
            lvlSelect <- unique(ctryAdmLevelNamesFilter[[ctryAdmLevels[lvlIdx]]])
            updateSelectizeInput(session, paste0("selectAdm", lvlIdx), choices = lvlSelect, selected = NULL)
          }
          else
          {
            updateSelectizeInput(session, paste0("selectAdm", lvlIdx), choices = "", selected = NULL)
          }
        }
      }
      #})
    })
  #})

    output$intraCountry <- renderUI({

      #input$btnIntraCtry
      print("")
      countries <- input$countries
      
      if(length(countries) != 1)
        return()      
      
      ctryAdmLevels <- ctryAdmLevels()

      ctryAdmLevelNames <- ctryAdmLevelNames()

      elems <- lapply(2:length(ctryAdmLevels), function(lvlIdx) {
        
        lvl <- ctryAdmLevels[lvlIdx]
        
        lvlSelect <- unique(ctryAdmLevelNames[[ctryAdmLevels[lvlIdx]]])

#         a <- checkboxInput(inputId = paste0("radioAdm", lvlIdx),
#                      label = ctryAdmLevels[lvlIdx], 
#                      value = FALSE
#         )
        
        b <- selectizeInput(inputId = paste0("selectAdm", lvlIdx),
                       label = ctryAdmLevels[lvlIdx],
                       choices = lvlSelect,
                       selected = NULL,
                       multiple = TRUE
                       )
        
        #list(a,b)
      })
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
    
    
    output$plotNightLights <- renderPlotly({
      
      input$btnCtry
      
      countries <- isolate(input$countries)
      
      admLvlCtrlNames <- names(input)
      
      x <- admLvlCtrlNames[grep("selectAdm", admLvlCtrlNames)]
      
      isolate({
      admLvlNum<-""
      for (i in x)
        if(length(input[[i]])>0)
          admLvlNum <- i
      
      print(paste0("x", x))
      print(paste0("admlvlnum:", admLvlNum))
      
      if (admLvlNum=="")
        return()
      
      admLvlNum <- gsub("[^[:digit:]]","",admLvlNum)
      
      ctryAdmLevels <- ctryAdmLevels()
      admLevel <- ctryAdmLevels[as.numeric(admLvlNum)]
      
      print(paste0("admLevel:", admLevel))
      
      scale <- input$scale
      nlYearMonthRange <- input$nlYearMonthRange
      graphType <- input$graphType
      
      ctryData <- ctryNlData()

      if (is.null(countries) || is.null(ctryData))
        return()
            
      print ("here: renderplot")
      
      ctryData <- subset(ctryData, variable >= nlYearMonthRange[1] & variable <= nlYearMonthRange[2])
      
      for (lvl in 2:length(ctryAdmLevels))
      {
        print(paste0("lvl:",lvl))
        
        if (length(input[[x[lvl-1]]])>0)
        {
          ctryData <- subset(ctryData, ctryData[[ctryAdmLevels[lvl]]] %in% input[[x[lvl-1]]])
        }
      }
      
      if ("norm_area" %in% scale)
        ctryData$value <- (ctryData$value*10e4)/ctryData$area_sq_km

      if (graphType == "boxplot")
      {
        if (length(countries)==1)
        {
          g <- ggplot(data=ctryData, aes(x=factor(variable), y=value, col=ctryData[,admLevel])) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + labs(col=admLevel)
        }
        else
        {
          g <- ggplot(data=ctryData, aes(x=factor(variable), y=value, col=country_code)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + labs(col=admLevel)
        }
        
        g <- g + geom_boxplot()# +facet_grid(.~variable)
      }
      else if (graphType == "line")
      {
        if (length(countries)==1)
        {
          ctryData <- setNames(aggregate(ctryData$value, by=list(ctryData[,admLevel], ctryData[,"variable"]), mean, na.rm=T), c(admLevel, "variable", "value"))
          
          g <- ggplot(data=ctryData, aes(x=variable, y=value, col=ctryData[,admLevel])) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + labs(col=admLevel)
        }
        else
        {
          ctryData <- aggregate(value ~ country_code+variable, data=ctryData, mean)
          g <- ggplot(data=ctryData, aes(x=variable, y=value, col=country_code))
        }

        g <- g+ geom_line() + geom_point()
      }
      else if (graphType == "histogram")
      {
        #ctryData <- aggregate(value ~ country_code+variable, data=ctryData, mean)
        
        g <- ggplot(data=ctryData, aes(x=value))
        
        g <- g + geom_histogram(aes(y=..density..), bins = 30, colour="black", fill="white") + geom_density(alpha=.2, fill="#FF6666") + facet_wrap(~ variable+country_code, ncol = length(countries)) # Overlay with transparent density plot

      }

      if ("scale_y_log" %in% scale)
        g <- g + scale_y_log10()
      
      if ("scale_x_log" %in% scale)
        g <- g + scale_x_log10()
      
      ggplotly(g)
      
      })
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
        addPolygons(fill = FALSE, stroke = TRUE, weight=3, smoothFactor = 0.7, opacity = 0.5, color="green")

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
        addPolygons(fill = FALSE, stroke = TRUE, weight=3, smoothFactor = 0.7, opacity = 0.5, color="green") %>%
        addLayersControl(options = layersControlOptions(collapsed = FALSE))
    })
    
})
