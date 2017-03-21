
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

if (!require("pacman")) install.packages('pacman', repos='http://cran.r-project.org')

pacman::p_load(shiny, ggplot2, plotly, reshape, rgdal, RColorBrewer, ggdendro, dendextend)

pacman::p_load_gh("rstudio/leaflet")

library(shiny)
library(ggplot2)
library(plotly)
library(leaflet)
library(reshape)
library(rgdal)
library(RColorBrewer)
library(ggdendro)
library(dendextend)

source("nightlights.R")
options(shiny.trace=F)

shinyServer(function(input, output, session){
  #Since renderUI does not like intraCountry returning NULL we init with an empty renderUI, set suspendWhenHidden = FALSE to force it to recheck intraCountry even if null
  
  output$intraCountry <- renderUI({})
  outputOptions(output, "intraCountry", suspendWhenHidden = FALSE)
  
  #yrs <- getAllNlYears("VIIRS")
  
  #isolate({updateTabItems(session, "inputs", "plotNightLights")})

  #### reactive ctryAdmLevels ####
  
    ctryAdmLevels <- reactive({
      print(paste0("here: ctryAdmLevels"))
      
      if (length(input$countries) != 1)
        return()

      temp <- fread(getCtryNlDataFnamePath(input$countries), nrows = 1, header = T)
      
      cols <- names(temp)
      
      cols <- cols[-grep("area_sq_km|NL_", cols)]
    })
    
    #### reactive ctryAdmLevelNames ####
    
    ctryAdmLevelNames <- reactive({
      print(paste0("here: ctryAdmLevelNames"))
      
      countries <- input$countries
      
      if (length(countries) != 1)
        return()

      hdr <- fread(getCtryNlDataFnamePath(countries), nrows = 1, header = T)
      
      colClasses <- names(hdr)
      
      colClasses[-grep("area_sq_km|NL_", colClasses)] <- "character"
      colClasses[grep("area_sq_km|NL_", colClasses)] <- "NULL"
      
      data <- fread(getCtryNlDataFnamePath(countries), colClasses = colClasses, header = T)
    })
    
    #### reactive ctryNlData ####
  
    ctryNlData <- reactive({
      print(paste0("here: ctryNlData"))
      input$btnGo
      
      countries <- isolate(input$countries)
      
      if (length(countries)<1)
        return()
      
      ctryData <- NULL
      
      if (length(countries) == 1)
      {
        ctryData <- fread(getCtryNlDataFnamePath(countries))
      }
      else if(length(countries) > 1)
      {
        for (ctryCode in countries)
        {
          print(ctryCode)
          temp <- fread(getCtryNlDataFnamePath(ctryCode))
          
          ctryCols <- grep("country_code|area|NL_", names(temp))
          
          temp <- temp[, ctryCols, with=F]
          
          if (is.null(ctryData))
          {
            ctryData <- temp
          }else
          {
            ctryData <- merge(ctryData, temp, all=TRUE)
          }
        }
      }
      return(ctryData)
    })  
  
    #### reactive ctryNlDataMelted ####
    
    ctryNlDataMelted <- reactive({
      print(paste0("here: ctryNlDataMelted"))
      
      if(is.null(ctryNlData()))
        return()
      
      ctryData <- ctryNlData()
      
      meltMeasureVars <- names(ctryData)[grep("NL_", names(ctryData))]
      
      meltVarNames <- gsub("[^[:digit:]]", "", meltMeasureVars)
      
      ctryData <- melt(ctryData, measure.vars=meltMeasureVars)
      
      # ctryData$variable <- sapply(ctryData$variable, function(x) {paste0(gsub("[^[:digit:]]","", x),"01")})
      ctryData$variable <- paste0(gsub("[^[:digit:]]","", ctryData$variable),"01")
      
      ctryData$variable <- as.Date(ctryData$variable, format="%Y%m%d")
      
      return(ctryData)
    })

    #### reactiveValues values ####
    
    values <- reactiveValues(
      a=print(paste0("here: lastUpdated")),
      lastUpdated = NULL
    )
    
    #### observe lastUpdated ####
    
    observe({
      lapply(names(input), function(x) {
        observe({
          input[[x]]
          values$lastUpdated <- x
        })
      })
    })
    
    output$intraCountry1 <- renderUI({
      if(length(input$countries) != 1)
        return()
      
      radioButtons(inputId = "admLevel", 
                     label = "Admin Level", 
                     choices = ctryAdmLevels()
                   )
    })
    
    #### render UI: intraCountry ####
    
    output$intraCountry <- renderUI({
      
      print("here: renderUI intracountry")
      countries <- input$countries
      
      if(length(countries) != 1)
        return()
      
      ctryAdmLevels <- ctryAdmLevels()
      
      ctryAdmLevelNames <- ctryAdmLevelNames()
      
      if (length(ctryAdmLevelNames)>1)
        elems <- lapply(2:length(ctryAdmLevels), function(lvlIdx){
          
          lvl <- ctryAdmLevels[lvlIdx]
          
          lvlSelect <- unique(ctryAdmLevelNames[[ctryAdmLevels[lvlIdx]]])
          
          #         a <- checkboxInput(inputId = paste0("radioAdm", lvlIdx),
          #                      label = ctryAdmLevels[lvlIdx], 
          #                      value = FALSE
          #         )
          
          b <- selectizeInput(inputId = paste0("selectAdm", lvlIdx),
                              label = ctryAdmLevels[lvlIdx],
                              choices = NULL,
                              selected = NULL,
                              multiple = TRUE
          )
          
          updateSelectizeInput(session = session,
                               inputId = paste0("selectAdm", lvlIdx),
                               choices = lvlSelect,
                               server = TRUE
                               )
          
          b
          #list(a,b)
        })
    })
    
    #### observe selectAdms (intraCountry) ####
    
    observe({
      print(paste0("here: observe selectAdms"))

      admLvlCtrlsNames <- names(input)
      
      x <- admLvlCtrlsNames[grep("selectAdm", admLvlCtrlsNames)]
      
      if(length(x)==0)
        return()

      admSelected <- FALSE
      lowestSelected <- ""
      for (i in x)
      {
        if (length(input[[i]]) > 0)
        {
          admSelected <- TRUE
          lowestSelected <- gsub("[^[:digit:]]","",i)
        }
      }
      
      if (!admSelected)
        return()
      
      ctryAdmLevelNames <- ctryAdmLevelNames()
      
      ctryAdmLevelNamesFilter <- ctryAdmLevelNames
      
      ctryAdmLevels <- ctryAdmLevels()
      
      lvlNum <- gsub("[^[:digit:]]", "",values$lastUpdated) #gsub("[^[:digit:]]", "", x)
      
      if(lvlNum=="")
        return()
      
      print(paste0("lastupdated:",values$lastUpdated))
      
      print(paste0("x:",x))
      print(paste0("lvlnum:",lvlNum))
      
      #set admLevel to match the selectizeInput level
      #if (length(input[[paste0("selectAdm", lvlNum)]]) > 0)
        updateRadioButtons(session = session, inputId = "admLevel", selected = ctryAdmLevels[as.numeric(lowestSelected)])
      
      multipleSelected <- FALSE
      
      for (lvlIdx in 2:length(ctryAdmLevels))
      {
        lvlSelect <- ""
        top10 <- ""
        
        if (length(input[[paste0("selectAdm", lvlIdx)]]) > 1)
          multipleSelected <- TRUE
        
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
          #print(paste0("lvlSelect:",lvlSelect))
          
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
          
          if (multipleSelected)
          {
            updateSelectizeInput(session, paste0("selectAdm", lvlIdx), choices = "", selected = NULL)
            next()
          }
          
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
            lvlSelect <- unique(ctryAdmLevelNamesFilter[[ctryAdmLevels[lvlIdx]]])
            
            updateSelectizeInput(session, paste0("selectAdm", lvlIdx), choices = lvlSelect, selected = NULL)
          }
        }
      }
      #})
    })
  #})


    #### sliderNlYearMonthRange ####
    
    output$sliderNlYearMonthRange <- renderUI({
      print(paste0("here: sliderNlYearMonthRange"))
      ctryData <- ctryNlDataMelted()
      
      if (is.null(ctryData))
      {
        sliderInput(inputId = "nlYearMonthRange",
                    label = "Time",
                    min = as.Date("2012-04-01", "%Y-%m-%d"),
                    max = as.Date("2016-12-31", "%Y-%m-%d"),
                    timeFormat = "%Y-%m",
                    step = 31,
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
                    step = 31,
                    value = c(minDate, maxDate),
                    animate = animationOptions(interval = 2000, loop = FALSE, playButton = NULL, pauseButton = NULL)
        )
      }
      
    })
    
    #### sliderNlYearMonth ####
    
    output$sliderNlYearMonth <- renderUI({
      print(paste0("here: sliderNlYearMonth"))
      
      ctryData <- ctryNlDataMelted()
      
      if (is.null(ctryData))
      {
        sliderInput(inputId = "nlYearMonth",
                    label = "Time",
                    min = as.Date("2012-04-01", "%Y-%m-%d"),
                    max = as.Date("2016-12-31", "%Y-%m-%d"),
                    timeFormat = "%Y-%m",
                    step = 31,
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
                    step = 31,
                    value = minDate,
                    animate = animationOptions(interval = 10000, loop = FALSE, playButton = "Play", pauseButton = NULL)
                    
        )
      }
    })
    ####reactive hcluster####
    hCluster <- reactive({
      print(paste0("here: reactive hCluster"))
      input$btnGo
      
      countries <- isolate(input$countries)
      
      if (is.null(countries))
        return()
      
      scale <- input$scale
      
      isolate({
      nlYearMonthRange <- input$nlYearMonthRange
      graphType <- input$graphType
      admLevel <- ctryAdmLevels()[2]

      #return if the country doesn't have adm levels below country
      if (is.na(admLevel))
        return()
      
      meltCtryData <- ctryNlDataMelted()
      
      if (is.null(countries) || is.null(meltCtryData))
        return()
      
      if ("norm_area" %in% scale)
        meltCtryData$value <- (meltCtryData$value)/meltCtryData$area_sq_km
      
      #aggMeltCtryData <- aggregate(mean(value), by=list(eval(admLevel)+variable), data=meltCtryData, mean)
      aggMeltCtryData <- setNames(meltCtryData[,list(mean(value, na.rm = TRUE)), by = list(meltCtryData[[admLevel]], variable)], c(admLevel, "variable", "value"))
      
      dcastFormula <- paste(paste(admLevel, collapse = " + "), "~", paste("variable", collapse = " + "))
      
      unmeltCtryData <- dcast(aggMeltCtryData, dcastFormula, value.var='value', aggregate='mean')
      
      d <- dist(unmeltCtryData)
      
      h<-hclust(d)
      
      h$labels <- unmeltCtryData[[admLevel]]
      
      h
      })
    })
    
    output$plotHCluster <- renderPlot({
      print(paste0("here: plotHCluster"))
      
      clusts <- hCluster()
      numClusters <- input$kClusters
      
      if (is.null(clusts))
        return("Country has no adm levels")
      
      isolate({
      dendro <- as.dendrogram(clusts)
      
      cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

      dendro %>% color_branches(k=numClusters, col = cbPalette) %>% 
        color_labels(k=numClusters, col = cbPalette) %>%
        plot(horiz=FALSE, main = "")
      
      dendro %>% rect.dendrogram(k=numClusters,horiz=FALSE,border = cbPalette)
      
      })
      
    })
    
    
    ####renderPlotly plotCluster####
    
    output$plotPointsCluster <- renderPlotly({
      print(paste0("here: plotPointsCluster"))
      
      input$btnGo
      
      countries <- isolate(input$countries)
      
      if(length(countries) < 1)
        return()
      
      clusts <- hCluster()

      if(is.null(clusts))
        return()
            
      admLevel <- ctryAdmLevels()[2]

      
      #return if the country doesn't have adm levels below country
      if (admLevel == "")
        return()
      
      numClusters <- input$kClusters
      scale <- input$scale
      
      isolate({
        meltCtryData <- ctryNlDataMelted()
        
        if ("norm_area" %in% scale)
          meltCtryData$value <- (meltCtryData$value)/meltCtryData$area_sq_km
        
        cutClusts <- cutree(clusts, k=numClusters)
        
        #ctryAvg <- aggregate(value ~ admLevel, data=meltCtryData, mean)
        ctryAvg <- setNames(meltCtryData[,mean(value, na.rm = TRUE), by = list(meltCtryData[[admLevel]])], c(admLevel, "value"))
  
        cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        
        g <- ggplot(data=ctryAvg, aes(x=ctryAvg[[admLevel]], y=value, col=as.factor(cutClusts)))+geom_point(size=2)+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+scale_colour_manual(values=cbPalette)
        
        ggplotly(g)
      })
    })
    
    output$mapHCluster <- renderLeaflet({
      print(paste0("here: draw mapHCluster"))
      # Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      
      input$btnGo
      
      
      countries <- isolate(input$countries)
      scale <- input$scale
      numClusters <- input$kClusters
      
      isolate({      
        if (is.null(countries))
          return()
        
        if (length(countries) != 1)
        {
          renderText("Please select only one country/region")
          return()
        }

        print("drawing leaflet cluster")
        
        clusts <- hCluster()
        
        cutClusts <- cutree(clusts, k=numClusters)
        
        admLevel <- ctryAdmLevels()[2]
        
        meltCtryData <- ctryNlDataMelted()
        
        if ("norm_area" %in% scale)
          meltCtryData$value <- (meltCtryData$value)/meltCtryData$area_sq_km
        
        ctryPoly0 <- readOGR(getPolyFnamePath(countries), getCtryShpLyrName(countries,0))
        
        map <- leaflet(data=ctryPoly0) %>%
          #addTiles("http://a.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png") %>%
          addTiles %>%
          addWMSTiles(layerId="nlRaster", baseUrl = "http://localhost/cgi-bin/mapserv?map=nightlights_wms.map", layers = "nightlights_201204", options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity=1)) %>%
          addPolygons(layerId = countries, fill = FALSE, fillColor = "#fefe40", stroke = TRUE, weight=4, smoothFactor = 0.7, opacity = 1, color="white", dashArray = "5", group = "country_code")
        
        
        lvlCtryData <- setNames(meltCtryData[,mean(value, na.rm = TRUE), by = list(meltCtryData[[admLevel]])], c(admLevel, "value"))
        
        lvlCtryData[["rank"]] <- with(lvlCtryData, rank(-value, ties.method = 'first'))
        
        cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        
        pal <- cbPalette
        
        #turn off previous layer? No point keeping it if it is hidden. Also we want to turn the current layer to transparent so that one can see through to the raster layer on hover
        ctryPoly <- readOGR(getPolyFnamePath(countries), getCtryShpLyrName(countries, 1)) 
        
        ctryPoly <- spTransform(ctryPoly, wgs84)
        
        mapLabels <- sprintf(
          paste0("%s:%s", "<br/>Cluster: %s", "<br/>Rad:%s", "<br/>Rank: %s/%s"),
          admLevel, lvlCtryData[[1]], cutClusts, format(lvlCtryData[[2]],scientific = T,digits = 2), lvlCtryData[["rank"]], nrow(lvlCtryData)
        ) %>% lapply(htmltools::HTML)
        
        map <- map %>% addPolygons(
          data = ctryPoly,
          layerId = as.character(ctryPoly@data[,'NAME_1']),
          fill = TRUE,
          fillColor = pal[cutClusts],
          fillOpacity = 0.9,
          stroke = TRUE, 
          weight=1,
          smoothFactor = 0.7,
          opacity = 1,
          color="white",
          dashArray = "5",
          group = admLevel,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0,
            bringToFront = TRUE),
          label = mapLabels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal",
                         padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        )
        
        map <- map %>% addLayersControl(overlayGroups = admLevel)
        
        map <- map %>% addLegend(position = "bottomright", 
                                 colors = pal[unique(cutClusts)], 
                                 labels = unique(cutClusts),
                                 #title = "Nightlight percentiles",
                                 title = "clusters",
                                 opacity = 1 )
        
        map
      })
    })
    
    ####renderPlotly plotTSDecomposed####
    
    output$plotTSDecomposed <- renderPlot({
      print(paste0("here: plotTSDecomposed"))
      input$btnGo
      
      countries <- isolate(input$countries)
      
      if(length(countries) < 1)
        return()
      
      admLevel <- ctryAdmLevels()[1]
      
      
      #return if the country doesn't have adm levels below country
      if (admLevel == "")
        return()
      
      scale <- input$scale
      
      isolate({
        meltCtryData <- ctryNlDataMelted()
        
        if ("norm_area" %in% scale)
          meltCtryData$value <- (meltCtryData$value)/meltCtryData$area_sq_km

        #ctryAvg <- aggregate(value ~ admLevel, data=meltCtryData, mean)
        ctryAvg <- setNames(meltCtryData[,mean(value, na.rm = TRUE), by = list(meltCtryData[[admLevel]], variable)], c(admLevel, "variable", "value"))
        
        startYear <- year(min(ctryAvg$variable))
        startMonth <- month(min(ctryAvg$variable))
        endYear <- year(max(ctryAvg$variable))
        endMonth <- month(max(ctryAvg$variable))
        
        ctryDataTS <- ts(ctryAvg$value, start = c(startYear,startMonth), end = c(endYear,endMonth), frequency = 12)

        ctryDataTScomponents <- decompose(ctryDataTS)
        g <- autoplot(ctryDataTScomponents)
        
        plot(ctryDataTScomponents)
      })
    })
    
    ####renderPlotly plotYearly####
    
    output$plotYearly <- renderPlotly({
      print(paste0("here: renderPlotYearly"))
      input$btnGo
      
      countries <- isolate(input$countries)
      
      if (is.null(countries))
        return()
      
      scale <- input$scale
      nlYearMonthRange <- input$nlYearMonthRange
      graphType <- input$graphType

      isolate({      
        ctryData <- ctryNlDataMelted()
        
        if (is.null(countries) || is.null(ctryData))
          return()
        
        admLvlCtrlNames <- names(input)
        
        x <- admLvlCtrlNames[grep("selectAdm", admLvlCtrlNames)]

        admLvlNums <- NULL
        for (i in x)
          if(length(input[[i]])>0)
            admLvlNums <- c(admLvlNums, i)
          
          
          print(paste0("x", x))
          print(paste0("admlvlnums:", admLvlNums))
          
          #if (admLvlNum=="" && length(countries)>0)
          #  return()
          
          admLvlNums <- as.numeric(gsub("[^[:digit:]]","",admLvlNums))
          
          if (length(admLvlNums)==0)
            admLvlNums <- 1
          
          ctryAdmLevels <- ctryAdmLevels()
          admLevel <- ctryAdmLevels[as.numeric(last(admLvlNums))]
          
          print(paste0("admLevel:", admLevel))
          
          if (!exists("admLevel") || is.null(admLevel) || length(admLevel)==0)
            admLevel <- "country_code"
          
          ctryData <- subset(ctryData, variable >= nlYearMonthRange[1] & variable <= nlYearMonthRange[2])
          
          for (lvl in admLvlNums)
          {
            if (lvl == 1)
              next()
            
            print(paste0("lvl:",lvl))
            
            if (length(input[[x[lvl-1]]])>0)
            {
              ctryData <- subset(ctryData, ctryData[[ctryAdmLevels[lvl]]] %in% input[[x[lvl-1]]])
            }
          }
          
          ctryData$year <- year(ctryData$variable)
          
          ctryData$month <- month(ctryData$variable)
          
          print(paste0("ctrydata nrow:", nrow(ctryData)))
          
          if ("norm_area" %in% scale)
            ctryData$value <- (ctryData$value)/ctryData$area_sq_km
          
          if (graphType == "boxplot")
          {
            if (length(countries)==1)
            {
              g <- ggplot(data=ctryData, aes(x=factor(variable), y=value, col=ctryData[[admLevel]])) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + labs(col=admLevel)
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
              #switched to data.table aggregation
              #ctryData <- setNames(aggregate(ctryData$value, by=list(ctryData[,admLevel], ctryData[,"variable"]), mean, na.rm=T), c(admLevel, "variable", "value"))
              ctryData <- setNames(ctryData[,list(mean(value, na.rm = TRUE)), by = list(ctryData[[admLevel]], variable, as.factor(year), as.factor(month))], c(admLevel, "variable", "year", "month", "value"))
              g <- ggplot(ctryData, aes(x=month, y=value, col=year, group=year))
            }
            else
            {
              #ctryData <- aggregate(value ~ country_code+variable, data=ctryData, mean)
              #switched to data.table aggregation
              ctryData <- setNames(ctryData[,mean(value, na.rm = TRUE),by = list(country_code, variable)], c("country_code", "variable", "value"))
              g <- ggplot(data=ctryData, aes(x=variable, y=value, col=country_code))
            }
            
            g <- g + geom_line() + geom_point() + geom_smooth(aes(group=1),method = "loess", weight=3) #+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) #+ labs(col=year)
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
          
          if ("norm_area" %in% scale)
            g <- g + labs(title="Nightlight Radiances", x = "Month", y = expression(paste("Avg Rad W" %.% "Sr" ^{-1} %.% "cm" ^{-2}, "per Km" ^{2})))
          else
            g <- g + labs(title="Nightlight Radiances", x = "Month", y = expression(~Total~Rad~W %.% Sr^{-1}%.%cm^{-2}))
          
          ggplotly(g)
          
      })
    })
    
    #### renderPlotly plotNightLights####
    
    output$plotNightLights <- renderPlotly({
      print(paste0("here: renderPlot"))
      input$btnGo
      
      countries <- isolate(input$countries)
      
      if (is.null(countries))
        return()
      
      scale <- input$scale
      nlYearMonthRange <- input$nlYearMonthRange
      graphType <- input$graphType
      
      ctryData <- ctryNlDataMelted()

      if (is.null(countries) || is.null(ctryData))
        return()
            
      admLvlCtrlNames <- names(input)
      
      x <- admLvlCtrlNames[grep("selectAdm", admLvlCtrlNames)]
      
      isolate({
        admLvlNums <- NULL
        for (i in x)
          if(length(input[[i]])>0)
            admLvlNums <- c(admLvlNums, i)
          
          
        print(paste0("x", x))
        print(paste0("admlvlnums:", admLvlNums))
        
        #if (admLvlNum=="" && length(countries)>0)
        #  return()
        
        admLvlNums <- as.numeric(gsub("[^[:digit:]]","",admLvlNums))
        
        if (length(admLvlNums)==0)
          admLvlNums <- 1
        
        ctryAdmLevels <- ctryAdmLevels()
        admLevel <- ctryAdmLevels[as.numeric(last(admLvlNums))]
        
        print(paste0("admLevel:", admLevel))
        
        if (!exists("admLevel") || is.null(admLevel) || length(admLevel)==0)
          admLevel <- "country_code"
          
      ctryData <- subset(ctryData, variable >= nlYearMonthRange[1] & variable <= nlYearMonthRange[2])
      
      for (lvl in admLvlNums)
      {
        if (lvl == 1)
          next()
        
        print(paste0("lvl:",lvl))
        
        if (length(input[[x[lvl-1]]])>0)
        {
          ctryData <- subset(ctryData, ctryData[[ctryAdmLevels[lvl]]] %in% input[[x[lvl-1]]])
        }
      }
      
      print(paste0("ctrydata nrow:", nrow(ctryData)))
      
      if ("norm_area" %in% scale)
        ctryData$value <- (ctryData$value)/ctryData$area_sq_km

      if (graphType == "boxplot")
      {
        if (length(countries)==1)
        {
          g <- ggplot(data=ctryData, aes(x=factor(variable), y=value, col=ctryData[[admLevel]])) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + labs(col=admLevel)
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
          #switched to data.table aggregation
          #ctryData <- setNames(aggregate(ctryData$value, by=list(ctryData[,admLevel], ctryData[,"variable"]), mean, na.rm=T), c(admLevel, "variable", "value"))
          ctryData <- setNames(ctryData[,mean(value, na.rm = TRUE),by = list(ctryData[[admLevel]], variable)], c(admLevel, "variable", "value"))
          g <- ggplot(data=ctryData, aes(x=variable, y=value, col=ctryData[[admLevel]]))
        }
        else
        {
          #ctryData <- aggregate(value ~ country_code+variable, data=ctryData, mean)
          #switched to data.table aggregation
          ctryData <- setNames(ctryData[,mean(value, na.rm = TRUE),by = list(country_code, variable)], c("country_code", "variable", "value"))
          g <- ggplot(data=ctryData, aes(x=variable, y=value, col=country_code))
        }

        g <- g+ geom_line() + geom_point()+ theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + labs(col=admLevel)
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
      
      if ("norm_area" %in% scale)
        g <- g + labs(title="Nightlight Radiances", x = "Month", y = expression(paste("Avg Rad W" %.% "Sr" ^{-1} %.% "cm" ^{-2}, "per Km" ^{2})))
      else
        g <- g + labs(title="Nightlight Radiances", x = "Month", y = expression(~Total~Rad~W %.% Sr^{-1}%.%cm^{-2}))
      
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
    
#     observeEvent(input$admLevel, {
#       print(paste0("here: observe admLevel 2 update map"))
#       admLevel <- input$admLevel
#       countries <- input$countries
#       
#       if (input$drawMap == 0)
#         return()
#       
#       lyrs <- ctryAdmLevels()
#       
#       lyrNum <- which(lyrs == admLevel) - 1
#       
#       ctryPoly <- readOGR(getPolyFnamePath(countries), ifelse(is.null(admLevel),  yes = getCtryShpLyrName(countries,0), no = getCtryShpLyrName(countries,lyrNum)))
#       
#       proxy <- leafletProxy("map", data=ctryPoly)
#       
#       print("drawing leaflet proxy")
#       proxy %>% 
#         clearShapes() %>% 
#         addPolygons(fill = FALSE, stroke = TRUE, weight=3, smoothFactor = 0.7, opacity = 0.5, color="green")
#     })
    

    output$map <- renderLeaflet({
      print(paste0("here: draw leaflet map"))
      # Use leaflet() here, and only include aspects of the map that
      # won't need to change dynamically (at least, not unless the
      # entire map is being torn down and recreated).
      
      input$btnGo


      countries <- isolate(input$countries)
      nlYearMonth <- input$nlYearMonth
      admLevel <- isolate(input$admLevel)
      scale <- input$scale
      
      isolate({      
      if (is.null(countries) || is.null(nlYearMonth) || is.null(admLevel))
        return()
      
      if (length(countries) != 1)
      {
        renderText("Please select only one country/region")
        return()
      }
      
      admLvlCtrlNames <- names(input)
      
      x <- admLvlCtrlNames[grep("selectAdm", admLvlCtrlNames)]
      
      admLvlNums <- NULL
      for (i in x)
        if(length(input[[i]])>0)
          admLvlNums <- c(admLvlNums, i)
        
        
      print(paste0("x", x))
      print(paste0("admlvlnums:", admLvlNums))

      admLvlNums <- as.numeric(gsub("[^[:digit:]]","",admLvlNums))
      
      print(paste0("admlvlNums:", admLvlNums))

      #get the selected admLevel and convert to lyrnum
      ctryAdmLevels <- ctryAdmLevels()

      lyrNum <- which(ctryAdmLevels == admLevel)
      
      #line weight increases. max=4 min=1
      deltaLineWt <- (4 - 1) / as.numeric(lyrNum)

      nlYm <- as.Date(nlYearMonth[1], "%Y%m%d")

      ctryData <- ctryNlDataMelted()
      
      if (is.null(ctryData))
        return()
      
      #get our data ready to match with polygons
      #subset data based on level selections
      ctryData <- subset(ctryData, year(variable) == year(nlYm) & month(variable) == month(nlYm))

      #only used when we want to show only the selected features
      #for now we want all features shown and then highlight the selected features
#       for (lvl in admLvlNums)
#       {
#         if (lvl == 1)
#           next()
#         
#         print(paste0("lvl:",lvl))
#         
#         if (length(input[[x[lvl-1]]])>0)
#         {
#           ctryData <- subset(ctryData, ctryData[[ctryAdmLevels[lvl]]] %in% input[[x[lvl-1]]])
#         }
#       }

      if ("norm_area" %in% scale)
        ctryData$value <- (ctryData$value)/ctryData$area_sq_km

      print(paste0("ctrydata nrow:", nrow(ctryData)))
      
      print("drawing leaflet")
      
      ctryYearMonth <- paste0(countries, "_", nlYm)
      
      message(ctryYearMonth)
      
      ctryPoly0 <- readOGR(getPolyFnamePath(countries), getCtryShpLyrName(countries,0))
      
      map <- leaflet(data=ctryPoly0) %>%
        #addTiles("http://a.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png") %>%
        addTiles %>%
        addWMSTiles(layerId="nlRaster", baseUrl = "http://localhost/cgi-bin/mapserv?map=nightlights_wms.map", layers = "nightlights_201204", options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity=1)) %>%
        addPolygons(layerId = countries, fill = FALSE, fillColor = "#fefe40", stroke = TRUE, weight=4, smoothFactor = 0.7, opacity = 1, color="white", dashArray = "5", group = "country_code")

        selected <- NULL
        if (lyrNum > 1) #skip drawing the country level. avoid reverse seq
        for (iterAdmLevel in 2:lyrNum)
        {
          #aggregate the data to the current level
          iterAdmLevelName <- ctryAdmLevels[iterAdmLevel]
          #lvlCtryData <- setNames(aggregate(ctryData$value, by=list(ctryData[[iterAdmLevelName]], ctryData[,"variable"]), mean, na.rm=T), c(iterAdmLevelName, "variable", "value"))
          
          #temp <- as.data.table(ctryData)
          #data already in data.table form
          lvlCtryData <- setNames(ctryData[,list(mean(value,na.rm=T), sum(area_sq_km, na.rm=T)), by=list(ctryData[[iterAdmLevelName]], ctryData[["variable"]])], c(iterAdmLevelName, "variable", "value", "area_sq_km"))
          #lvlCtryData <- as.data.frame(lvlCtryData)
          
          #rank the data
          varname <- paste0('rank',iterAdmLevel)
          lvlCtryData[[varname]] <- with(lvlCtryData, rank(-value, ties.method = 'first'))
          
          #palette deciles for the layer
          bins <- unique(quantile(lvlCtryData$value, seq(0,1,0.1), na.rm=T))
          brewerPal <- rev(brewer.pal(10, "YlOrRd"))
          pal <- colorBin(brewerPal, domain = lvlCtryData$value, na.color = "grey", bins=bins)
          
          #turn off previous layer? No point keeping it if it is hidden. Also we want to turn the current layer to transparent so that one can see through to the raster layer on hover
          ctryPoly <- readOGR(getPolyFnamePath(countries), getCtryShpLyrName(countries, iterAdmLevel-1)) 
          
          ctryPoly <- spTransform(ctryPoly, wgs84)
          
          if (length(admLvlNums) > 0)
          if((iterAdmLevel) == last(admLvlNums)) #iterAdmLevel+1 %in% admLvlNums)
            selected <- which(ctryPoly@data[[paste0("NAME_",iterAdmLevel-1)]] %in% input[[paste0("selectAdm", iterAdmLevel)]])
          else
            selected <- c()
          

          mapLabels <- sprintf(
            paste0("<strong>%s:%s</strong>", "<br/>Area: %s km<superscript>2</superscript>","<br/>Date: %s", ifelse("norm_area" %in% scale, "<br/>Rad: %s /sq.km", "<br/>Rad: %s"), "<br/>Rank: %s/%s"),
            ctryAdmLevels[iterAdmLevel], lvlCtryData[[ctryAdmLevels[iterAdmLevel]]], format(lvlCtryData[["area_sq_km"]],scientific = T,digits = 2), lvlCtryData[["variable"]], format(lvlCtryData[["value"]],scientific = T,digits = 2),  lvlCtryData[[paste0("rank",iterAdmLevel)]], nrow(lvlCtryData)
          ) %>% lapply(htmltools::HTML)

          map <- map %>% addPolygons(
            data = ctryPoly,
            layerId = as.character(ctryPoly@data[,paste0('NAME_',iterAdmLevel-1)]),
            fill = TRUE,
            fillColor = ~pal(lvlCtryData[["value"]]),
            fillOpacity = 0.9,
            stroke = TRUE, weight=4-(iterAdmLevel-1)*deltaLineWt,
            smoothFactor = 0.7,
            opacity = 1,
            color="white",
            dashArray = "5",
            group = ctryAdmLevels[iterAdmLevel],
            highlight = highlightOptions(
              weight = 5,
              color = "#666",
              dashArray = "",
              fillOpacity = 0,
              bringToFront = TRUE),
              label = mapLabels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal",
                             padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
                )
            )
            
        
          for (iterPoly in selected)
          {
              map <- map %>% addPolygons(
                data = ctryPoly[iterPoly,],
                layerId = paste0(as.character(ctryPoly@data[iterPoly,paste0('NAME_',iterAdmLevel-1)]),"_selected"),
                fill = TRUE,
                fillColor = ~pal(lvlCtryData[["value"]][iterPoly]),
                fillOpacity = 0.9,
                stroke = TRUE,
                weight=4-(iterAdmLevel-1)*deltaLineWt+0.5,
                smoothFactor = 0.7,
                opacity = 1,
                color="blue",
                # dashArray = "5",
                group = "selected",
                highlight = highlightOptions(
                  weight = 5,
                  color = "blue",
                  dashArray = "",
                  fillOpacity = 0,
                  bringToFront = TRUE),
                  label = mapLabels[iterPoly],
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                    )
                )
              
              e <- extent(ctryPoly[iterPoly,])
              if (exists("mapExtent"))
              {
                mapExtent@xmin <- min(mapExtent@xmin, e@xmin)
                mapExtent@ymin <- min(mapExtent@ymin, e@ymin)
                mapExtent@xmax <- max(mapExtent@xmax, e@xmax)
                mapExtent@ymax <- max(mapExtent@ymax, e@ymax)
              }
              else
              {
                mapExtent <- e
              }
            }

        }
      map <- map %>% addLayersControl(overlayGroups = c(ctryAdmLevels[2:lyrNum], "selected"))
      
      if (admLevel != "country_code")
        map <- map %>% addLegend(position = "bottomright", 
                                 pal = pal, 
                                 values = format(ctryData$value, scientific = T),
                                 labels = quantile(ctryData$value, seq(0,1,0.1), na.rm=T),
                                 #title = "Nightlight percentiles",
                                 title = ifelse("norm_area" %in% scale, "Rad/sq. Km.", "Total Rad"),
                                 opacity = 1 )
#       #Zoom in disabled
#       if (exists("mapExtent"))
#         map <- map %>% fitBounds(mapExtent@xmin, mapExtent@ymin, mapExtent@xmax, mapExtent@ymax)
      
      map
      })
    })
    
})
