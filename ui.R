
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

if (!require("pacman")) install.packages('pacman', repos='http://cran.r-project.org')

pacman::p_load(shiny, shinydashboard, leaflet, plotly)

suppressMessages(library(shiny))

suppressMessages(library(shinydashboard))

suppressMessages(library(leaflet))

suppressMessages(source("nightlights.R"))

suppressMessages(library(plotly))


filenames<-list.files(dirNlData)

ctryCodesWithData <- substr(filenames, 1, 3)

ctryCodeNames <- lapply(ctryCodesWithData, function(x) ctryCodeToNAME(x))

ctryCodesWithData <- setNames(ctryCodesWithData, ctryCodeNames)

#allCtryCodes <- getAllNlCtryCodes()
alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto;"
  )
}

#shinyUI(
  dashboardPage(

    # Application title
    dashboardHeader(title="Nightlights"),
  
    # Sidebar with a slider input for number of bins
    dashboardSidebar(
      sidebarMenu(
      
        menuItem("inputs", selected = TRUE,
                 
                 selectizeInput(inputId = "countries",
                                label = "Select Country(ies)",
                                choices = ctryCodesWithData,
                                multiple = TRUE
                 ),
                 
                 uiOutput(outputId = "intraCountry"),
                 
                 uiOutput("intraCountry1"),

                 actionButton("btnCtry", "Plot"),

#                 actionButton("btnIntraCtry", "Done"),
                 
                 menuItem(text = "options", tabName = "plots",

                          radioButtons(inputId = "graphType",
                                       label = "Graph type",
                                       choices = c("line", "boxplot", "histogram", "point"),
                                       selected = "line",
                                       inline = T
                          ),
                          
                          checkboxGroupInput(inputId = "scale",
                                        label = "Scale",
                                        choices = c("norm_area", "scale_x_log", "scale_y_log")
                          )
                )
        ),
        
        menuItem("plots", tabName = "plots", selected = TRUE),
        
        menuItem("maps", tabName = "maps"),
        
        menuItem("stats", tabName = "stats"),
        
        menuItem("models", tabName = "models"),
        
        menuItem("data", tabName = "data")
        )
      ),

      # body
      dashboardBody(
        tabItems(
          tabItem(tabName = "plots",
                   plotlyOutput(outputId = "plotNightLights"),
                   
                   uiOutput("sliderNlYearMonthRange")
                   ),

          tabItem(tabName = "maps",
                  tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                  leafletOutput("map"),
                  
                  uiOutput("sliderNlYearMonth")
                  
#                   actionButton(inputId="drawMap",
#                                label = "Draw Map")
                  )
# 
#           tabItem(tabName = "stats",
#                    textOutput("Stats")
#                   ),
#           
#           tabItem(tabName = "models",
#                    textOutput("Models")
#                    ),
#           
#           tabItem(tabName = "Data",
#                    DT::dataTableOutput(outputId = "dataset")
#                    )
        )
      )
    )
  #)
#)
