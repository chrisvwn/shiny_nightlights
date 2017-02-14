
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

suppressMessages(library(shiny))

suppressMessages(library(shinydashboard))

suppressMessages(library(leaflet))

suppressMessages(source("nightlights.R"))


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
      
        menuItem("plots", tabName = "plots", selected = TRUE),
        
        menuItem("maps", tabName = "maps"),
        
        menuItem("stats", tabName = "stats"),
        
        menuItem("models", tabName = "models"),
        
        menuItem("data", tabName = "data"),
                  
        menuItem("inputs", selected = TRUE,
                 
          selectizeInput(inputId = "countries",
                      label = "Select Country(ies)",
                      choices = ctryCodesWithData,
                      multiple = TRUE
                      ),
          
          uiOutput("intraCountry"),
          
          radioButtons(inputId = "nltype",
                       label = "Nightlight type",
                       choices = c("OLS", "VIIRS"),
                       selected = "VIIRS",
                       inline = T
                       ),
          
          checkboxInput(inputId = "norm_area",
                        label = "Normalize by Area",
                        value = FALSE
                        ),
  
          checkboxInput(inputId = "scale_x_log",
                        label = "Log Scale X",
                        value = FALSE
                        ),
  
          checkboxInput(inputId = "scale_y_log",
                        label = "Log Scale Y",
                        value = FALSE
                        ),
          
          radioButtons(inputId = "graphtype",
                       label = "Graph type",
                       choices = c("boxplot", "histogram", "line", "point"),
                       selected = "boxplot",
                       inline = T
                       )
        ))
      ),

      # body
      dashboardBody(
        tabItems(
          tabItem(tabName = "plots",
                   plotOutput(outputId = "plotNightLights"),
                   
                   uiOutput("sliderNlYearMonthRange")
                   ),

          tabItem(tabName = "maps",
                  leafletOutput("map"),
                  
                  uiOutput("sliderNlYearMonth")
                  ),

          tabItem(tabName = "stats",
                   textOutput("Stats")
                  ),
          
          tabItem(tabName = "models",
                   textOutput("Models")
                   ),
          
          tabItem(tabName = "Data",
                   DT::dataTableOutput(outputId = "dataset")
                   )
        )
      )
    )
  #)
#)
