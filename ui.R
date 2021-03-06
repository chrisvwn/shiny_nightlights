
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# if (!require("pacman")) install.packages('pacman', repos='http://cran.r-project.org')
# 
# pacman::p_load(shiny, shinydashboard, plotly)
# 
# pacman::p_load_gh("rstudio/leaflet", "cloudyr/aws.s3") #get the github version of leaflet
# 
# suppressMessages(library(shiny))
# 
# suppressMessages(library(shinydashboard))
# 
# suppressMessages(library(leaflet))
# 
# suppressMessages(library(plotly))
# 
# suppressMessages(library(aws.s3))

#suppressMessages(source("nightlights.R"))

#source("nightlights.R")

if (!requireNamespace("shiny", quietly = TRUE))
{
  stop("Pkg shiny needed for this function to work. Please install missing packages.", call. = FALSE)
}

if (!requireNamespace("shinydashboard", quietly = TRUE))
{
  stop("Pkg shinydashboard needed for this function to work. Please install missing packages.", call. = FALSE)
}

if (!requireNamespace("leaflet", quietly = TRUE))
{
  stop("Pkg leaflet needed for this function to work. Please install missing packages.", call. = FALSE)
}

if (!requireNamespace("plotly", quietly = TRUE))
{
  stop("Pkg plotly needed for this function to work. Please install missing packages.", call. = FALSE)
}

library(rnightlights)

filenames <- list.files(file.path(rnightlights::pkg_options("rootPath"), rnightlights::pkg_options("dirNlData")))

print(file.path(rnightlights::pkg_options("rootPath"), rnightlights::pkg_options("dirNlData")))

ctryCodesWithData <- substr(filenames, 1, 3)

ctryCodeNames <- lapply(ctryCodesWithData, function(x) ctryCodeToName(x))

ctryCodesWithData <- stats::setNames(ctryCodesWithData, ctryCodeNames)

#allCtryCodes <- getAllNlCtryCodes()
alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto;"
  )
}

#shinyUI(
  shinydashboard::dashboardPage(

    # Application title
    shinydashboard::dashboardHeader(title="Nightlights"),
  
    # Sidebar with a slider input for number of bins
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
      
        shinydashboard::menuItem("inputs", selected = TRUE,
                 
                 shiny::selectizeInput(inputId = "countries",
                                label = "Select Country(ies)",
                                choices = ctryCodesWithData,
                                multiple = TRUE
                 ),
                 
                 shiny::uiOutput("radioStats"),
                 
                 shiny::uiOutput(outputId = "intraCountry"),
                 
                 shiny::uiOutput("intraCountry1"),

                 shiny::actionButton("btnGo", "Go"),

#                 actionButton("btnIntraCtry", "Done"),
                 
                 shinydashboard::menuItem(text = "options", tabName = "plots",

                          shiny::radioButtons(inputId = "graphType",
                                       label = "Graph type",
                                       choices = c("line", "boxplot", "histogram", "point"),
                                       selected = "line",
                                       inline = T
                          ),
                          
                          shiny::checkboxGroupInput(inputId = "scale",
                                        label = "Scale",
                                        choices = c("norm_area", "scale_x_log", "scale_y_log")
                          )
                )
        ),
        
        shinydashboard::menuItem("plots", tabName = "plots"),
        
        shinydashboard::menuItem("maps", tabName = "maps"),
        
        shinydashboard::menuItem("stats", tabName = "stats"),
        
        shinydashboard::menuItem("models", tabName = "models"),
        
        shinydashboard::menuItem("data", tabName = "data")
        )
      ),

      # body
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "plots",
                   plotly::plotlyOutput(outputId = "plotNightLights"),
                   
                   shiny::uiOutput("sliderNlYearMonthRange")
                   ),

          shinydashboard::tabItem(tabName = "maps",
                  tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                  leaflet::leafletOutput("map"),
                  
                  shiny::uiOutput("sliderNlYearMonth")
                  
#                   actionButton(inputId="drawMap",
#                                label = "Draw Map")
                  ),

          shinydashboard::tabItem(tabName = "stats",
                   shiny::fluidRow(
                     shinydashboard::box(title = "Annual Trends", 
                         plotly::plotlyOutput("plotYearly")),
                     
                     shinydashboard::tabBox(
                       shiny::tabPanel(title = "plotPointsCluster",
                                plotly::plotlyOutput("plotPointsCluster")
                                ),
                       
                       shiny::tabPanel(title = "plotHCluster",
                                shiny::plotOutput("plotHCluster")
                                ),
                       shiny::tabPanel(title = "mapHCluster",
                                leaflet::leafletOutput("mapHCluster")
                       ),
                       shiny::sliderInput("kClusters", "Num Clusters", min=1, max=10, value=2)
                     )
                   ),
                  
                  shiny::fluidRow(
                    shinydashboard::tabBox(
                      shiny::tabPanel(title = "Time Series Decomposed",
                               shiny::plotOutput("plotTSDecomposed")
                      
                      )
                    )
                  )
                  ),
          
          shinydashboard::tabItem(tabName = "models",
                   shiny::textOutput("Models")
                   ),
          
          shinydashboard::tabItem(tabName = "Data",
                   dataTableOutput(outputId = "dataset")
                   )
        )
      )
    )
  #)
#)
