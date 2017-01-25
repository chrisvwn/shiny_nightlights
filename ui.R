
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

suppressMessages(library(shiny))

suppressMessages(source("nightlights.R"))


filenames<-list.files(dirNlData)

ctryCodesWithData <- substr(filenames, 1, 3)

ctryCodeNames <- lapply(ctryCodesWithData, function(x) ctryCodeToNAME(x))

ctryCodesWithData <- setNames(ctryCodesWithData, ctryCodeNames)

#allCtryCodes <- getAllNlCtryCodes()

shinyUI(
  fluidPage(

    # Application title
    titlePanel("NightLights"),
  
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
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
                     inline = T),
        
        checkboxInput(inputId = "norm_area",
                      label = "Normalize by Area",
                      value = FALSE
                      ),
        
        radioButtons(inputId = "graphtype",
                     label = "Graph type",
                     choices = c("boxplot", "histogram", "line"),
                     selected = "VIIRS",
                     inline = T),
        
        sliderInput(inputId = "time",
                    label = "Time",
                    min = 2012,
                    max = 2016,
                    timeFormat = "%Y-%m-%d",
                    step = 1,
                    value = c(2012,2016)
                    )
        
        # uiOutput(output$interCountry)
      ),

      # Show a plot of
      mainPanel(
        tabsetPanel(
          tabPanel("Plot",
                   plotOutput(outputId = "plotNightLights")
                   ),
          tabPanel("Data",
                   tableOutput(outputId = "dataset"))
        )
        
      )
    )
  )
)
