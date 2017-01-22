
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

suppressMessages(library(shiny))

suppressMessages(source("nightlights.R"))


filenames<-list.files(dirNlData)

ctryCodesWithData <- substr(filenames, 1, 3)

allCtryCodes <- getAllNlCtryCodes()

shinyUI(
  fluidPage(

    # Application title
    titlePanel("NightLights"),
  
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(inputId = "countries",
                    label = "Select Country(ies)",
                    choices = ctryCodesWithData
                    ),
        
        uiOutput("intraCountry"),
        
        radioButtons(inputId = "nltype",
                     label = "Nightlight type",
                     choices = c("OLS", "VIIRS"),
                     selected = "VIIRS",
                     inline = T),
        uiOutput("message")
        
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
