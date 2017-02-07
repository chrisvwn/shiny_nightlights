
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
alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto;"
  )
}


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

        checkboxInput(inputId = "scale_y_log",
                      label = "Log Scale Y",
                      value = FALSE
                      ),
        
        radioButtons(inputId = "graphtype",
                     label = "Graph type",
                     choices = c("boxplot", "histogram", "line", "point"),
                     selected = "boxplot",
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
                   plotOutput(outputId = "plotNightLights"),

                    sliderInput(inputId = "time",
                    label = "Time",
                    min = as.Date("2012-04-01", "%Y-%m-%d"),
                    max = as.Date("2016-12-31", "%Y-%m-%d"),
                    timeFormat = "%Y-%m",
                    step = 1,
                    value = c(as.Date("2012-01-01","%Y-%m-%d"),as.Date("2016-12-31","%Y-%m-%d")),
                    width = 700
                    )
                   ),
          tabPanel("Data",
                   tableOutput(outputId = "dataset"))
        )
        
      )
    )
  )
)
