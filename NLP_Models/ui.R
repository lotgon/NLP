library(shiny)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

# Define the overall UI
shinyUI(
  fluidPage(
    titlePanel("Kneser-Ney Smoothing"),
    
    sidebarLayout(
      sidebarPanel(
        textInput("tokens", "Enter first words:", ""),
        actionButton("goButton", "Go!"),
        h6("Press go button or add space to get predictions of the next word")
        
      ),
    mainPanel(
      fluidRow(
        
        DT::dataTableOutput("tableOutput"),
        column(6,
        h4("Kneser-Ney algorithm"),
        DT::dataTableOutput("tableKneserOutput")
        ),
        column(6, 
        h4("Naive algorithm w/smoothing"),
        DT::dataTableOutput("tableNaiveOutput")
        ))
    )
  )
)
)