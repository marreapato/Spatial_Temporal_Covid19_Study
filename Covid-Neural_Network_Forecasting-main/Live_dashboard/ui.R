#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Live Covid Cases Updated Time-Series"),
    
 
    # Sidebar with a slider input for number of bins
    # Sidebar with a slider input for the number of bins
    sidebarLayout(
        sidebarPanel(
            helpText(paste("Updated 7-Days Forecasts of Covid-19 New-Cases in the World Starting in",Sys.Date(),sep=" "))
        ),
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            dataTableOutput('table'),
            plotOutput("distPlot2"),
            dataTableOutput('table2'),
            plotOutput("distPlot3"),
            dataTableOutput('table3')
            
            
        )
    )
))
