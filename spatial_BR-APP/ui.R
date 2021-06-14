#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Quantidade atualizada de casos de Covid-19 no Brasil, \n dados apresentados são por data de divulgação dos boletins de cada unidade federativa."),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel("Atualizações de 3 em 3 horas,\n fonte:W. Cota, “Monitoring the number of COVID-19 cases and deaths in brazil at municipal and federative units level”, SciELOPreprints:362 (2020), 10.1590/scielopreprints.362"),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("distPlot"),
            plotlyOutput("distPlot2"),
            plotlyOutput("distPlot3")
        )
    )
))
