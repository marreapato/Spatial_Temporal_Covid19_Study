#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#                    useShinyjs()

#install.packages("thematic")
#https://unleash-shiny.rinterface.com/web-intro.html
library(shiny)
library(plotly)
library(bslib)
library(thematic)

theme_toggle <- function() {
    div(
        class = "custom-control custom-switch", 
        tags$input(
            id = "custom_mode", 
            type = "checkbox", 
            class = "custom-control-input",
            onclick = HTML(
                "Shiny.setInputValue(
          'dark_mode', 
          document.getElementById('custom_mode').value
        );"
            )
        ),
        tags$label(
            "Custom mode?", 
            `for` = "custom_mode", 
            class = "custom-control-label"
        )
    )
}

neon_theme <- bs_theme(
    bg = "#000000", 
    fg = "#FFFFFF", 
    primary = "#9600FF", 
    secondary = "#1900A0",
    success = "#38FF12",
    info = "#00F5FB",
    warning = "#FFF100",
    danger = "#FF00E3",
    base_font = "Marker Felt",
    heading_font = "Marker Felt",
    code_font = "Chalkduster"
)
#bs_theme_preview(neon_theme, with_themer = FALSE)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = neon_theme, 
    theme_toggle(),
    img(src='https://intercom2020.ufba.br/wp-content/uploads/2020/11/ufbalogo.png', align = "right"),
    # Application title

    titlePanel("Quantidade atualizada de casos de Covid-19 no Brasil, \n dados apresentados são por data de divulgação dos boletins de cada unidade federativa."),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel("Atualizações de 3 em 3 horas,\n fonte:W. Cota, “Monitoring the number of COVID-19 cases and deaths in brazil at municipal and federative units level”, SciELOPreprints:362 (2020), 10.1590/scielopreprints.362",
                     selectInput("inSelect", "\nSelecionar Gráfico:",
                                 c("Casos Hoje", "Casos Ontem", "Média Móvel de Casos"))
                     
                     ),
        
        # Show a plot of the generated distribution
        mainPanel(
            h6("Localidades em Cinza ainda não foram divulgadas.", align = "center"),
            plotlyOutput("distPlot")
        )
    )
))
