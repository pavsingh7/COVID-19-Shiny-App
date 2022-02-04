
#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4
#/&^%$#@-)+#4582*39)@(*#>) Pavan Singh #/&^%$#@-)+#4582*39)@(*#>)
#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4

# load libraryies and packages used
library(shiny) # for deployment and converting to HTML
library(ggplot2) # plotting
library(shinythemes) # package  extending shiny capabilities
library(plotly) # graphing / plotting
# thematic package which automatically themes ggplot2, lattice, and base plots


# source() causes R to accept its input from the named file
source("readIn.R") # read data from github and manipulate  it  with dpylr
source("long.R") # change data from wide format to long, using melt
source("theme.R") #theme for plots

#app.R file with components: ui, server,  

#Side Panel will have the widget where users can select the input 
#layout inspired by: https://mastering-shiny.org/action-layout.html
# fluidPage(
#  titlePanel(),
#  sidebarLayout(
#    sidebarPanel(),
#    mainPanel()
#  )
# )
# https://d33wubrfki0l68.cloudfront.net/37aa2b1c61a6141cc95188bffd0cfc782fdb27d5/b6aa6/diagrams/action-layout/sidebar.png

# simple way to break up a page into pieces is to use tabsetPanel() and its close friend tabPanel()
# tabsetPanel() creates a container for any number of tabPanels()
# yeti theme from shinytheme is used


# possible to add your own HTML to the ui 
# by including literal HTML with the HTML() function.

# Plotly in Shiny - https://plotly-r.com/linking-views-with-shiny.html



#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4
#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4

#/&^%$#@-)+#4582*39)@(*#>) User Interface (UI)

#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4
#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4


ui <- fluidPage(
  theme = shinytheme("yeti"),
  
  tags$style(HTML('table.dataTable tr:nth-child(even) {color: black !important;}')),
  tags$style(HTML('table.dataTable tr:nth-child(odd) {background-color: grey !important;}')),
  tags$style(HTML('table.dataTable th {color: blue !important;}')),
  tags$style(HTML('table.dataTable th {background-color: white !important;}')),
  tags$style(HTML('label {color: black}')),
  
  title = "COVID-19",
  titlePanel(h3("COVID-19 Map w/Country-level Data")),
  sidebarLayout(
    mainPanel(
      tabsetPanel(
        tabPanel(
          "County-wide Statistics",
          plotlyOutput("allCountries"),
          hr(), # horizontal rule
          ), # end tabPanel-1
        tabPanel("Tabular Data",
                 DT::DTOutput("tabular")     #  DTOutput() is an alias of dataTableOutput()
                 )
      ) # end tabsetPanel
    ), # end mainPanel
    sidebarPanel(
      helpText("Use the widget below to reactively
               generate the plot elements."),
      selectInput("countrySelector",
                  label="Select a Country",
                  choices=c("Singapore", "Japan", "Indonesia", "US", "Malaysia", "China", "South Africa"),
                  selected="Singapore"),
      dateRangeInput("dateSelector",
                     label = "Observation Period",
                     start = as.Date("2020-01-01"),
                     end = Sys.Date(),
                     min = as.Date("2019-12-01"),
                     max = Sys.Date(),
                     separator = "to"
                    ),
      checkboxInput("smoothSelector",  # want a single check-box for a single yes/no question
                    label = "Smoother?", value=FALSE),
      textInput("titleInput", label = "Plot Title"),
      downloadButton("saveAction", label = "Download Plot", style="background:#F4D258;color:black")
    )
  )
)


#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4
#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4

#/&^%$#@-)+#4582*39)@(*#>) Server (Backend)

#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4
#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4


corona <- read_github() #data from github, formatted to d
long <- tolong(corona) #change formatted (wide) country data to long format data

# In the long format, each row is one time point per subject
# https://www.theanalysisfactor.com/wide-and-long-data/#:~:text=In%20the%20long%20format%2C%20each,below%20in%20the%20long%20format.

server <- function(input, output){
  
  #REACTIVE EXPRESSION FOR EFFICIENCY
  dat <- reactive({    #reactive expression give Shiny more information so that it can do less re-computation when inputs change
    x <- subset(long, 
           (country == input$countrySelector
            & date >= input$dateSelector[1]
            & date < input$dateSelector[2]
            ))
    return(x)
  })
  #https://mastering-shiny.org/basic-reactivity.html#reactive-expressions-1
  
  #PLOT ON MAIN PANEL
  output$allCountries <- renderPlotly({
    geoms <- switch(as.character(input$smoothSelector), 
                    "TRUE"=geom_smooth(size=1.2), 
                    "FALSE"=geom_line(size=1.2))
    ggplotly(
      ggplot(data=dat(), 
             aes(x=date, y=value, col=variable)) +
        geoms +
        labs(title = input$titleInput) +
        grim
    )
  })
  
  #SAVE BUTTOM
  output$saveAction <- downloadHandler(    # https://shiny.rstudio.com/reference/shiny/0.14/downloadHandler.html
    filename = 'plotFromApp.png',
    content = function(file) {
      ggsave(file, dat())                  # ggsave() is a convenient function for saving a plot.
    }
  )

  #TABLE OUTPUT ON OTHER TAB
  output$tabular <- DT::renderDT({
    x <- corona[corona$country == input$countrySelector,]
    return(x)
  })
  
}

#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4
#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4
#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4
#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4
#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4
#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4
#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4582*39)@(*#>)#/&^%$#@-)+#4


shinyApp(ui, server)


# deployment was to shinyapps.io servers
