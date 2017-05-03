#ui.R

require(shiny)
require(shinydashboard)
require(plotly)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Barchart", tabName = "barchart", icon = icon("dashboard")),
      menuItem("Box Plots", tabName = "boxplot", icon = icon("dashboard")),
      menuItem("Histogram", tabName = "histogram", icon = icon("dashboard")),
      menuItem("Crosstab", tabName = "crosstab", icon = icon("dashboard")),
      menuItem("Scatter", tabName = "scatter", icon = icon("dashboard")),
      menuItem("ID Sets", tabName = "idset", icon = icon("dashboard"))
  )),
  dashboardBody(
    tabItems(
    tabItem(tabName = "barchart",
            tabsetPanel(
              tabPanel("Data",
                       uiOutput("regions2"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                       hr(),
                       actionButton(inputId = "click2",  label = "To get data, click here"),
                       hr(), # Add space after button.
                       'Here is data for the "Barchart with Table Calculation" tab',
                       hr(),
                       DT::dataTableOutput("barchartData1")
              ),
              tabPanel("Barchart with Table Calculation", plotOutput("plot1", height=2000))
            )
    ),
    # Begin Box Plots tab content.
    tabItem(tabName = "boxplot",
            tabsetPanel(
              tabPanel("Data",
                       uiOutput("boxplotRegions"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html,
                       actionButton(inputId = "click5",  label = "To get data, click here"),
                       hr(), # Add space after button.
                       DT::dataTableOutput("boxplotData1")
              ),
              tabPanel("Simple Box Plot",
                       plotlyOutput("boxplotPlot1", height=500))
            )
    ),
    tabItem(tabName = 'histogram',tabsetPanel(
      tabPanel("Treemap", plotOutput("treemap")),
      tabPanel("Histogram",plotlyOutput("histogram"))
    )
    ),
    tabItem(tabName = 'crosstab', tabsetPanel(
      tabPanel("Data",
               sliderInput(inputId="KPI1", "KPI_Low:", 
                           min = 0, max = 500,  value = 500),
               sliderInput(inputId= "KPI2", "KPI_Medium:", 
                           min = 500, max = 1000,  value = 1000),
               actionButton(inputId = "click1",  label = "To get graph, click here"),
               hr(), # Add space after button.
               DT::dataTableOutput("crossdata")),
      tabPanel("Crosstab", plotlyOutput("crosstab1", height=1000))
    )
    ),
    tabItem(tabName = 'scatter',tabsetPanel(
                 tabPanel("Scatterplot", "Regression line is y=0.41-0.189x. The r^2 is 0.06518 and the RSME is 0.046.",plotOutput("scatter"))              
      )),
    tabItem(tabName = "idset",
            plotOutput("action1",
                       click = "plot_click",
                       dblclick = "plot_dblclick",
                       hover = "plot_hover",
                       brush = "plot_brush"
            ),
            plotOutput("action2"))
    )))
