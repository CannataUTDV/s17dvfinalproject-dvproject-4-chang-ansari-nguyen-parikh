require(dplyr)
require(ggplot2)
require(shiny)
require(shinydashboard)
require(data.world)
library(treemap)
library(remotes)

data = query(
  data.world(),
  dataset="jackchang/s-17-dv-final-project", type="sql",
  query="SELECT healthcare.`Provider State` as State,
  sum(healthcare.`Total Discharges`) as Total_Discharges
  FROM `healthcare.csv/healthcare`
  GROUP BY healthcare.`Provider State`
  ORDER BY healthcare.`Provider State`"
)

field <- c("State", "Total_Discharges")
data$labels <- do.call("paste", c(data[field], sep = "\n"))

server <- function(input, output) {
  output$plot1 <- renderPlot({
    treemap(data,
            index = "labels",
            vSize = "Total_Discharges",
            type =  "index",
            palette = "Greens",
            title = "Total Discharges per State")
  })
  output$plot2 <- renderPlot({
    brush = brushOpts(id="plot_brush", delayType = "throttle", delay = 30)
    bdf=brushedPoints(data, input$plot_brush, xvar = "State", yvar = "Total_Discharges")
    if( !is.null(input$plot_brush) ) {
      data = data %>% dplyr::filter(State %in% bdf$State)
      data %>% ggplot() + geom_histogram(aes(x=Total_Discharges))
    }
  })
}