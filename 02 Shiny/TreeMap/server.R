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

}