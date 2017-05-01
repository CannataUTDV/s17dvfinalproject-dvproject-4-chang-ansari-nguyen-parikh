require(dplyr)
require(ggplot2)
require(shiny)
require(shinydashboard)
require(data.world)
library(remotes)

#query
data = query(
  data.world(),
  dataset="jackchang/s-17-dv-final-project", type="sql",
  query="SELECT healthcare.`Provider State` as State,
  `USA_All_States.csv/USA_All_States`.`Insured Males 18-25` as Insured_Males,
  `USA_All_States.csv/USA_All_States`.`Noninsured Males 18-25` as Noninsured_Males,
  `USA_All_States.csv/USA_All_States`.`Females 18-25 with Insurance` as Insured_Females,
  `USA_All_States.csv/USA_All_States`.`Females 18-25 without insurance` as Noninsured_Females,
  `Poverty.csv/Poverty`.B17001_024 as Female_Poverty,
  `Poverty.csv/Poverty`.B17001_010 as Male_Poverty,
  `Poverty.csv/Poverty`.B17001_053 as Female_Above,
  `Poverty.csv/Poverty`.B17001_039 as Male_Above
  FROM `USA_All_States.csv/USA_All_States`
  JOIN healthcare ON (healthcare.`Provider State` = USA_All_States.State)
  JOIN `Poverty.csv/Poverty` ON (healthcare.`Provider State` = `Poverty.csv/Poverty`.State)
  GROUP BY healthcare.`Provider State`
  ORDER BY USA_All_States.State;"
)

data = data %>% dplyr::mutate(InsurancePerc = (Insured_Males + Insured_Females)/(Insured_Males + Insured_Females + Noninsured_Males + Noninsured_Females))
data = data %>% dplyr::mutate(PovertyPerc = (Male_Poverty + Female_Poverty)/(Male_Poverty + Female_Poverty + Male_Above + Female_Above))

server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot() + geom_point(data=data, (aes(x=InsurancePerc, y=PovertyPerc, color=State))) + geom_smooth(data=data, aes(x=InsurancePerc, y=PovertyPerc), se=FALSE, method=lm) + scale_x_continuous(limits = c(.5, .95)) + scale_y_continuous(limits = c(0,.38))
  })
}
