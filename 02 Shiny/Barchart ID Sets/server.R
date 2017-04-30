require(dplyr)
require(ggplot2)
require(shiny)
require(shinydashboard)
require(data.world)
library(remotes)

#query
data = query(
  data.world(propsfile = "www/.data.world"),
  dataset="jackchang/s-17-dv-final-project", type="sql",
  query="SELECT healthcare.`DRG Definition` as Procedure, healthcare.`Average Total Payments` as Total_Payments
  FROM healthcare
  JOIN USA_All_States ON (USA_All_States.State = healthcare.`Provider State`);"
)

data = data %>% dplyr::filter(Procedure %in% c('238 - MAJOR CARDIOVASC PROCEDURES W/O MCC', '243 - PERMANENT CARDIAC PACEMAKER IMPLANT W CC', '244 - PERMANENT CARDIAC PACEMAKER IMPLANT W/O CC/MCC', '246 - PERC CARDIOVASC PROC W DRUG-ELUTING STENT W MCC OR 4+ VESSELS/STENTS', '247 - PERC CARDIOVASC PROC W DRUG-ELUTING STENT W/O MCC', '249 - PERC CARDIOVASC PROC W NON-DRUG-ELUTING STENT W/O MCC', '251 - PERC CARDIOVASC PROC W/O CORONARY ARTERY STENT W/O MCC', '280 - ACUTE MYOCARDIAL INFARCTION, DISCHARGED ALIVE W MCC', '281 - ACUTE MYOCARDIAL INFARCTION, DISCHARGED ALIVE W CC', '282 - ACUTE MYOCARDIAL INFARCTION, DISCHARGED ALIVE W/O CC/MCC', '286 - CIRCULATORY DISORDERS EXCEPT AMI, W CARD CATH W MCC', '287 - CIRCULATORY DISORDERS EXCEPT AMI, W CARD CATH W/O MCC', '308 - CARDIAC ARRHYTHMIA  and  CONDUCTION DISORDERS W MCC', '309 - CARDIAC ARRHYTHMIA  and  CONDUCTION DISORDERS W CC', '310 - CARDIAC ARRHYTHMIA  and  CONDUCTION DISORDERS W/O CC/MCC'))

server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(data, (aes(x=Procedure, y=Total_Payments))) + geom_bar(stat='identity') + scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
  })
  
  output$plot2 <- renderPlot({
    brush = brushOpts(id="plot_brush", delayType = "throttle", delay = 30)
    bdf=brushedPoints(data, input$plot_brush)
    if(!is.null(input$plot_brush) ) {
      data = data %>% dplyr::filter(Procedure %in% bdf$Procedure)
      ggplot(data, aes(x=Procedure,y=Total_Payments)) + geom_bar(stat='identity')
    } 
  })
}
