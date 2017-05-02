require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
#install.packages("remotes")
library(remotes)
#remotes::install_github("datadotworld/data.world-r")
require(tidyr)
require(plotly)
require(treemap)
require(stringr)
require(DT)

online5 = "SQL"
online1 = "SQL"

#for scatter
data4 = query(
  data.world(propsfile = "www/.data.world"),
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

data4 = data4 %>% dplyr::mutate(InsurancePerc = (Insured_Males + Insured_Females)/(Insured_Males + Insured_Females + Noninsured_Males + Noninsured_Females))
data4 = data4 %>% dplyr::mutate(PovertyPerc = (Male_Poverty + Female_Poverty)/(Male_Poverty + Female_Poverty + Male_Above + Female_Above))


#For Bar Chart
data = query(
  data.world(propsfile = "www/.data.world"),
  dataset="jackchang/s-17-dv-project-6", type="sql",
  query="select USA_All_States.State , sum(USA_All_States.`Females 18-25 with Insurance`) as F_I, sum(USA_All_States.`Females 18-25 without insurance`) as F_N,
sum(USA_All_States.`Insured Males 18-25`) as M_I, sum(USA_All_States.`Noninsured Males 18-25`) as M_N, sum(healthcare.`Total Discharges`) AS discharges
  from USA_All_States join healthcare 
  on USA_All_States.State = healthcare.`Provider State`
  group by USA_All_States.State"
)

#For Tree Map
data2 = query(
  data.world(propsfile = "www/.data.world"),
  dataset="jackchang/s-17-dv-final-project", type="sql",
  query="SELECT healthcare.`Provider State` as State,
  sum(healthcare.`Total Discharges`) as Total_Discharges
  FROM `healthcare.csv/healthcare`
  GROUP BY healthcare.`Provider State`
  ORDER BY healthcare.`Provider State`"
)
field <- c("State", "Total_Discharges")
data2$labels <- do.call("paste", c(data2[field], sep = "\n"))


#for State Choosing
states = query(
  data.world(propsfile = "www/.data.world"),
  dataset="jackchang/s-17-dv-final-project", type="sql",
  query="select distinct USA_All_States.State as D, USA_All_States.State as R
  from USA_All_States
  order by 1"
) # %>% View()

#For Crosstab
data3 = query(
  data.world(propsfile = "www/.data.world"),
  dataset="jackchang/s-17-dv-final-project", type="sql",
  query="select healthcare.`DRG Definition` as Procedure, healthcare.`Provider State` as State, 
healthcare.`Total Discharges` as Discharges, healthcare.`Average Total Payments` as Payments, healthcare.`Average Medicare Payments`
  from healthcare
  where healthcare.`Provider State` in ('AK', 'FL', 'GA', 'LA', 'MS') and healthcare.`DRG Definition`
  in ('282 - ACUTE MYOCARDIAL INFARCTION, DISCHARGED ALIVE W/O CC/MCC', '315 - OTHER CIRCULATORY SYSTEM DIAGNOSES W CC',
  '419 - LAPAROSCOPIC CHOLECYSTECTOMY W/O C.D.E. W/O CC/MCC','439 - DISORDERS OF PANCREAS EXCEPT MALIGNANCY W CC',
  '602 - CELLULITIS W MCC','811 - RED BLOOD CELL DISORDERS W MCC')
  group by healthcare.`Provider State`,healthcare.`DRG Definition`"
)

region_list <- as.list(states$D, states$R)
region_list <- append(list("All" = "All"), region_list)
region_list5 <- region_list

data = data %>% dplyr::mutate(FemaleInsRate = F_I /(F_I + F_N), MaleInsRate = M_I / (M_I + M_N)) %>% select(State,discharges, FemaleInsRate,MaleInsRate)
male_av = mean(data$MaleInsRate)
female_av = mean(data$FemaleInsRate)
data = tidyr::gather(data, variable, InsuranceRate, -State:-discharges)
data$average = female_av
data$average[52:102]=male_av

#For ID Sets
data5 = query(
  data.world(propsfile = "www/.data.world"),
  dataset="jackchang/s-17-dv-final-project", type="sql",
  query="SELECT healthcare.`DRG Definition` as Procedure, healthcare.`Average Total Payments` as Total_Payments
  FROM healthcare
  JOIN USA_All_States ON (USA_All_States.State = healthcare.`Provider State`);"
)
data5 = data5 %>% dplyr::filter(Procedure %in% c('238 - MAJOR CARDIOVASC PROCEDURES W/O MCC', '243 - PERMANENT CARDIAC PACEMAKER IMPLANT W CC', '244 - PERMANENT CARDIAC PACEMAKER IMPLANT W/O CC/MCC', '246 - PERC CARDIOVASC PROC W DRUG-ELUTING STENT W MCC OR 4+ VESSELS/STENTS', '247 - PERC CARDIOVASC PROC W DRUG-ELUTING STENT W/O MCC', '249 - PERC CARDIOVASC PROC W NON-DRUG-ELUTING STENT W/O MCC', '251 - PERC CARDIOVASC PROC W/O CORONARY ARTERY STENT W/O MCC', '280 - ACUTE MYOCARDIAL INFARCTION, DISCHARGED ALIVE W MCC', '281 - ACUTE MYOCARDIAL INFARCTION, DISCHARGED ALIVE W CC', '282 - ACUTE MYOCARDIAL INFARCTION, DISCHARGED ALIVE W/O CC/MCC', '286 - CIRCULATORY DISORDERS EXCEPT AMI, W CARD CATH W MCC', '287 - CIRCULATORY DISORDERS EXCEPT AMI, W CARD CATH W/O MCC', '308 - CARDIAC ARRHYTHMIA  and  CONDUCTION DISORDERS W MCC', '309 - CARDIAC ARRHYTHMIA  and  CONDUCTION DISORDERS W CC', '310 - CARDIAC ARRHYTHMIA  and  CONDUCTION DISORDERS W/O CC/MCC'))

#START SERVER!!!!!!
server <- function(input, output) {
  #widgets for crosstab and boxplot
  KPI_Low = reactive({input$KPI1})     
  KPI_Medium = reactive({input$KPI2})
  
  output$boxplotRegions <- renderUI({selectInput("selectedBoxplotRegions", "Choose Regions:",
                                                 region_list5, multiple = TRUE, selected='All') })
  #Barchart
  output$plot1 <- renderPlot(height =1000, {
    ggplot(data, aes(x=State,y=InsuranceRate,fill=discharges))+ 
      scale_y_continuous()+
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_bar(stat = "identity") + 
      facet_wrap(~variable, ncol=1) +
      coord_flip()+
      geom_text(mapping=aes(x=State, y=InsuranceRate, label=round(InsuranceRate,2)),colour="black", hjust=-.5) +
      geom_text(mapping=aes(x=State, y=InsuranceRate, label=round(InsuranceRate - average,2)),colour="blue", hjust=-2) +
      # Add reference line with a label.
      geom_hline(aes(yintercept = round(average,2)), color="red") +
      geom_text(aes( -1, average, label = round(average,2), vjust = -.5, hjust = -.25), color="red")+
      labs(title="Insurance Rate by Gender and States")
    })
  
  output$barchartData1 <- renderDataTable({DT::datatable(data,rownames = FALSE,extensions = list(Responsive = TRUE, FixedHeader = TRUE))
  })
  
  #BOXPLOT
  dfbp1 <- eventReactive(input$click5, {
    if(input$selectedBoxplotRegions == 'All') region_list5 <- input$selectedBoxplotRegions
    else region_list5 <- append(list("Skip" = "Skip"), input$selectedBoxplotRegions)
    if(online5 == "SQL") {
      print("Getting from data.world")
      df <- query(
        data.world(propsfile = "www/.data.world"),
        dataset="jackchang/s-17-dv-final-project", type="sql",
        query="select USA_All_States.State as State, USA_All_States.`Females 18-25 with Insurance` as F_I, USA_All_States.`Females 18-25 without insurance` as F_N, healthcare.`DRG Definition` as Procedure,
          USA_All_States.`Insured Males 18-25` as M_I, USA_All_States.`Noninsured Males 18-25` as M_N, healthcare.`Total Discharges` AS discharges
          from USA_All_States join healthcare 
          on USA_All_States.State = healthcare.`Provider State`
          where ? = 'All' or State in (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
          ",
        queryParameters = region_list5) # %>% View()
    }
  })
  
  output$boxplotData1 <- renderDataTable({DT::datatable(dfbp1(), rownames = FALSE,extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  
  output$boxplotPlot1 <- renderPlotly({
    p <- ggplot(dfbp1()) + 
      geom_boxplot(aes(x=State, y=discharges, color=(F_I+M_I)/(F_I+F_N+M_I+M_N))) +
      theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5)) + labs(color="Insurance Rate", title="Insurance Rate versus Discharges by State")
    ggplotly(p)})
  
  #TREE MAP
  output$treemap <- renderPlot({
    treemap(data2,
            index = "labels",
            vSize = "Total_Discharges",
            type =  "index",
            palette = "Greens",
            title = "Total Discharges per State")
  })
  
  output$histogram <- renderPlotly({
    p2 <- data2 %>% ggplot() + geom_histogram(aes(x=Total_Discharges)) + labs(title="Number of States with Various Discharges")
    ggplotly(p2)
  })
  
  #CROSSTAB
  
  dfct1 <- eventReactive(input$click1, {
      print("Getting from data.world")
    query(
      data.world(propsfile = "www/.data.world"),
      dataset="jackchang/s-17-dv-final-project", type="sql",
      query="select healthcare.`DRG Definition` as Procedure, healthcare.`Provider State` as State, 
      sum(healthcare.`Total Discharges`) as Discharges, sum(healthcare.`Average Total Payments`) as Payments, sum(healthcare.`Average Medicare Payments`)
      from healthcare
      where healthcare.`Provider State` in ('AK', 'FL', 'GA', 'LA', 'MS') and healthcare.`DRG Definition`
      in ('282 - ACUTE MYOCARDIAL INFARCTION, DISCHARGED ALIVE W/O CC/MCC', '315 - OTHER CIRCULATORY SYSTEM DIAGNOSES W CC',
      '419 - LAPAROSCOPIC CHOLECYSTECTOMY W/O C.D.E. W/O CC/MCC','439 - DISORDERS OF PANCREAS EXCEPT MALIGNANCY W CC',
      '602 - CELLULITIS W MCC','811 - RED BLOOD CELL DISORDERS W MCC')
      group by healthcare.`Provider State`, healthcare.`DRG Definition`") %>% mutate(costperdischarge = Payments / Discharges, kpi = if_else(costperdischarge <= input$KPI1, '03 Low',if_else(costperdischarge <= input$KPI2, '02 Medium', '01 High'))) #%>% View()
  })
  
    output$crossdata <- renderDataTable({DT::datatable(dfct1(), rownames = FALSE,extensions = list(Responsive = TRUE, FixedHeader = TRUE)
    )
    })
    
    output$crosstab1 <- renderPlotly({
    p3 <-  ggplot(dfct1()) + geom_text(aes(x=State, y=Procedure, label = round(costperdischarge)),size = 5) +
    geom_tile(aes(x=State, y=Procedure, fill = kpi), alpha = .5) +scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n")) +labs(title="Cost per Discharge for Least Insured States")
    ggplotly(p3)
  })
    
  #SCATTER
    output$scatter <- renderPlot({
      ggplot() + geom_point(data=data4, (aes(x=InsurancePerc, y=PovertyPerc, color=State))) + geom_smooth(data=data4, aes(x=InsurancePerc, y=PovertyPerc), se=FALSE, method=lm) + scale_x_continuous(limits = c(.5, 1.05)) + scale_y_continuous(limits = c(0,.38))+labs(title="Correlation between insurance rate and poverty")
  })
    
  #ID Set Actions
    output$action1 <- renderPlot({
      ggplot(data5, (aes(x=Procedure, y=Total_Payments))) + geom_bar(stat='identity') + scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))+labs(title="Total Costs for Cardiac Procedures")
    })
    
    output$action2 <- renderPlot({
      brush = brushOpts(id="plot_brush", delayType = "throttle", delay = 30)
      bdf=brushedPoints(data5, input$plot_brush)
      if(!is.null(input$plot_brush) ) {
        data5 = data5 %>% dplyr::filter(Procedure %in% bdf$Procedure)
        ggplot(data5, aes(x=Procedure,y=Total_Payments)) + geom_bar(stat='identity')}
    })
}
