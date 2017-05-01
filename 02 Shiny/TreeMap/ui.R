#ui.R

require(shiny)
require(shinydashboard)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tree Map", tabName = "Treemap", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Treemap",
              plotOutput("plot1",
              click = "plot_click",
              dblclick = "plot_dblclick",
              hover = "plot_hover",
              brush = "plot_brush"
      ),
      plotOutput("plot2")
      )
    )
  )
)
