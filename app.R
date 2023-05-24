library(shiny)
library(shinydashboard)
library(shinyBS)

source("plot_arlisM.R")
source("get_arlisM.R")
source("get_fundingM.R")
source("plot_refugeM.R")
source("plot_yearsM.R")
source("plot_indivM.R")

#Drop down list
refuges <- list("Alaska Maritime",
                "APB",
                "Arctic",
                "Izembek",
                "Kanuti",
                "Kenai",
                "KNI",
                "Kodiak",
                "Selawik",
                "Tetlin",
                "Togiak",
                "Yukon Delta",
                "Yukon Flats")

ui <- dashboardPage(
  dashboardHeader(title = "Operation ServCat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Project Progress", tabName = "op", icon = icon("list-check")),
      menuItem("Tracking Individuals", tabName = "indiv", icon = icon("user-tie")),
      menuItem("Overall ServCat Stats", tabName = "serv", icon = icon("chart-simple"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "op",
              fluidRow(
                column(8,
                       box(title = "ARLIS Inputs by Refuge", plotOutput("plot_arlis"), background = "light-blue", width = NULL)
                ),
                column(4,
                       fluidRow(column(12, strong("Directions:"),p("Hover over the boxes below."))),
                       fluidRow(column(12, div(id="box1",valueBox("3 Refuges", "Have Been Visited In Person", icon = icon("person"), color = "light-blue", width = NULL)))),
                       fluidRow(column(12, div(id="box2", valueBox("5 Refuges", "Have Sent Hard Copies", icon = icon("file"), color = "light-blue", width = NULL)))),
                       fluidRow(column(12, div(id="box3",valueBox("3 Refuges", "Have Uploaded Digital Files", icon = icon("desktop"), color = "light-blue", width = NULL)))),
                       bsPopover(id = "box1", title = "Refuges/Complexes", content = "Kenai, Kodiak, Togiak", placement = "left", trigger = "hover", options = NULL),
                       bsPopover(id = "box2", title = "Refuges/Complexes", content = "APB, Izembek, Kenai, Tetlin, Yukon Delta", placement = "left", trigger = "hover", options = NULL),
                       bsPopover(id = "box3", title = "Refuges/Complexes", content = "Kenai, Togiak, Yukon Delta", placement = "left", trigger = "hover", options = NULL)
                )
              ),
              fluidRow(
                infoBox("Total Uploads by ARLIS", textOutput("get_arlis"), "Since Agreement Start (9/2018)", icon = icon("stats", lib="glyphicon"), color = "light-blue", width = 4),
                infoBox("Funding for ARLIS", textOutput("funding"), "Since Agreement Start (9/2018)", icon = icon("dollar-sign"), color = "light-blue", width = 4)
              )
      ),
      tabItem(tabName = "serv",
              fluidRow(
                column(6,
                       box(title = "Total Files by Refuge", plotOutput("refuge"), background = "light-blue", width = NULL)
                ),
                column(6,
                       box(title = "Files Added by Year for Region 7 Refuge Program", plotOutput("years"), background = "light-blue", width = NULL)
                )
              )
      ),
      tabItem(tabName = "indiv",
              fluidRow(
                column(10,
                       selectInput(inputId = "dropdown", label = "Select refuge:", refuges),
                       box(plotOutput("indiv"), background = "light-blue", width = NULL)
                       )
              )
      )
    )
  )
)

server <- function(input, output) {
  output$plot_arlis <- renderPlot({
    plot_arlisM()
  })
  
  output$get_arlis <- renderText({
    get_arlisM()
  })
  
  output$funding <- renderText({
    get_fundingM()
  })
  
  output$refuge <- renderPlot({
    plot_refugeM()
  })
  
  output$years <- renderPlot({
    plot_yearsM()
  })
  
  output$indiv <- renderPlot({
    plot_indivM(input$dropdown)
  })
}

shinyApp(ui, server)