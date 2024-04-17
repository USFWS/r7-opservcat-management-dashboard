library(shiny)
library(shinydashboard)
library(shinyBS)
library(bootstrap)
library(shinycssloaders)
library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(plotly)
library(stringr)

source("helper_functions.R")
source("get_data.R")
source("ui_outputs.R")
source("global.R")

box1 <- paste("Alaska Maritime<br>Alaska Peninsula/Becharof<br>Arctic<br>Izembek<br>Kanuti<br>Kenai<br>Kodiak<br>Koyukuk/Nowitna/Innoko<br>",strong("Selawik"),"<br>",strong("Tetlin"),"<br>",strong("Togiak"),"<br>Yukon Delta<br>", strong("Yukon Flats"))
box2 <- paste("Alaska Maritime<br>Alaska Peninsula/Becharof<br>Arctic<br>Izembek<br>Kanuti<br>Kenai<br>Kodiak<br>",strong("Koyukuk/Nowitna/Innoko"),"<br>Selawik<br>Tetlin<br>",strong("Togiak"),"<br>Yukon Delta<br>",strong("Yukon Flats"))
box3 <- paste("Alaska Maritime<br>Alaska Peninsula/Becharof<br>",strong("Arctic"),"<br>Izembek<br>",strong("Kanuti"),"<br>",strong("Kenai x 2"),"<br>",strong("Kodiak x 2"),"<br>", strong("Koyukuk/Nowitna/Innoko"),"<br>",strong("Selawik"),"<br>",strong("Tetlin"),"<br>",strong("Togiak"),"<br>",strong("Yukon Delta"),"<br>",strong("Yukon Flats"))
box4 <- paste("Alaska Maritime<br>",strong("Alaska Peninsula/Becharof"),"<br>Arctic<br>",strong("Izembek"),"<br>Kanuti<br>",strong("Kenai"),"<br>Kodiak<br>Koyukuk/Nowitna/Innoko<br>Selawik<br>",strong("Tetlin"),"<br>Togiak<br>",strong("Yukon Delta"),"<br>Yukon Flats")
box5 <- paste("Alaska Maritime<br>Alaska Peninsula/Becharof<br>Arctic<br>Izembek<br>Kanuti<br>",strong("Kenai"),"<br>Kodiak<br>",strong("Koyukuk/Nowitna/Innoko"),"<br>Selawik<br>Tetlin<br>",strong("Togiak"),"<br>",strong("Yukon Delta"),"<br>",strong("Yukon Flats"))

popover <- paste("References can pertain to multiple refuges and have multiple organizations specified. As a result, summing up the refuge columns would lead to double counting some references.")

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Operation ServCat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Refuge Progress", tabName = "refuge", icon = icon("house")),
      menuItem("ARLIS Progress", tabName = "arlis", icon = icon("book")),
      menuItem("Region Progress", tabName = "region", icon = icon("earth-americas"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "refuge",
              tags$head(tags$style(
                HTML("
                  .small-box {height:110px; background-color: #377CA4 !important;}
                  .info-box {height:90px; color: black;}
                  .info-box .info-box-icon {background-color: #377CA4 !important; text-align: center !important;}
                  .box{background-color: #377CA4 !important; text-align: center; color: white !important; font-size: 14pt;}
                  .skin-blue .main-header .logo {background-color: #2A5F7E !important;}
                  .skin-blue .main-header .navbar {background-color: #2A5F7E !important;}
                  .irs-grid-text { font-size: 12pt;}
                  .irs--shiny .irs-bar {
                    background: black;
                    border-top: 1px solid black;
                    border-bottom: 1px solid black;
                  }
                  span {text-align: left !important;}
                  .div-1 {text-align: left !important; font-size: 12pt; padding-left:6em}
                  .popover-content{color: #000000;}
                  .popover-title{color: #000000;}
                  .div-button-1 {padding-right: 15px;}
                  .div-button-2 {padding-right: 40px;}
                  ")
                )
              ),
              fluidRow(
                column(8,
                       box(uiOutput("get_title1"),
                           fluidRow(
                             column(6, align = "left", span(sliderInput("sliderRef", "Select Year Range for Plot:", min = 2011, max = as.integer(format(Sys.Date(), "%Y")), value = c(2011, as.integer(format(Sys.Date(), "%Y"))), step = 1, sep = "", width = "300px"))),
                             column(6, align = "right", br(), br(), div(class="div-button-1", downloadButton("downloadRef", "Download Dataset (.csv)", icon = shiny::icon("download"), width = 12, style = "background-color:#000000; border-color:#000000; border-width: 1px; color:#FFFFFF; font-size:16px; border-radius: 20px; height: 40px; text-align: center; display:table-cell; vertical-align:middle;")))
                           ),
                           withSpinner(plotlyOutput("plot_refuge")),
                           width = NULL, height = "750px")
                ),
                column(4,
                       fluidRow(column(12, h3(em("Hover over the boxes below.")))),
                       h4(strong("Completion Rates")),
                       fluidRow(column(12, div(id="box1",infoBox(title = "Refuge Completion of Hardcopy Effort", value = p(style = "font-size: 30pt", "30.8%"), icon = icon("file-lines"), color = "light-blue", width = NULL)))),
                       fluidRow(column(12, div(id="box2",infoBox(title = "Refuge Completion of Digital Effort", value = p(style = "font-size: 30pt", "23.1%"), icon = icon("computer-mouse"), color = "light-blue", width = NULL)))),
                       h4(strong("Participation Rates")),
                       fluidRow(column(12, div(id="box3",valueBox("10 of 13 Refuges", "Have Hosted In-Person Visits", icon = icon("person"), color = "light-blue", width = NULL)))),
                       fluidRow(column(12, div(id="box4",valueBox("5 of 13 Refuges", "Have Mailed Hardcopies", icon = icon("box-open"), color = "light-blue", width = NULL)))),
                       fluidRow(column(12, div(id="box5",valueBox("5 of 13 Refuges", "Have Sent Digital Files", icon = icon("cloud-arrow-down"), color = "light-blue", width = NULL)))),
                       p(em("Section Last Updated: 04/17/24")),
                       bsPopover(id = "box1", title = "Refuges/Complexes", content = box1, placement = "left", trigger = "hover", options = NULL),
                       bsPopover(id = "box2", title = "Refuges/Complexes", content = box2, placement = "left", trigger = "hover", options = NULL),
                       bsPopover(id = "box3", title = "Refuges/Complexes", content = box3, placement = "left", trigger = "hover", options = NULL),
                       bsPopover(id = "box4", title = "Refuges/Complexes", content = box4, placement = "left", trigger = "hover", options = NULL),
                       bsPopover(id = "box5", title = "Refuges/Complexes", content = box5, placement = "left", trigger = "hover", options = NULL)
                )
              )
      ),
      tabItem(tabName = "arlis",
              fluidRow(
                column(3, box(span(sliderInput("sliderArl", "Select Year Range for Page:", min = 2015, max = as.integer(format(Sys.Date(), "%Y")), value = c(2015, as.integer(format(Sys.Date(), "%Y"))), step = 1, sep = "")), width = NULL, height = "113px")),
                column(3, valueBox(textOutput("get_arlis_total"), "All References Added by ARLIS", icon = icon("chart-line"), width = NULL)),
                column(3, valueBox(textOutput("get_arlis_regtotal"), "Region 7 References Added by ARLIS", icon = icon("chart-pie"), width = NULL)),
                column(3, valueBox(textOutput("get_arlis_avg"), "Average References Added by ARLIS per Year", icon = icon("weight-scale"), width = NULL))
              ),
              fluidRow(
                column(12,
                       box(uiOutput("get_title2"),
                           #downloadButton("downloadArl", "Download Dataset (.csv)", icon = shiny::icon("download"), width = 12, style = "background-color:#002B47; border-color:#D3D3D3; border-width: 1px; color:#FFFFFF; font-size:16px; border-radius: 20px; height: 40px; text-align: center; display:table-cell; vertical-align:middle;"),
                           #div(class="div-1", id = "div1", icon("circle-info"), em("Why does the sum of the refuge columns not match up with the Region 7 Refuges Total?")),
                           fluidRow(
                             column(7, align = "left", div(class="div-1", id = "div1", icon("circle-info"),em("Why does the sum of the refuge columns not match up with the Region 7 Refuges Total?"))),
                             column(5, align = "right", div(class="div-button-2", downloadButton("downloadArl", "Download Dataset (.csv)", icon = shiny::icon("download"), width = 12, style = "background-color:#000000; border-color:#000000; border-width: 1px; color:#FFFFFF; font-size:16px; border-radius: 20px; height: 40px; text-align: center; display:table-cell; vertical-align:middle;"))),
                           ),
                           withSpinner(plotlyOutput("plot_arlis")),
                           width = NULL, height = "690px"),
                       bsPopover(id = "div1", title = "Helpful Info", content = popover, placement = "top", trigger = "hover", options = NULL)
                )
              )
      ),
      tabItem(tabName = "region",
              fluidRow(
                column(6,
                       box(uiOutput("get_title3"),
                           fluidRow(
                             column(6, align = "left", span(sliderInput("sliderReg", "Select Year Range for Plot:", min = 2011, max = as.integer(format(Sys.Date(), "%Y")), value = c(as.integer(format(Sys.Date(), "%Y"))-1, as.integer(format(Sys.Date(), "%Y"))), step = 1, sep = "", width = "300px"))),
                             column(6, align = "right", br(), br(), div(class="div-button-2", downloadButton("downloadReg", "Download Dataset (.csv)", icon = shiny::icon("download"), width = 12, style = "background-color:#000000; border-color:#000000; border-width: 1px; color:#FFFFFF; font-size:16px; border-radius: 20px; height: 40px; text-align: center; display:table-cell; vertical-align:middle;")))
                           ),
                           withSpinner(plotlyOutput("plot_region")),
                           width = NULL, height="700px")
                ),
                column(6,
                       box(h3(strong("Cumulative Total of Region 7 References")), withSpinner(plotlyOutput("plot_region_cumul")), width = NULL, height="700px")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  output$plot_refuge <- renderPlotly({
    plot_refuge_progress(input$sliderRef[1], input$sliderRef[2])
  })
  
  output$plot_arlis<- renderPlotly({
    subplot(plot_arlis_progress_1(input$sliderArl[1], input$sliderArl[2]), plot_arlis_progress_2(input$sliderArl[1], input$sliderArl[2]), widths = c(0.75,0.25), margin = 0.03) %>% layout(yaxis = list(title = '<b> Reference Count </b>', color = "white"))
  })
  
  output$plot_arlis_2 <- renderPlotly({
    plot_arlis_progress_2(input$sliderArl[1], input$sliderArl[2])
  })
  
  output$plot_region <- renderPlotly({
    plot_region_progress(input$sliderReg[1], input$sliderReg[2])
  })
  
  output$plot_region_cumul <- renderPlotly({
    plot_region_cumul()
  })
  
  output$get_arlis_total <- renderText({
    get_arlis_total(input$sliderArl[1], input$sliderArl[2])
  })
  
  output$get_arlis_regtotal <- renderText({
    get_arlis_regtotal(input$sliderArl[1], input$sliderArl[2])
  })
  
  output$get_arlis_avg <- renderText({
    get_arlis_avg(input$sliderArl[1], input$sliderArl[2])
  })
  
  output$get_title1 <- renderUI({
    if(input$sliderRef[1] == input$sliderRef[2]){
      title <- paste("References Added by Refuge in", input$sliderRef[1])
    }else{
      title <- paste("References Added by Refuge From", input$sliderRef[1], "Through", input$sliderRef[2])
    }
    h3(strong(title))
  })
  
  output$get_title2 <- renderUI({
    if(input$sliderArl[1] == input$sliderArl[2]){
      title <- paste("References Added by ARLIS in", input$sliderArl[1])
    }else{
      title <- paste("References Added by ARLIS From", input$sliderArl[1], "Through", input$sliderArl[2])
    }
    h3(strong(title))
  })
  
  output$get_title3 <- renderUI({
    if(input$sliderReg[1] == input$sliderReg[2]){
      title <- paste("References Added by Region in", input$sliderReg[1])
    }else{
      title <- paste("References Added by Region From", input$sliderReg[1], "Through", input$sliderReg[2])
    }
    h3(strong(title))
  })
  
  output$downloadRef <- downloadHandler(
    filename = "refuge_progress_table.csv",
    content = function(file){
      write.csv(make_download_table(refuge_totals_df), file, row.names = FALSE)
    }
  )
  
  output$downloadArl <- downloadHandler(
    filename = "arlis_progress_table.csv",
    content = function(file){
      write.csv(make_download_table(arlis_totals_df), file, row.names = FALSE)
    }
  )
  
  output$downloadReg <- downloadHandler(
    filename = "region_progress_table.csv",
    content = function(file){
      write.csv(make_download_table(region_totals_df), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)