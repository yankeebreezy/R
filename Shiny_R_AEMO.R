library(shiny)
library(shinydashboard)
library(shinyjs)
library(lubridate)
library(plotly)
library(dplyr)

ui <- dashboardPage(
  dashboardHeader(title = "AEMO Dashboard"),
  dashboardSidebar(
      useShinyjs(),
      dateInput(inputId = "day", label = "Select a Day", value = as.character("2019-09-01"), format = "yyyy-mm-dd"),
      dateRangeInput(inputId = "week", label = "Select a Week Period", start = "2019-09-01", end = "2019-09-07", format = "yyyy-mm-dd"),
      radioButtons(inputId = "radio_option", label = "Daily/Weekly/Monthly/All", choices = c("Daily" = "d", "Weekly" = "w", "Monthly" = "m", "All" = "all"), selected = "d")
    ),
    dashboardBody(
      fluidRow(h2("Insights for the month of September 2019")),
      fluidRow(plotlyOutput(outputId = "daily_plot"),
      plotlyOutput(outputId = "weekly_plot")),
      fluidRow(plotlyOutput(outputId = "monthly_plot"))
  )
)

server <- function(input, output) {
  
  aemo_data <- reactive({
    data <- read.csv2("PRICE_AND_DEMAND_NSW.csv",sep=",",stringsAsFactors = FALSE)
    data$SETTLEMENTDATE <- ymd_hms(data$SETTLEMENTDATE)
    data$RRP <- as.numeric(data$RRP)
    data
  }) 
  
  day_data <- reactive({
   day_dataset <- aemo_data()
   date <- as.character(input$day)
   day_data_boolean <- sapply(day_dataset$SETTLEMENTDATE, function(x)  date %in% format(x, "%Y-%m-%d"))
   day_data <- day_dataset[day_data_boolean,]
  })
  
  week_data <- reactive({
    week_dataset <- aemo_data()
    start <- as.character(input$week[1])
    end <- as.character(input$week[2])
    week_data_boolean <- sapply(week_dataset$SETTLEMENTDATE, function(x)  format(x, "%Y-%m-%d") >= start & format(x, "%Y-%m-%d") <= end)
    week_dataset <- week_dataset[week_data_boolean,]
  })
  
  # Observe radio button event
  observeEvent(input$radio_option, {
    if( input$radio_option == "d") {
      shinyjs::hide(id = "weekly_plot")
      shinyjs::hide(id = "monthly_plot")
      shinyjs::hide(id = "week")
      shinyjs::show(id = "day")
      shinyjs::show(id = "daily_plot")
    } else if( input$radio_option == "w") {
      shinyjs::hide(id = "daily_plot")
      shinyjs::hide(id = "monthly_plot")
      shinyjs::hide(id = "day")
      shinyjs::show(id = "week")
      shinyjs::show(id = "weekly_plot")
    } else if( input$radio_option == "m") {
      shinyjs::hide(id = "daily_plot")
      shinyjs::hide(id = "weekly_plot")
      shinyjs::hide(id = "day")
      shinyjs::hide(id = "week")
      shinyjs::show(id = "monthly_plot")
    } else {
      shinyjs::show(id = "daily_plot")
      shinyjs::show(id = "weekly_plot")
      shinyjs::show(id = "monthly_plot")
      shinyjs::show(id = "day")
      shinyjs::show(id = "week")}
  })

  output$daily_plot <- renderPlotly({
    dataset <- day_data()
    p <- ggplot(dataset, aes(x=SETTLEMENTDATE,y=RRP)) +
      geom_line(color="red")
    ggplotly(p)
  })

  output$weekly_plot <- renderPlotly({
    dataset <- week_data()
    p <- ggplot(dataset, aes(x=SETTLEMENTDATE,y=RRP)) +
      geom_line(color="green")
    ggplotly(p)
  })
  
  output$monthly_plot <- renderPlotly({
    dataset <- aemo_data()
    p <- ggplot(dataset, aes(x=SETTLEMENTDATE,y=RRP)) +
      geom_line(color="blue")
    ggplotly(p)
  })
  
}

shinyApp(ui = ui, server = server)

#rsconnect::setAccountInfo(name='ankc', token='FC1FBDD2CC15A2216827579844382E22', secret='yC66poNpy0RsFqS4uhCN1cfRc4ug6Uqegt2gwvs9')
#rsconnect::deployApp('/home/yankee/Shiny')