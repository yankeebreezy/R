library(shiny)
library(shinydashboard)
library(shinyjs)
library(lubridate)
library(plotly)
library(dplyr)

shinyUI(dashboardPage(
  dashboardHeader(title = "AEMO Price Insight Dashboard"),
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
))
