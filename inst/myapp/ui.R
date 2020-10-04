# ui.R

library(shiny)
require(httr)
require(jsonlite)

source('AdvancedRlab5/R/koladabindings.R')

ui <- fluidPage(
    selectInput(
        inputId = "municipality",
        choices = as.matrix(municipalitiesDataFrame["values.title"]),
        label = "Select Municipality:"),
    dataTableOutput("municipalityId")
)

