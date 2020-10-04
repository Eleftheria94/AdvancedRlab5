# ui.R

library(shiny)
require(httr)
require(jsonlite)

source('C:/Users/marip/Documents/Advanced R_732A94/Labs/AdvancedRlab5/R/koladabindings.R')

ui <- fluidPage(
    selectInput(
        inputId = "municipality",
        choices = as.data.frame(municipalitiesDataFrame$values[[1]]$title),
        label = "Select Municipality:"),
    dataTableOutput("municipalityId")
)

