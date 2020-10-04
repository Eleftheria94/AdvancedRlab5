# A Shiny app that shows the values for a specific municipality loaded from the Kolada API.
# server.R

library(shiny)
require(httr)
require(jsonlite)

source('AdvancedRlab5/R/koladabindings.R')

server = function(input, output) {
    municipalitiesDataFrame = get_municipality()
    municipalityId = as.numeric(as.matrix(municipalitiesDataFrame["values.id"])[match(
        as.character(input$municipality),
        as.matrix(municipalitiesDataFrame["values.title"])
    )])
    output$municipality = renderDataTable({
        print(input$municipality)
    })
}

shinyApp(ui = ui, server = server)