# A Shiny app that shows the values for a specific municipality loaded from the Kolada API.
# server.R

library(shiny)
require(httr)
require(jsonlite)

source('C:/Users/marip/Documents/Advanced R_732A94/Labs/AdvancedRlab5/R/koladabindings.R')

server = function(input, output) {
    out <- reactive({
    municipalitiesDataFrame = get_municipality(name) # is of list class so we will convert it to a data.frame
    municipalitiesDataFrame = as.data.frame(municipalitiesDataFrame)
    municipalityId = municipalitiesDataFrame 
    })
    output$municipality = renderDataTable({
        out()
      })
}

