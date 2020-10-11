shinyServer(function(input, output) {
  
  reactive_data = reactive({
    municipality_id = get_municipality(input$state)$values[[1]]$id
    
    search = get_search_results(kpi_list=list("N07908"),
                                municipality_list=list(municipality_id),
                                year_list = list("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")
    )
    x = sapply(search$values, function(x) return(x$values[[1]]$value))
    
    return(x)
  })
  
  output$bar <- renderPlot({
    
    x=reactive_data()
    
    barplot(x,
            ylab = "SEK per Square Meter",
            xlab = "Year",
            names.arg = list("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")
    )
    
  })
  
})