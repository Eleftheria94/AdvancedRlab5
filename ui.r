shinyUI(fluidPage(
  
  # App title ----
  titlePanel("Property price condominium"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput("state", "Choose a state:",
                  list("Stockholm","Göteborg", "Linköping", "Norrköping")
      ),
      
    ),
    mainPanel(
      plotOutput(outputId = "bar")
      
    )
  )
))