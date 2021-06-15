library(shiny)
library(leaflet)


ui <- fluidPage(
  leafletOutput("mymap"),
  p()
  #actionButton("recalc", "New points")
)

server <- function(input, output, session) {
  # 
  # points <- eventReactive(input$recalc, {
  #   cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  # }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet(states) %>% 
      addTiles()%>%
      #addTiles("CartoDB.Positron") %>%
      addPolygons(#fill = TRUE,
                  #fillColor = etats_selec
        stroke = FALSE,
        #smoothFactor = 0.2, fillOpacity = 1,
        color = ~factpal(etats_selec))
  })
}

shinyApp(ui, server)

?addPolygons
