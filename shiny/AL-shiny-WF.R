library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(leaflet)

###Define UI

ui <- fluidPage(theme = shinytheme("lumen"),
               
                tabsetPanel(
                  tabPanel ("Etats étudiés",
                            
                            h3("Wild fires in USA"),
                            h4("Etats les plus à risque de feu"),
                            leafletOutput("mymap"),
                            p()),
                  tabPanel("Statistiques Descriptives",
                           
                           sidebarPanel(h4 ("Informations sur les Etats"),
                           selectInput(inputId = "state",
                                       label = h4("Choose a state :"), 
                                       choices = c("California","Florida", "Georgia", "Mississippi","North Carolina")),
                           uiOutput("state"),
                           hr(),
                           fluidRow(column(3, verbatimTextOutput("value")))),
                           
                           mainPanel(
                             h4 ("Graphiques"),
                             plotOutput("histo")
                           )),
                           
                  tabPanel("Prévisions")
                )
                    
                )
                  





# Define server function
server <- function(input, output) {
  
  stateInput <- reactive(input$state)
  
 
   
  output$histo <- renderPlot({
    state <- stateInput()
    presentation_etat_feux(state_name = state)})
    
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

# Create Shiny object
shinyApp(ui = ui, server = server)
