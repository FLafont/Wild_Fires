library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(leaflet)
library(tidyverse)
library(maps)
library(mapdata)
library(ggplot2)
library(ggthemes)
library(mapproj)
library(readxl)

source("prep_shiny_data_v3.R",encoding = "utf-8")
source("stat_desc_counties.R",encoding = "utf-8")

qsdf <- county.fips %>% 
  mutate(fips = ifelse(nchar(fips) == 4, paste0(0,fips), fips))

countyz <- map_data('county')

data_fl <- read.csv("florida_pred.csv")
data_nc <- read.csv("nc_pred.csv")
data_gg <- read.csv("georgia_pred.csv")
data_ms <- read.csv("mississippi_pred.csv")
#data_cf <- read.csv("california_pred.csv")

state_names <- unique(counties_selec$NAME_1)
counties_names <- unique(counties_selec$NAME_2)


###Define UI

ui <- fluidPage(theme = shinytheme("lumen"),
               
                tabsetPanel(
                  tabPanel ("Etats étudiés",
                            
                            h3("Wild fires in USA"),
                            h4("Etats les plus à risque de feu"),
                            leafletOutput("mymap"),
                            p()),
                  
                  tabPanel("Descriptions des états",
                           
                           fluidRow(
                           column(width=3,
                                  h4 ("Informations sur les Etats"),
                                  selectInput(inputId = "state",
                                       label = h4("Choose a state :"), 
                                       choices = c("California","Florida", "Georgia", "Mississippi","North Carolina")),
                           uiOutput("state"),
                           hr(),
                           fluidRow(column(3, verbatimTextOutput("value")))),
                           
                           column(width=9,
                             h4 ("Graphiques"),
                             plotOutput("histo")
                           ))),
                 
                  tabPanel("Description des comtés",
                           fluidRow(
                             column(width = 3,
                                    selectInput("State",
                                                "Choose a State:",
                                                choices = state_names),
                                    uiOutput("state_name"),
                                    selectInput(inputId="county",
                                                label=h4("Choose a county :"),
                                                choices=NULL),
                                    sliderInput(inputId = "year",
                                                label = h4("Choose a year:"),
                                                value = 2014,
                                                min = 2013, 
                                                max = 2015,sep="")),
                             
                             column(width=9,
                                    h4("Graphiques"),
                                    plotOutput("meteo")
                                  )
                           )),
                            
                  tabPanel("Prévisions",
                           sidebarLayout(
                             sidebarPanel(
                               radioButtons(inputId = "staate",
                                            label = "Choose a state:",
                                            c("California" = "california",
                                              "Florida" =  "florida",
                                              "Georgia"= "georgia",
                                              "Mississippi" = "mississippi",
                                              "North Carolina" = "north carolina"),
                                            inline = F),
                               sliderInput(inputId = "yearz",
                                           label = "Choose a year:",
                                           value = 2013,
                                           min = 2011, 
                                           max = 2015,sep=""),
                               sliderInput(inputId = "weekk",
                                           label = "Week",
                                           value = 25,
                                           min = 1,
                                           max = 52)
                             ),
                             mainPanel(plotOutput("chloropleth"))
                           ))))
                
                    
                
                  





# Define server function
server <- function(input, output,session) {
  
 ####Onglet Etats sélectionnés
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

  
  ####Onglet stats Etats

  stateInput <- reactive(input$state)
  
  
  
  output$histo <- renderPlot({
    state <- stateInput()
    presentation_etat_feux(state_name = state)})
  
  #####Onglet stats counties
  
 
  countyInput <- reactive(filter(counties_selec, NAME_1==input$State) %>% 
    pull(NAME_2) )
  
  df_state <- reactive({
    # Après on ajoutera possibilité de selectionner tous les états
    df <-counties_selec %>% select(NAME_1,NAME_2,fips)
    return(df)
  })
  
  # counties <- reactive({
  #   req(input$State)
  #   filter(df_state(), NAME_1 == input$State)%>%
  #     pull(fips)
  # })
  observeEvent(countyInput(), {
    choices <- unique(countyInput())
    updateSelectInput(inputId = "county", choices = choices,session=session)
  })
  
  yearInput <- reactive(as.character(input$year))
  
  output$meteo <- renderPlot({
  #   fips <- countyInput()
     name_county <- input$county
     state <- input$State
     year <- yearInput()
     plot_counties(state,year,name_county)
    
  })
  
  
  ####Onglet prévisions
  staate <- reactive(as.character(input$staate))
  Staate <- reactive({str_to_title(input$staate)})
  yearz <- reactive({input$yearz})
  weekk <- reactive({input$weekk})
  
  
  threshold <- reactive({
    if(staate() == "california"){0.5} 
    else
      if(staate() == "florida"){0.423}
    else
      if(staate() == "georgia"){0.5964007}
    else
      if(staate() == "mississippi"){0.5077671}
    else
      if(staate() == "north carolina"){0.374}
    
  })
  
  df <- reactive({
    if(staate() == "florida"){
      data_fl %>% 
        select(county, date_semaine, annee, semaine, FIRE, pred) %>%
        filter(annee == yearz(), semaine == weekk()) %>% 
        rename(fips = county) %>%
        mutate(predtrue = ifelse(pred>threshold(),1,0)) %>%
        mutate(fourgroups = as.numeric(FIRE)+ predtrue) %>%
        mutate(fourgroups = ifelse(fourgroups == 1 & FIRE == TRUE, "FN", fourgroups)) %>%
        mutate(fourgroups = ifelse(fourgroups == 1 & FIRE == FALSE, "FP", fourgroups)) %>% 
        mutate(fourgroups = ifelse(fourgroups == 2, "TP", fourgroups)) %>% 
        mutate(fourgroups = ifelse(fourgroups == 0, "TN", fourgroups))
    }
    else
      if(staate() == "north carolina"){
        data_nc %>% 
          select(county, date_semaine, annee, semaine, FIRE, pred) %>% 
          filter(annee == yearz(), semaine == weekk()) %>% 
          rename(fips = county) %>% 
          mutate(predtrue = ifelse(pred>threshold(),1,0)) %>%
          mutate(fourgroups = as.numeric(FIRE)+ predtrue) %>%
          mutate(fourgroups = ifelse(fourgroups == 1 & FIRE == TRUE, "FN", fourgroups)) %>% 
          mutate(fourgroups = ifelse(fourgroups == 1 & FIRE == FALSE, "FP", fourgroups)) %>% 
          mutate(fourgroups = ifelse(fourgroups == 2, "TP", fourgroups)) %>% 
          mutate(fourgroups = ifelse(fourgroups == 0, "TN", fourgroups))
      }
    else
      if(staate() == "georgia"){
        data_gg %>% 
          select(county, date_semaine, annee, semaine, FIRE, pred) %>% 
          filter(annee == yearz(), semaine == weekk()) %>% 
          rename(fips = county) %>% 
          mutate(predtrue = ifelse(pred>threshold(),1,0)) %>%
          mutate(fourgroups = as.numeric(FIRE)+ predtrue) %>%
          mutate(fourgroups = ifelse(fourgroups == 1 & FIRE == TRUE, "FN", fourgroups)) %>% 
          mutate(fourgroups = ifelse(fourgroups == 1 & FIRE == FALSE, "FP", fourgroups)) %>% 
          mutate(fourgroups = ifelse(fourgroups == 2, "TP", fourgroups)) %>% 
          mutate(fourgroups = ifelse(fourgroups == 0, "TN", fourgroups)) 
      }
    else
      if(staate() == "mississippi"){
        data_ms %>% 
          select(county, date_semaine, annee, semaine, FIRE, pred) %>% 
          filter(annee == yearz(), semaine == weekk()) %>% 
          rename(fips = county) %>% 
          mutate(predtrue = ifelse(pred>threshold(),1,0)) %>%
          mutate(fourgroups = as.numeric(FIRE)+ predtrue) %>%
          mutate(fourgroups = ifelse(fourgroups == 1 & FIRE == TRUE, "FN", fourgroups)) %>% 
          mutate(fourgroups = ifelse(fourgroups == 1 & FIRE == FALSE, "FP", fourgroups)) %>% 
          mutate(fourgroups = ifelse(fourgroups == 2, "TP", fourgroups)) %>% 
          mutate(fourgroups = ifelse(fourgroups == 0, "TN", fourgroups)) 
      }
    
  })
  
  counties <- reactive({
    countyz %>% 
      filter(region == staate()) %>%
      mutate(polyname = paste0(region,",",subregion)) %>%
      left_join(qsdf) %>% 
      mutate(fips = as.integer(fips)) %>% 
      left_join(df()) %>% 
      arrange(order) 
  })
  
  dateweek <- reactive({
    counties() %>% 
      select(date_semaine) %>% 
      drop_na() %>% 
      distinct() %>% 
      as.character
    
  })
  
  
  output$chloropleth <- renderPlot({ggplot(data = counties(), 
                                           aes(x = long, 
                                               y = lat, 
                                               group = group, 
                                               fill = as.character(fourgroups)))+
      geom_polygon()+
      geom_path(color = 'grey', size = 0.1)+
      scale_fill_manual(values = c("TP" = "red", 
                                   "TN" = "#D3EAF4",
                                   "FN" = "orange", 
                                   "FP" = "#F182A4"),
                        name = "Fire risk")+
      theme_map() +
      coord_map('albers', lat0=30, lat1=40) +
      ggtitle(paste0(Staate(), " (week: ",  dateweek(),  "): Wildfire risk level / occurence per county")) +
      theme(plot.title = element_text(hjust = 0.5))
  }) 
 
}

# Create Shiny object
shinyApp(ui = ui, server = server)
