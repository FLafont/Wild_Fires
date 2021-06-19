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
data_cf <- read.csv("california_pred.csv")  

data_cf$FIRE[data_cf$FIRE == "FEU"] <- TRUE
data_cf$FIRE[data_cf$FIRE == "NON_FEU"] <- FALSE
data_cf$FIRE <- as.logical(data_cf$FIRE)


threshy <- read.csv("threshy.csv")

state_names <- unique(counties_selec$NAME_1)
counties_names <- unique(counties_selec$NAME_2)


###Define UI

ui <- fluidPage(theme = shinytheme("lumen"),
                
                tabsetPanel(
                  tabPanel ("Etats étudiés",
                            fluidRow(
                              width=12,
                              h3("Wild fires in USA"),
                              h4("Etats les plus à risque de feu"),
                              leafletOutput("mymap"),
                              p()
                              ),
                            fluidRow(
                              column(width=2,
                                     h4 ("Informations sur les Etats"),
                                     selectInput(inputId = "state",
                                                 label = h4("Choose a state :"), 
                                                 choices = c("California","Florida", "Georgia", "Mississippi","North Carolina")),
                                     uiOutput("state"),
                                     hr(),
                                     fluidRow(column(3, verbatimTextOutput("value")))),
                              
                              column(width=8,
                                     h4 (""),
                                     plotOutput("histo")),
                              
                              column(width=2)
                              
                            )
                            ),
                
                  
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
                             column(width=7,
                                    h4(""),
                                    leafletOutput("countymap")),
                             column(width = 2)
                           ),
                           
                           fluidRow(
                             
                             column(width=12,
                                    h4("Eléments liés aux risques de feu"),
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
  observeEvent(input$mymap_marker_click, {
    name = input$mymap_marker_click$id
    print(id)
    #id = input$mymap_marker_click$
    showModal(modalDialog(
      title = paste0("Etat sélectionné: ",name,"."),
      HTML(paste0("Nombre de counties avec un risque non-négligeable de feux: ",
                  filter(centers,NAME ==name)$n,".",
                  "<br>",
                  "Nombre de feux sur la période dans ces counties: ",
                  round(filter(centers,NAME ==name)$number_of_fires),".",
                  "<br>",
                  "Surface totale brûlée sur la période (en hectares): ",
                  round(filter(centers,NAME ==name)$number_of_ha_burned),".")
      )
    ))
  })
  
  output$mymap <- renderLeaflet({
    leaflet(states) %>% 
      addTiles()%>%
      addPolygons(
        stroke = FALSE,
        label =   ~NAME,
        color = ~factpal(etats_selec))%>%
      addMarkers(layerId =  ~centers$NAME,
                 data=centers)
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
  
  # Carte county
  output$countymap <- renderLeaflet({
    map_county_selec(state_name = input$State,
                     county_name =input$county )
  })
  
  
  ####Onglet prévisions
  staate <- reactive(as.character(input$staate))
  Staate <- reactive({str_to_title(input$staate)})
  yearz <- reactive({input$yearz})
  weekk <- reactive({input$weekk})
  
  
  threshold <- reactive({
    if(staate() == "california"){
      threshy %>% 
        filter(stat=="california") %>% 
        select(threshi) %>% 
        as.numeric()
    } 
    else
      if(staate() == "florida"){
        threshy %>% 
          filter(stat=="florida") %>% 
          select(threshi) %>% 
          as.numeric()
      }
    else
      if(staate() == "georgia"){
        threshy %>% 
          filter(stat=="georgia") %>% 
          select(threshi) %>% 
          as.numeric()
      }
    else
      if(staate() == "mississippi"){
        threshy %>% 
          filter(stat=="mississippi") %>% 
          select(threshi) %>% 
          as.numeric()
      }
    else
      if(staate() == "north carolina"){
        threshy %>% 
          filter(stat=="northcarolina") %>% 
          select(threshi) %>% 
          as.numeric()
      }
    
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
    else
      if(staate() == "california"){
        data_cf %>% 
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
