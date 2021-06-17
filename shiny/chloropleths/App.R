


library(shiny)
library(tidyverse)
library(maps)
library(mapdata)
library(ggplot2)
library(ggthemes)
library(mapproj)


qsdf <- county.fips %>% 
  mutate(fips = ifelse(nchar(fips) == 4, paste0(0,fips), fips))

countyz <- map_data('county')

data_fl <- read.csv("florida_pred.csv")
data_nc <- read.csv("nc_pred.csv")

ui <- fluidPage(
  radioButtons(inputId = "state",
               label = "Choose a state:",
               c("Florida" =  "florida",
                  "North Carolina" = "north carolina"),
               inline = F),
  sliderInput(inputId = "yearz",
              label = "Choose a year:",
              value = 2013,
              min = 2011, 
              max = 2015),
  sliderInput(inputId = "weekk",
              label = "Week",
              value = 25,
              min = 1,
              max = 52),
  plotOutput("chloropleth"),
  #textOutput("test"),
  #tableOutput("cnty")
  
)

server <- function(input, output){
  state <- reactive(as.character(input$state))
  State <- reactive({str_to_title(input$state)})
  yearz <- reactive({input$yearz})
  weekk <- reactive({input$weekk})
  
  
  threshold <- reactive({
    if(state() == "florida"){0.423} 
    else
      if(state() == "north carolina"){0.374}
    
    })
  
  df <- reactive({
    if(state() == "florida"){
      data_fl %>% 
        select(county, date_semaine, annee, semaine, FIRE, pred) %>%
        filter(annee == yearz(), semaine == weekk()) %>% 
        rename(fips = county) %>%
        mutate(predtrue = ifelse(pred>threshold(),1,0)) %>%
        mutate(fourgroups = as.numeric(FIRE)+ predtrue) %>%
        mutate(fourgroups = ifelse(fourgroups == 1 & FIRE == TRUE, "fn", fourgroups)) %>%
        mutate(fourgroups = ifelse(fourgroups == 1 & FIRE == FALSE, "fp", fourgroups)) 
      }
    else
      if(state() == "north carolina"){
        data_nc %>% 
          select(county, date_semaine, annee, semaine, FIRE, pred) %>% 
          filter(annee == yearz(), semaine == weekk()) %>% 
          rename(fips = county) %>% 
          mutate(predtrue = ifelse(pred>threshold(),1,0)) %>%
          mutate(fourgroups = as.numeric(FIRE)+ predtrue) %>%
          mutate(fourgroups = ifelse(fourgroups == 1 & FIRE == TRUE, "fn", fourgroups)) %>% 
          mutate(fourgroups = ifelse(fourgroups == 1 & FIRE == FALSE, "fp", fourgroups)) 
      }
    })
  
  counties <- reactive({
    countyz %>% 
      filter(region == state()) %>%
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
      scale_fill_manual(values = c("2" = "red", 
                                   "0" = "#D3EAF4",
                                   "fn" = "orange", 
                                   "fp" = "#F182A4"),
                        name = "Fire risk",
                        labels = c("TN; low risk",
                                   "VP: high risk + fire",
                                   "FN: low risk + fire",
                                   "FP: high risk"))+
      theme_map() +
      coord_map('albers', lat0=30, lat1=40) +
      ggtitle(paste0(State(), " (week: ",  dateweek(),  "): Wildfire risk level per county")) +
      theme(plot.title = element_text(hjust = 0.5))
  })  
    
    
  
    
  
  
  #output$test <- renderText({dateweek()})
  #output$cnty <- renderTable(counties())
 
  
  
}

shinyApp(ui=ui, server=server)

















