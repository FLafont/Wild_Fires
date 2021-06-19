library(shiny)
library(tidyverse)
#source("script_analyses/stat_desc_counties.R", encoding = "utf-8")
source("script_analyses/stat_desc_counties.R", encoding = "utf-8")
## pacman handle packages


# Define UI for application that draws a histogram

ui <- fluidPage(
    
    # Application title
    titlePanel("Wild Fire in the U.S"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("State",
                        "Choose a State:",
                        choices = state_names),
            
            selectInput("County",
                        "Choose a County:",
                        choices = NULL),
            # Après on mettra les années en reactive de l'état
            selectInput("Year",
                        "Choose a Year:",
                        choices = years),
            ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot"),
            tableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    ### Filtre sur la table selon l'Etat 
    df_state <- reactive({
        # Après on ajoutera possibilité de selectionner tous les états
        df <-counties_selec %>% select(NAME_1,NAME_2,fips)
        return(df)
    })
    
    
    counties <- reactive({
        req(input$State)
        filter(df_state(), NAME_1 == input$State)%>%
            pull(fips)
    })
    observeEvent(counties(), {
        choices <- unique(counties())
        updateSelectInput(inputId = "County", choices = choices)
    })
    
    
    df_county <- reactive({
        df2 <- df_lc %>%
            filter(county==input$County,annee==input$Year)%>%
            arrange(desc(broad_type_lc))%>%
            mutate(prop = round(surface/sum(surface)*100,1)) %>%
            filter(prop>.1)%>%
            mutate(ypos = cumsum(prop)- 0.5*prop )
        return(df2)
    })
 
    plot <- reactive({
        plot_ly(data=df_county(),labels=~broad_type_lc, values=~prop, type="pie") %>% 
            layout(title = "Occupation et utilisation du sol",
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

    })

    # Outputs
    output$plot <- renderPlot({plot()})
}

# Run the application 
shinyApp(ui = ui, server = server)

