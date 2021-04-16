library(shiny)
library(skimr)


library(sf)

## pacman handle packages
source("script/15_03_wild_fires.R", encoding = "utf-8")
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
            # Après on mettra les années en reactive de l'état
            sliderInput("year_range","Select years:",
                        min = min(years),
                        max = max(years),
                        value = c(min(years),max(years)),
                        round = TRUE,
                        step = 1
                        ),
            radioButtons('indicateur', "Choisir un indicateur",
                         choices = indicateurs,
                         selected = 'number_of_fires'),
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
        df <- fires_per_year_state %>% 
            filter(STATE %in% input$State,
                   between(FIRE_YEAR,
                           input$year_range[1],
                           input$year_range[2]))
        return(df)
    })
    
    desc <- reactive({
        table <- skim(select_if(df_state(), ~is.numeric(.)))
        return(table)
    })
    plot <- reactive({
        df_state() %>%
            ggplot(aes(x=FIRE_YEAR, y = !!as.name(input$indicateur))) +
            geom_col(fill="red",color=NA,alpha=1/2) +
            #geom_line(colour="red",alpha=1/2) +
            theme_bw()+
            labs(x="",y=names(indicateurs[indicateurs==input$indicateur]))
    })
    
    # Outputs
    output$plot <- renderPlot({plot()})
    output$table <- renderTable({desc()})
}

# Run the application 
shinyApp(ui = ui, server = server)

