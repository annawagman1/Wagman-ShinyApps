library(shiny)
library(janitor)
library(sf)
library(raster)
library(tidyverse)



# Load map data
ny_counties <- map_data("county", "new york") 

# Define UI for application 
ui <- fluidPage(
    
    # Application title
    titlePanel(h1("New York")),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h2("Fun Facts:"), 
            p(strong("Population:"), "20 million"), 
            p(strong("State Capital:"), "Albany"), 
            p(strong("State Flower:"), "Rose"),
            p(strong("State Size:"), "54,556 mi²"),
            p(strong("Most visited attraction in the US:"), "Time Square, NYC"),
            p(strong("State Nickname:"), "The Empire State"),
            p(strong("New York City: ")), 
            img(src = "nyc.png", height = 100, width = 250), 
            p(strong("The State Flag:")), 
            img(src = "flag.png", height = 100, width = 200)),
        
        # Main panel display
        mainPanel(
            plotOutput(outputId = "plot"),
            tags$ul(
                tags$li(("New York City is the most visited city in the United States"),
                        a("Click here to learn more about New York City: ", href = "https://www1.nyc.gov/")),
                tags$li(("The first pizzeria in the United States opened in NYC in 1895"),
                        a("Learn about the best pizza in New York: ", href = "https://ny.eater.com/maps/nyc-best-iconic-pizza-pizzeria")),  
                tags$li(("To learn more on New York, click here: "), 
                        a("ny.gov", href = "https://www.ny.gov/"))
            )
        )
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$plot <- renderPlot({
        ggplot(
            data = ny_counties,
            mapping = aes(long, lat)) +
            geom_polygon(aes(group = group), fill = "#ADD8E7", color = "black") +
            coord_quickmap() +
            theme_void()
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
