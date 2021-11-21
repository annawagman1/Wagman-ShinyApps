library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Hello World!"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 5,
                        max = 50,
                        value = 30)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful$waiting
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        ggplot(faithful, 
               aes(waiting)) +
            geom_histogram(
                breaks = bins,
                fill = "pink", 
                color = "blue") +
            theme_classic() +
            theme(
                plot.title = element_text(face = "bold", hjust = 0.5),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank()
            ) +
            labs(
                title = "Histogram of waiting times",
                x = "Waiting time to next eruption (in mins)",
                y = "Frequency"
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
