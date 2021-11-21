# Loading Packages
library(shiny)
library(tidyverse)
library(ggthemes)

# Load Data
cdc <- read_delim("cdc.txt", delim = "|") %>%
    mutate(
        exerany = factor(exerany, levels = c(1, 0), labels = c("Yes", "No")),
        hlthplan = factor(hlthplan, levels = c(1, 0), labels = c("Yes", "No")),
        smoke100 = factor(smoke100, levels = c(1, 0), labels = c("Yes", "No")),
        gender = factor(gender, levels = c("f", "m"), labels = c("Female", "Male")),
        genhlth = factor(genhlth,
                         levels = c("excellent", "very good", "good", "fair", "poor"),
                         labels = c("Excellent", "Very Good", "Good", "Fair", "Poor"))
    )

# Define UI for application that draws a histogram ----
ui <- fluidPage(
    
    # Application title
    titlePanel("CDC BRFSS: Histogram of Weight Grouped by Gender"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(position = "right",
                  sidebarPanel(
                      sliderInput("bins",
                                  "Number of bins:",
                                  min = 5,
                                  max = 50,
                                  value = 30)
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                      plotOutput("histPlot")
                  )
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
    
    output$histPlot <- renderPlot({
        
        x <- cdc$weight
        bin_breaks <- seq(min(x), max(x), length.out = input$bins + 1)
        
        ggplot(cdc, aes(weight)) + 
            geom_histogram(aes(fill = cdc$gender),
                           breaks = bin_breaks,
                           color = "black") +
            theme_minimal() +
            theme(legend.position = c(0.48, 0.75)) +
            labs(
                x = "Weight in Pounds",
                y = "Count") + 
            scale_fill_discrete("Gender")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
