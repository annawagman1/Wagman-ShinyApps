#L13 Shiny II, app 2
library(shiny)

#load packages
library(ggthemes)
library(tidyverse)

#load data
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
# define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("CDC BRFSS Histograms"),
    
    sidebarLayout(
        position = "right",
        sidebarPanel(
            
            selectInput(
                inputId = "x_variable", 
                label = "Select Variable:",
                choices = list(
                    "Actual Weight"  = "weight",
                    "Desired Weight" = "wtdesire",
                    "Height"         = "height"
                ),
                selected = "Actual Weight"
            ),
            sliderInput("bins",
                        "Number of bins:",
                        min = 5,
                        max = 50,
                        value = 30,
            ),
            radioButtons(
                inputId = "fill_var" ,
                label = "Select Fill/Legend Variable:",
                choices = list(
                    "General Health",
                    "Health Coverage",
                    "Exercised in Past Month",
                    "Smoked 100 Cigarettes",
                    "Gender"
                ),
                selected = "Gender"
            )
        ),
        
        mainPanel(
            plotOutput("histPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$histPlot <- renderPlot({
        var_hist <- case_when(
            input$x_variable == "weight" ~  pull(cdc, weight),
            input$x_variable == "wtdesire" ~ pull(cdc, wtdesire),
            input$x_variable == "height" ~ pull(cdc, height)
        )
        
        x_label <- switch(
            input$x_variable,
            "weight" = "Actual Weight in Pounds",
            "wtdesire" = "Desired Weight in Pounds",
            "height" = "Height in Inches"
        )
        
        fill_hist <- switch(
            input$fill_var,
            "General Health"          = cdc$genhlth,
            "Health Coverage"         = cdc$hlthplan,
            "Exercised in Past Month" = cdc$exerany,
            "Smoked 100 Cigarettes"   = cdc$smoke100,
            "Gender"                  = cdc$gender
        )
        
        bin_breaks <- seq(min(var_hist), max(var_hist), length.out = input$bins + 1)
        
        # building histogram
        ggplot(cdc, aes(var_hist)) +
            geom_histogram(
                aes(fill = fill_hist),
                breaks = bin_breaks,
                color = "black"
            ) +
            scale_fill_discrete(
                name = input$fill_var, 
            ) +
            theme_fivethirtyeight() +
            theme(
                legend.position = "top",
                legend.direction = "horizontal",
                axis.title = element_text(),
                legend.title.align = 0.5
            ) +
            labs(
                x = x_label,
                y = "Count"
            ) +
            guides(fill = guide_legend(title.position = "top"))
        
    })
}

#run the application 
shinyApp(ui = ui, server = server)