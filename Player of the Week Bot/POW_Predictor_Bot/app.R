library(shiny)

library(rsconnect)

rsconnect::setAccountInfo(name='tyson-king', 
                          token='7B171C23BE1E9335A868448501855CC2', 
                          secret='1QWICfR3NXKG+BRlVp25tqE3K0zc1NIgTUtsGYT3')

rsconnect::deployApp("~/Desktop/Player of the Week Bot/POW_Predictor_Bot")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("TEMPORARY APP"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
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
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
