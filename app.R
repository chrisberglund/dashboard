#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("gamePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    df <- read.csv("~/Desktop/football_stats.csv")
    game <- df[which(df$ID == 401326316),]
    
    output$gamePlot <- renderPlot({
        ggplot(game, aes(x=Time, y=Percent)) + geom_line()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
