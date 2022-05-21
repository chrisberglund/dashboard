library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NFL Winning percentage"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          sliderInput("minutes",
                      "Minute",
                      min = 0,
                      max = 70,
                      value = 1)
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
    df$Minute <- floor(df$Time / 60)
    
    output$gamePlot <- renderPlot({
      df <- df[which(df$Minute == input$minutes), ]
      df$Percent <- round(df$Percent, 1)
      df$Correct <- as.integer((df$Percent > 0.5 & df$Home_Win))
      correct_prcnt <- aggregate(df$Home_Win, by=list(df$Percent), FUN=sum)
      correct_count <- df %>% count(Percent)
      correct_prcnt$x <- correct_prcnt$x / correct_count$n
      ggplot(correct_prcnt, aes(x=Group.1, y=x)) + geom_line() + labs(x = "ESPN winning percentage", y = "Actual winning percentage")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
