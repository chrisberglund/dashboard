library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(

    titlePanel("NFL Winning percentage"),

    sidebarLayout(
        sidebarPanel(
          sliderInput("minutes",
                      "Minute",
                      min = 0,
                      max = 70,
                      value = 1)
        ),

        mainPanel(
          plotOutput("flaccoMeter"),
           plotOutput("gamePlot")
        )
    )
)

server <- function(input, output) {
    df <- read.csv("~/Desktop/football_stats.csv")
    df$Minute <- floor(df$Time / 60)
    
    flacco_search <- read.csv("~/Desktop/joe_flacco.csv")
    flac_stats <- read.csv("~/Desktop/flacco_stats.csv")
    flac_stats <- flac_stats[which(!is.na(as.integer(flac_stats$TD))),]
    flac_stats$TD <- as.integer(flac_stats$TD)
    flac_stats$Date <- as.Date(flac_stats$Date)
    month_td <-flac_stats %>%
      mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
      group_by(month, year) %>%
      summarise(total = sum(TD))
    month_td <- month_td[which(!is.na(month_td$month)),]
    month_td$Date <- paste(month_td$year, month_td$month, sep="-")
    
    output$flaccoMeter <- renderPlot({
      ggplot() + geom_line(aes(x=flacco_search$Month, y=flacco_search$Searches, group = 1)) + 
        geom_point(aes(x=month_td$Date, y=month_td$total))
    })
    
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

shinyApp(ui = ui, server = server)
