# Functions to scrape win percentage from ESPN website

library("stringr")
library("httr")
library("plyr")

get_game_urls <- function(week_url) {
  res <- GET(week_url)
  schedule <- content(res, "text")
  sub_urls <- str_match_all(schedule, '(?<=score\\" href=\\").*?(?=")')[[1]]
  return(paste("https://www.espn.com", sub_urls, sep=""))
}

get_game_stats <- function(game_url, year) {
  id <- str_sub(game_url, -9)
  
  res <- GET(game_url)
  game_page <- content(res, "text")
  win_prcnts <- str_match_all(game_page, '(?<=homeWinPercentage\\":)[0,1](?:\\.\\d{1,3})?')[[1]]
  period_times <- str_match_all(game_page, "(?<=clock\\\":\\{\\\"displayValue\\\":\\\")\\d{1,2}:\\d{2}")[[1]]
  periods <- str_match_all(game_page, "(?<=period\\\":\\{\\\"number\\\":)\\d")[[1]]
  
  minutes <- as.integer(sub("\\:.*", "", period_times))
  # Minutes are originally given in game clock which counts down from 15 at
  # the start of the quarter to 0 at the end. This needs to be converted to 
  # starting the quarter at 0 and counting up. However, the length of overtime
  # changed from 15 minutes to 10 minutes in 2017
  if (year >= 2017) {
    minutes[which(periods < 5)] = abs(minutes[which(periods < 5)] - 15)
    minutes[which(periods >= 5)] = abs(minutes[which(periods >= 5)] - 10)
  }
  
  minutes <- minutes + 15 * (as.integer(periods) - 1) # Will break if there's ever a playoff game with double overtime
  
  seconds <- as.integer(sub(".*\\:", "", period_times))
  seconds <- abs(seconds - 60)
  
  times <- minutes * 60 + seconds # Store times in seconds from start of game
  
  df <- data.frame(ID = id, Time = times, Quarter = periods, Percent = win_prcnts, Year = year)
  df$Home_Win <- win_prcnts[length(win_prcnts)] == 1
  
  Sys.sleep(sample(0:2, 1))
  
  return(df)
}

get_week_games <- function(week_url, year) {
  games <- get_game_urls(week_url)
  week <- as.integer(str_match(week_url,"(?<=week\\/).*?(?=\\/)")[[1]])
  if (!is.na(str_match(week_url, "seasontype\\/3")[[1]])) {
    week <- week + 18
  }
  game_dfs <- lapply(games, get_game_stats, year=year)
  df <- rbind.fill(game_dfs)
  df$Week <- week
  
  return(df)
}

get_year_games <- function(base_url) {
  res <- GET(base_url)
  year <- as.integer(str_sub(base_url[1], -4))
  body <- content(res, "text")
  
  # Week 1 page's url is formatted differently so needs to be done separate
  first_week <- get_week_games(base_url, year) 
  first_week$Week <- 1
  
  week_urls <- str_match_all(body, '(?<=data-url=\\")\\/nfl\\/schedule\\/_\\/week.*?(?=\\")')[[1]]
  week_urls <- week_urls[!sapply(week_urls, grepl, pattern="seasontype/1", USE.NAMES = FALSE)] # Get rid of preseason
  week_urls <- paste("https:/www.espn.com", week_urls, sep="")
  
  game_dfs <- lapply(week_urls, get_week_games, year=year)
  game_df <- rbind.fill(game_dfs)
  df <- rbind.fill(first_week, game_df)
  return(df)
}

get_all_years <- function() {
  res <- GET("https://www.espn.com/nfl/schedule/_/year/2021")
  body <- content(res, "text")
  year_urls <- unique(str_match_all(body, '\\/nfl\\/schedule\\/_\\/year\\/20(?:1[6-9]|2[0-1])')[[1]])
  year_urls <- paste("https:/www.espn.com", year_urls, sep="")
  year_dfs <- lapply(year_urls, get_year_games)
  df <- rbind.fill(year_dfs)
  return(df)
}

stats <- get_all_years()