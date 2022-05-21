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
  game_dfs <- lapply(games, get_game_stats, year=year)
  df <- rbind.fill(game_dfs)
  return(df)
}

get_year_games <- function(base_url, year) {
  res <- GET(base_url)
  body <- content(res, "text")
  first_week <- get_week_games(base_url, year)
  week_urls <- str_match_all(body, '(?<=data-url=\\")\\/nfl\\/schedule\\/_\\/week.*?(?=\\")')[[1]]
  week_urls <- week_urls[!sapply(week_urls, grepl, pattern="seasontype/1", USE.NAMES = FALSE)] # Get rid of preseason
  week_urls <- paste("https:/www.espn.com", week_urls, sep="")
  
  game_dfs <- lapply(week_urls, get_week_games, year=year)
  game_df <- rbind.fill(game_dfs)
  df <- rbind.fill(first_week, game_df)
  
  return(df)
}

stats <- get_year_games("https://www.espn.com/nfl/schedule/_/year/2021", 2021)