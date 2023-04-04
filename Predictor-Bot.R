install.packages("lubridate")
install.packages("rvest")
install.packages("tidyverse")
install.packages("gt")
install.packages("gtExtras")
install.packages("hoopR")
install.packages("janitor")
install.packages("anytime")
install.packages("webshot2")

library(rtweet)
library(lubridate)
library(rvest)
library(tidyverse)
library(gt)
library(gtExtras)
library(hoopR)
library(janitor)
library(anytime)
library(webshot2)


#setwd("~/Desktop/Player of the Week Bot")

weeks <- read_csv("2023 Weeks.csv") %>% mutate(weekstart = mdy(weekstart),weekend = mdy(weekend))

#auth_as("bot_auth")

#auth_get()
#twitter_token <- get_token()

POWbot_token <- rtweet::rtweet_bot(
  api_key       = Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  api_secret    = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  access_token  = Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret = Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
)

#Read previous data
#stats_byweek <- read_csv("~/Desktop/Player of the Week Bot/Stats by Week.csv")
unique_headshots <- read_csv("Unique Headshots.csv")
#pow_weeks <- read_csv("POW Weeks.csv")
all_players <- read_csv("All Players.csv")

#Kinda ugly but gives conference by team
#conferences <- data.frame(team_short_display_name = unique(stats_byweek$team_short_display_name) %>% sort(),
#                          conference = c("East","East","East","East","East","West","West","East","East","East","West","West","East","West","East",
#                                         "West","East","West","East","West","East","East","West","West","West","West","West","West","West","East"))

conferences <- read_csv("Conferences.csv")

#Creates a model from 2016 season on
#training <- stats_byweek %>% filter(week_id <= 403 & week_id >= 314)

#log_model <- glm(pow ~ avg_prop_pts + apg + fg_percent + win_pct,data = training)

log_model <- readRDS("POW Model.rda")

#Function that takes data from hoopR and filters it between a start date and end date
fetch_games <- function(weekstart,weekend){
  #weekstart and weekend are inclusive
  
  games <- load_nba_player_box(season = 2023) %>% 
    filter(game_date >= weekstart & game_date <= weekend)
  
  team_points <- games %>% 
    group_by(team_short_display_name,game_id,game_date) %>% 
    summarise(team_points = sum(as.numeric(points),na.rm=T))
  
  
  winners <- team_points %>% 
    group_by(game_id) %>% 
    summarise(winner = team_short_display_name[which(team_points == max(team_points))])
  
  
  averages <- games %>% 
    merge(winners,by = "game_id") %>% 
    merge(team_points,by = c("team_short_display_name","game_id")) %>% 
    mutate(weekstart = weekstart,
           weekend = weekend,
           won = team_short_display_name == winner,
           pts = as.numeric(points),
           prop_points = pts / team_points,
           reb = as.numeric(rebounds),
           ast = as.numeric(assists),
           stl = as.numeric(steals),
           blk = as.numeric(blocks),
           to = as.numeric(turnovers)) %>%
    mutate(fgm = as.numeric(field_goals_made),
           fga = as.numeric(field_goals_attempted),
           `3pm` = as.numeric(three_point_field_goals_made),
           `3pa` = as.numeric(three_point_field_goals_attempted)) %>% 
    group_by(athlete_display_name,team_short_display_name,athlete_id,athlete_headshot_href,weekstart,weekend) %>% 
    summarise(games = n(),
              ppg = mean(pts),
              rpg = mean(reb),
              apg = mean(ast),
              spg = mean(stl),
              bpg = mean(blk),
              to = mean(to),
              fgm = sum(fgm),
              fga = sum(fga),
              wins = sum(won),
              avg_prop_pts = mean(prop_points),
              win_pct = mean(won),
              `fg%` = fgm / fga,
              `3pm` = sum(`3pm`),
              `3pa` = sum(`3pa`),
              `3p%` = `3pm` / `3pa`) %>% 
    merge(conferences,by = "team_short_display_name") %>% 
    distinct() %>% 
    clean_names()
}

#Creates a gt table from a dataframe of hoopR data, the conference name, a boolean for whether or not its the end of the week, and the name of the winner
predictions_output <- function(df,conf,eow = FALSE,winner = NA){
  
  #df: from fetch_games func
  #conf: "East" or "West"
  #eow: Is it the end of the week?
  #winner: winner of POW in that conf
  
  curr_week <- df %>% 
    filter(conference == conf)  
  
  curr_week$likelihood = predict(log_model,curr_week)
  
  days <- data.frame(weekstart = df$weekstart[1],
                     weekend = df$weekend[1])
  
  predictions <- curr_week %>% arrange(desc(likelihood)) %>% mutate(likelihood = ifelse(likelihood < 0,0,likelihood))
  total_likelihood <- sum(predictions$likelihood,na.rm = T)
  
  predictions <- predictions %>% mutate(likelihood = likelihood / total_likelihood)
  
  
  predictions_top <- Reduce(rbind, by(predictions, predictions["weekstart"], head, n = 5)) %>% 
    select(athlete_headshot_href,team_short_display_name,athlete_display_name,likelihood,games,ppg,rpg,apg,fg_percent,x3p_percent,win_pct,conference)
  
  if(eow == FALSE){
    gt <- predictions_top %>% 
      gt(rowname_col = "athlete_display_name") %>% 
      cols_label(athlete_headshot_href = "",
                 team_short_display_name = "Team",
                 likelihood = "Likelihood",
                 games = "Games",
                 ppg = "PPG",
                 rpg = "RPG",
                 apg = "APG",
                 fg_percent = "FG%",
                 x3p_percent = "3P%",
                 win_pct = "Win %") %>% 
      cols_hide(conference) %>% 
      fmt_number(columns = 6:8,
                 decimals = 1) %>% 
      fmt_percent(columns = c(4,9:11),
                  decimals = 1) %>% 
      gt_img_rows(athlete_headshot_href) %>% 
      tab_header(title = paste0(conf,"ern Conference: ",format(as.Date(days$weekstart),"%b %d, %Y")," through ",format(as.Date(days$weekend),"%b %d, %Y")),
                 subtitle = "Data from hoopR | Model by @tking0426 @POW_Predictor")
  }
  else{
    winners = data.frame(athlete_display_name = winner)
    
    if(!(winner %in% predictions_top$athlete_display_name)){
      winner_prediction <- predictions %>% 
        filter(athlete_display_name == winner) %>% 
        select(athlete_headshot_href,team_short_display_name,athlete_display_name,likelihood,games,ppg,rpg,apg,fg_percent,x3p_percent,win_pct,conference)
      
      predictions_top <- rbind(predictions_top,winner_prediction)
    }
    
    gt <- predictions_top %>% 
      mutate(pow = athlete_display_name %in% winners$athlete_display_name) %>% 
      gt(rowname_col = "athlete_display_name") %>% 
      cols_label(athlete_headshot_href = "",
                 team_short_display_name = "Team",
                 likelihood = "Likelihood",
                 games = "Games",
                 ppg = "PPG",
                 rpg = "RPG",
                 apg = "APG",
                 fg_percent = "FG%",
                 x3p_percent = "3P%",
                 win_pct = "Win %") %>% 
      tab_style(style = cell_fill(color = "green"),
                locations = cells_body(columns = everything(),
                                       rows = pow == TRUE)) %>% 
      cols_hide(c(conference,pow)) %>% 
      fmt_number(columns = 6:8,
                 decimals = 1) %>% 
      fmt_percent(columns = c(4,9:11),
                  decimals = 1) %>% 
      gt_img_rows(athlete_headshot_href) %>% 
      tab_header(title = paste0(conf,"ern Conference: ",format(as.Date(days$weekstart),"%b %d, %Y")," through ",format(as.Date(days$weekend),"%b %d, %Y")),
                 subtitle = "Data from hoopR | Model by @tking0426 @POW_Predictor")
  }
  
  gtsave(gt,"POW Table.png")
}


send_tweet <- function(conf,eow = FALSE){
  todays_date <- Sys.Date() - 1
  
  for(i in 1:nrow(weeks)){
    if(todays_date >= weeks$weekstart[i] & todays_date <= weeks$weekend[i]){
      weekstart <- weeks$weekstart[i]
      weekend <- weeks$weekend[i]
      week <- weeks$weeknum[i]
    }
  }
  
  predictions_output(fetch_games(weekstart, weekend), conf, eow, winner)
  
  if(wday(Sys.Date()) == 7){
    weekday = "Friday"
  } else if(wday(Sys.Date()) == 1){
    weekday = "Saturday"
  } else{
    weekday = "Test"
  }

  if(eow == FALSE){
    status = paste0(conf,"ern Conference: Week ",week, " ",weekday," Predictions")
  }
  else{
    status = paste0(conf,"ern Conference: Week ",week," Final Predictions")
  }
  
  rtweet::post_tweet(status = status,
                     media = "POW Table.png",
                     media_alt_text = paste0("Table showing the top 5 favorites for the ",conf,"ern Conference player of the week, ",format(as.Date(weekstart),"%b %d, %Y")," through ",format(as.Date(weekend),"%b %d, %Y")),
                     token = POWbot_token)
}

send_tweet("West")
send_tweet("East")
