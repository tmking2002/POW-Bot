library(tidyverse)
library(gt)
library(gtExtras)
library(hoopR)
library(janitor)
library(anytime)

#Read previous data
stats_byweek <- read_csv("~/Desktop/Player of the Week Bot/Stats by Week.csv")
unique_headshots <- read_csv("~/Desktop/Player of the Week Bot/Unique Headshots.csv")
pow_weeks <- read_csv("~/Desktop/Player of the Week Bot/POW Weeks.csv")
all_players <- read_csv("~/Desktop/Player of the Week Bot/All Players.csv")


#LOOK AT THIS AFTER FIRST POW ANNOUNCED
#pow_list <- "https://basketball.realgm.com/nba/awards/by-type/Player-Of-The-Week/30/2023" %>% read_html() %>% html_table() 
#pow_df <- pow_list[[1]] %>% 
#  select(Season,Player,Conference,Date) %>% 
#  mutate(Date = as.Date(anydate(Date)))

#Kinda ugly but gives conference by team
conferences <- data.frame(team_short_display_name = unique(stats_byweek$team_short_display_name) %>% sort(),
                          conference = c("East","East","East","East","East","West","West","East","East","East","West","West","East","West","East",
                                         "West","East","West","East","West","East","East","West","West","West","West","West","West","West","East"))

#Creates a model from 2016 season on
training <- stats_byweek %>% filter(week_id <= 403 & week_id >= 314)

#test_model <- stats_byweek %>% filter(week_id <= 440 & week_id > 403)

log_model <- glm(pow ~ avg_prop_pts + apg + fg_percent + win_pct,data = training)

#Function that takes data from hoopR and filters it between a start date and end date
fetch_games <- function(weekstart,weekend){
  #weekstart and weekend are inclusive
  
  gameids <- espn_nba_scoreboard(season = 2022) %>% filter(game_date <= weekend & game_date >= weekstart & status_name == "STATUS_FINAL")
  
  games <- data.frame(matrix(nrow = 0,ncol = 32))
  names(games) = names(espn_nba_player_box(401360358))
  
  for(i in 1:nrow(gameids)){
    curr <- espn_nba_player_box(gameids$game_id[i]) %>% mutate(game_id = gameids$game_id[i],game_date = gameids$game_date[i])
    
    games <- rbind(games,curr)
  }
  
  #games <- load_nba_player_box(season = 2023) %>% 
  #  filter(game_date >= weekstart & game_date <= weekend)
  
  team_points <- games %>% 
    group_by(team_short_display_name,game_id,game_date) %>% 
    summarise(points = sum(as.numeric(pts)))
  
  
  winners <- team_points %>% 
    group_by(game_id) %>% 
    summarise(winner = team_short_display_name[which(points == max(points))])
  
  
  averages <- games %>% 
    merge(winners,by = "game_id") %>% 
    merge(team_points,by = c("team_short_display_name","game_id")) %>% 
    mutate(weekstart = weekstart,
           weekend = weekend,
           won = team_short_display_name == winner,
           pts = as.numeric(pts),
           prop_points = pts / points,
           reb = as.numeric(reb),
           ast = as.numeric(ast),
           stl = as.numeric(stl),
           blk = as.numeric(blk),
           to = as.numeric(to)) %>%
    separate(fg, c("fgm","fga")) %>% 
    separate(fg3, c("3pm","3pa")) %>% 
    mutate(fgm = as.numeric(fgm),
           fga = as.numeric(fga),
           `3pm` = as.numeric(`3pm`),
           `3pa` = as.numeric(`3pa`)) %>% 
    group_by(athlete_display_name,team_short_display_name,athlete_id,weekstart,weekend) %>% 
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
    merge(unique_headshots,by = "athlete_id") %>% 
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
      tab_header(title = paste0(conf,"ern Conference: ",format(as.Date(days$weekstart),"%b %d, %Y")," through ",format(as.Date(days$weekend),"%b %d, %Y")))
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
  
  gtsave(gt,"~/Desktop/Player of the Week Bot/POW Table.png")
}


#df <- fetch_games("2022-03-07","2022-03-13")
