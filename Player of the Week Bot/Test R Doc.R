library(hoopR)
library(tidyverse)
library(janitor)
library(lubridate)
library(anytime)
library(rvest)

all_players <- nba_commonallplayers()[[1]] %>% 
  filter(2020 <= TO_YEAR)  %>% 
  select(DISPLAY_FIRST_LAST,PERSON_ID,TEAM_NAME)

#write_csv(all_players,"~/Desktop/Player of the Week Bot/All Players.csv")

raw_box <- load_nba_player_box(seasons = 2002:2021) %>% 
  filter(pts != "--" & team_short_display_name %in% unique(all_players$TEAM_NAME) & season_type == 2) 

team_points <- raw_box %>% 
  group_by(team_short_display_name,game_id,game_date) %>% 
  summarise(points = sum(as.numeric(pts)))

winners <- team_points %>% 
  group_by(game_id) %>% 
  summarise(winner = team_short_display_name[which(points == max(points))])


pow_list <- "https://basketball.realgm.com/nba/awards/by-type/Player-Of-The-Week/30" %>% read_html() %>% html_table() 
pow_df <- pow_list[[13]] %>% 
  select(Season,Player,Conference,Date) %>% 
  mutate(Date = as.Date(anydate(Date))) %>% 
  filter(Date > "2001-11-01")

pow_weeks <- data.frame(weekstart = as.Date(unique(pow_df$Date))) %>% 
  arrange(weekstart) %>% 
  mutate(season = ifelse(month(weekstart) < 7,year(weekstart),year(weekstart) + 1),
         weekend = ifelse(season == lead(season),ymd(lead(weekstart)) - 1,ymd(weekstart) + 6),
         weekend = as.Date(weekend,origin = "1970-01-01"),
         week_id = row_number()) %>% 
  select(weekstart,weekend,week_id)


#write_csv(pow_weeks,"~/Desktop/Player of the Week Bot/POW Weeks.csv")

pow_df_final <- merge(pow_df,pow_weeks,by.x="Date",by.y="weekstart") %>% select(Player,week_id) %>% mutate(week_id = week_id - 1)

temp_box <- raw_box %>% select(athlete_display_name,game_date)

box_final <- temp_box %>% 
  crossing(pow_weeks_week) %>% 
  filter(game_date <= weekend & game_date >= weekstart) %>% 
  select(-c(weekstart,weekend)) %>% 
  merge(raw_box,by = c("athlete_display_name","game_date")) %>% 
  select(c(1:18,24,34,35)) %>% 
  merge(winners,by = "game_id") %>% 
  mutate(won = ifelse(team_short_display_name == winner,TRUE,FALSE)) %>% 
  merge(team_points,by = c("game_id","team_short_display_name")) %>% 
  mutate(prop_points = as.numeric(pts) / points)

stats_byweek <- box_final %>% 
  mutate(pts = as.numeric(pts),
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
  group_by(athlete_display_name,week_id,team_short_display_name,athlete_id) %>% 
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
  merge(all_players,by.x = "athlete_display_name",by.y = "DISPLAY_FIRST_LAST") %>% 
  clean_names()

mvps <- "http://www.espn.com/nba/history/awards/_/id/33" %>% read_html() %>% html_table()
mvps <- (mvps[[1]][-1,]) %>% row_to_names(1)

mvps[,"MVP"] = TRUE

mvps <- mvps %>% select(PLAYER,YEAR,MVP) %>% rename(athlete_display_name = PLAYER,season = YEAR)

stats_byyear <- raw_box %>% 
  merge(winners,by = "game_id") %>% 
  mutate(won = ifelse(team_short_display_name == winner,TRUE,FALSE)) %>% 
  merge(team_points,by = c("game_id","team_short_display_name")) %>% 
  mutate(prop_points = as.numeric(pts) / points) %>% 
  mutate(pts = as.numeric(pts),
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
  group_by(athlete_display_name,season,team_short_display_name,athlete_id) %>% 
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
  filter(games >= 41) %>% 
  merge(all_players,by.x = "athlete_display_name",by.y = "DISPLAY_FIRST_LAST") %>% 
  merge(mvps,by = c("athlete_display_name","season"),all = TRUE) %>% 
  clean_names()

#PROBLEM: Non current players are not showing up in final dataframe

check_pow <- function(athlete_display_name,week_id){
  return(nrow(pow_df_final[which(pow_df_final$Player == athlete_display_name & pow_df_final$week_id == week_id),]) == 1)
}

stats_byweek$pow = NA

for(i in 1:nrow(stats_byweek)){
  stats_byweek$pow[i] = check_pow(stats_byweek$athlete_display_name[i],stats_byweek$week_id[i])
}

conferences <- data.frame(team_short_display_name = unique(stats_byweek$team_short_display_name) %>% sort(),
                          conference = c("East","East","East","East","East","West","West","East","East","East","West","West","East","West","East",
                                         "West","East","West","East","West","East","East","West","West","West","West","West","West","West","East"))

stats_byweek <- merge(stats_byweek,conferences,by = "team_short_display_name")

unique_headshots <- raw_box %>% select(athlete_id,athlete_headshot_href) %>% distinct()

#write_csv(unique_headshots,"~/Desktop/Player of the Week Bot/Unique Headshots.csv")

stats_byweek <- merge(stats_byweek,unique_headshots,by = "athlete_id") %>% select(-c(person_id,team_name)) %>% distinct()

#write_csv(stats_byweek,"~/Desktop/Player of the Week Bot/Stats by Week.csv")
#stats_byweek <- read_csv("~/Desktop/Player of the Week Bot/Stats by Week.csv")

training <- stats_byweek %>% filter(week_id <= 403)

test_model <- stats_byweek %>% filter(week_id <= 440 & week_id > 403)

log_model <- glm(pow ~ avg_prop_pts + apg + fg_percent + win_pct,data = training)

test_model$likelihood <- predict(log_model,test_model)

test_model <- test_model %>% arrange(desc(likelihood))
  
west_test <- test_model %>% filter(conference == "West")
east_test <- test_model %>% filter(conference == "East")


west_predictions <- Reduce(rbind,                                 # Top N highest values by group
                           by(west_test,
                              west_test["week_id"],
                              head,
                              n = 5))

east_predictions <- Reduce(rbind,                                 # Top N highest values by group
                           by(east_test,
                              east_test["week_id"],
                              head,
                              n = 5))

total_predictions <- rbind(west_predictions,east_predictions)
