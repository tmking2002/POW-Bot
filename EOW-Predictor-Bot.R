install.packages("lubridate")
install.packages("rvest")
install.packages("tidyverse")

library(rtweet)
library(lubridate)
library(rvest)
library(tidyverse)

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

source("POW_bot_functions.R")

todays_date <- Sys.Date() - 1

for(i in 1:nrow(weeks)){
  if(todays_date >= weeks$weekstart[i] & todays_date <= weeks$weekend[i]){
    weekstart <- weeks$weekstart[i]
    weekend <- weeks$weekend[i]
  }
}

send_tweet <- function(weekstart,weekend,conf,eow = FALSE){
  
  winner = pow_df %>% filter(Conference == conf) %>% select(Player) %>%  as.character()
  
  predictions_output(fetch_games(weekstart, weekend), conf, eow, winner)
  
  status = paste0(conf,"ern Conference: Final Predictions")
  
  #Use for manual posting
  #token = twitter_token
  
  #Use for actual bot
  token = POWbot_token
  
  post_tweet(status = status,
             media = "POW Table.png",
             media_alt_text = paste0("Table showing the top 5 favorites for the ",conf,"ern Conference player of the week, ",format(as.Date(weekstart),"%b %d, %Y")," through ",format(as.Date(weekend),"%b %d, %Y")),
             token = token)
}

send_tweet(weekstart,weekend,"West",eow = TRUE)
send_tweet(weekstart,weekend,"East",eow = TRUE)