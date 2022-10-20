library(rtweet)
library(lubridate)
library(rvest)
library(tidyverse)


weeks <- read_csv("2023 Weeks.csv") %>% mutate(weekstart = mdy(weekstart),weekend = mdy(weekend))

#auth_as("bot_auth")

#auth_get()
#twitter_token <- get_token()

londonmapbot_token <- rtweet::rtweet_bot(
  api_key       = Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  api_secret    = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  access_token  = Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret = Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
)

source("~/Desktop/Player of the Week Bot/POW_bot_functions.R")

todays_date <- Sys.Date()

for(i in 1:nrow(weeks)){
  if(todays_date >= weeks$weekstart[i] & todays_date <= weeks$weekend[i]){
    weekstart <- weeks$weekstart[i]
    weekend <- weeks$weekend[i]
  }
}

send_tweet <- function(weekstart,weekend,conf,eow = FALSE,winner = NA){
  predictions_output(fetch_games(weekstart, weekend), conf, eow, winner)
  
  #post_tweet(status = paste0(conf,"ern Conference: ",format(as.Date(weekstart),"%b %d, %Y")," through ",format(as.Date(weekend),"%b %d, %Y")),
  #           media = "POW Table.png",
  #           media_alt_text = paste0("Table showing the top 5 favorites for the ",conf,"ern Conference player of the week, ",format(as.Date(weekstart),"%b %d, %Y")," through ",format(as.Date(weekend),"%b %d, %Y")),
  #           token = twitter_token)
}

if(todays_date == weekend){eow_selection = TRUE}else{eow_selection = FALSE}

send_tweet(weekstart,weekend,"West",eow = eow_selection)
send_tweet(weekstart,weekend,"East",eow = eow_selection)
