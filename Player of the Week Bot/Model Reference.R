stats_byweek <- read_csv("~/Desktop/Player of the Week Bot/Stats by Week.csv")

training <- stats_byweek %>% filter(week_id <= 403)

test_model <- stats_byweek %>% filter(week_id <= 440 & week_id > 403)

log_model <- glm(pow ~ prop_pts + fg_percent + avg_prop_pts + win_pct,data = training)