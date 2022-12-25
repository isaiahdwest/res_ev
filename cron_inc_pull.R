message("start cron")
source("pull_incentive_data.R")

get_incentives() %>%
  mutate(last_update = Sys.time()) %>%
  write_csv("incentives.csv")
