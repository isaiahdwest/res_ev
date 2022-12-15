library(tidygeocoder)
source("00_functions.R")

options(tidygeocoder.quiet = TRUE)

abc <- get_incentives(base_url_inc,
                      jurisdiction = "US,DC",
                      user_type = "IND",
                      technology = "ELEC",
                      law_type = "STATEINC,UPINC,INC")


lol <- nearest_ev_stations(d_lst(location = "Boston MA"))

start_info <- data.frame(info_show = paste0("Input: "),
                         latitude = lol[[2]],
                         longitude = lol[[3]],
                         color = "green",
                         stringsAsFactors = FALSE) %>%
  mutate(q = paste0(latitude, ",", longitude))

bbb <- start_info %>%
  reverse_geocode(lat = latitude,
                  long = longitude,
                  full_results = TRUE) %>%
  clean_names() %>%
  mutate(state_q = iso3166_2_lvl4 %>% str_replace("-", ",")) %>%
  pull(state_q) %>%
  get_incentives(base_url_inc,
                 jurisdiction = .,
                 user_type = "IND",
                 technology = "ELEC",
                 law_type = "STATEINC,UPINC,INC")

lol[[1]] %>% glimpse
