library(tidyverse)
library(reticulate)
source("secret_key.R")

pd <- import("pandas")
requests <- import("requests")

source_python("get_incentives.py")

incent <- get_incentives()

inc_type <- c(
  "Incentives",
  "State Incentives",
  "Utility/Private Incentives"
)

e_cent <- incent %>%
  mutate(technologies = ifelse(lengths(technologies) == 0,
                               "",
                               technologies)) %>%
  unnest(c("technologies")) %>%
  unnest(c("county_ids", "title")) %>%
  filter(technologies == "ELEC",
         type %in% inc_type) %>%
  left_join(. %>% pull(references) %>% bind_rows()) %>%
  select(-topics,
         -text,
         -enacted_date)

