library(httr)
library(glue)
library(jsonlite)
library(tidyverse)
# source("secret_key.R")

httr::set_config(config(ssl_verifypeer = 0L))

message("loaded libraries")
afv_api <- Sys.getenv("AFV_API")

base_url_inc <-  paste0("https://developer.nrel.gov/api/transportation-incentives-laws/v1.json?api_key=",
                afv_api)
message("apikeys")
format_inc_params <- function(url, ...) {

  param_lst <-  list(...)

  param_str <- ""

  for (i in names(param_lst)) {

    param_str <- paste0(param_str, "&", i, "=", param_lst[[i]])

  }

  paste0(url, param_str)

}

query_inc_api <- function(url, ...) {

  api_url <- format_inc_params(url, ...)

  GET(api_url)
}

get_incentives <- function(url = base_url_inc, ind = TRUE, ...) {
  # print(afv_api) %>%
  # print("get_incentives")
  # print("query_inc_api")
  if (ind) {
    res <- query_inc_api(url, user_type = "IND", ...)
  } else {
    res <- query_inc_api(url, ...)
  }

  json_res <- res$content %>% rawToChar() %>% fromJSON()

  json_res[["result"]] %>%
    tibble() %>%
    unnest(references) %>%
    unnest(technologies) %>%
    filter(technologies == "ELEC") %>%
    mutate(reference = paste0("<a href = '", url, "' target = '_blank'>",
                              description, "</a>")) %>%
    select(-c("description", "url")) %>%
    group_by(id) %>%
    mutate(reference_num = paste0("reference_", row_number())) %>%
    ungroup() %>%
    pivot_wider(names_from = reference_num,
                values_from = reference) %>%
    select(id, state, title, description = plaintext,
           technologies, starts_with("reference_"))
}

get_afv_url <- function(id) {
  paste0("https://afdc.energy.gov/laws/", id)
}
