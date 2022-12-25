library(httr)
library(glue)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(rvest)
# source("secret_key.R")

base_vpic_url <- "https://vpic.nhtsa.dot.gov/api/vehicles/DecodeVin/"


format_vpic_query <- function(url = base_vpic_url, vin, ...) {

  vin_url <- paste0(url, vin, "?format=json")

  param_lst <-  list(...)

  param_str <- ""

  for (i in names(param_lst)) {

    param_str <- paste0(param_str, "&", i, "=", param_lst[[i]])

  }

  paste0(vin_url, param_str)

}

query_vpic_api <- function(url = base_vpic_url, vin, ...) {

  vpic_query <- format_vpic_query(vin = vin, ...)

  GET(vpic_query)

}

get_vin_info <- function(vin, ...) {

  res <- query_vpic_api(vin = vin, ...)

  res %>%
    pluck("content") %>%
    rawToChar() %>%
    fromJSON() %>%
    pluck("Results") %>%
    select(Variable, Value) %>%
    pivot_wider(names_from = Variable, values_from = Value)
}


# make / model / modelyear?
# make -> first model recent modelyear
# make, model -> most recent modelyear
# make model modelyear
# drop down of makes]
# drop down of models/modelyears

get_models <- function(make, modelyear = NULL) {
  if (is.null(modelyear)) {
    url <- paste0("https://vpic.nhtsa.dot.gov/api/vehicles/GetModelsForMake/",
                  make,
                  "?format=json")
  } else {
    url <- paste0("https://vpic.nhtsa.dot.gov/api/vehicles/GetModelsForMakeYear/make/",
                  make,
                  "/modelyear/",
                  modelyear,
                  "?format=json")
  }

  GET(url) %>%
    pluck("content") %>%
    rawToChar() %>%
    fromJSON() %>%
    pluck("Results")
}

get_wmi <- function(make, single_wmi = TRUE) {
  url <- paste0("https://vpic.nhtsa.dot.gov/api/",
                "vehicles/GetWMIsForManufacturer/",
                make,
                "?format=json")

  res <- GET(url) %>%
    pluck("content") %>%
    rawToChar() %>%
    fromJSON() %>%
    pluck("Results")%>%
    mutate(
      CreatedOn = as_datetime(CreatedOn),
      UpdatedOn = as_datetime(UpdatedOn)
    ) %>%
    arrange(desc(CreatedOn))

  if (single_wmi) {
    res %>%
      mutate(
        priority = case_when(
          VehicleType %in% c("Passenger Car",
                             "Multipurpose Passenger Vehicle (MPV)") ~ 1,
          VehicleType %in% c("Low Speed Vehicle (LSV)",
                             "Incomplete Vehicle",
                             "Truck") ~ 2,
          TRUE ~ 3
        ),
        country_prior = ifelse(grepl("USA", Country), 1, 2)
      ) %>%
      arrange(
        country_prior,
        priority,
        desc(CreatedOn)
      ) %>%
      filter(row_number() == 1)
  } else {
    res
  }
}

# HUH
# get_vins
# httr::set_config(httr::config(ssl_verifypeer = FALSE))
# res <- GET("https://epicvin.com/vin-lookup/honda/odyssey/2019/")
#
# a_text <- content(res) %>%
#   html_nodes("a") %>%
#   html_text() %>%
#   str_extract_all("[A-Z0-9]+")
#
# vin_strs <- a_text[lengths(a_text) == 1] %>% unlist()

# vin_strs[vin_strs %>% nchar() >= 10]


get_vins <- function(make, model, modelyear = year(Sys.Date())) {

  url <- paste0("https://epicvin.com/vin-lookup/",
                make,
                "/",
                model,
                "/",
                modelyear,
                "/")

  # print(url)

  res <- GET(url)

  a_text <- content(res) %>%
    html_nodes("a") %>%
    html_text() %>%
    str_extract_all("[A-Z0-9]+")

  vin_strs <- a_text[lengths(a_text) == 1] %>% unlist()

  f_url <- paste0(url, vin_strs[vin_strs %>% nchar() >= 10])[1]

  res_02 <- GET(f_url)

  content(res_02) %>%
    html_nodes("a")

}

make_model_q <- function(make, model, modelyear = year(Sys.Date())) {
  get_vins(make, model, modelyear) %>%
    pluck(2) %>%
    get_vin_info(modelyear = modelyear)
}

