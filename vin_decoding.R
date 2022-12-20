library(httr)
library(glue)
library(jsonlite)
library(tidyverse)
library(lubridate)
source("secret_key.R")

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


# make / model / year?
# make -> first model recent year
# make, model -> most recent year
# make model year
# drop down of makes]
# drop down of models/years

get_models <- function(make, year = NULL) {
  if (is.null(year)) {
    url <- paste0("https://vpic.nhtsa.dot.gov/api/vehicles/GetModelsForMake/",
                  make,
                  "?format=json")
  } else {
    url <- paste0("https://vpic.nhtsa.dot.gov/api/vehicles/GetModelsForMakeYear/make/",
                  make,
                  "/modelyear/",
                  year,
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
        )
      ) %>%
      filter(grepl("USA", Country)) %>%
      arrange(
        priority,
        desc(CreatedOn)
      ) %>%
      filter(row_number() == 1)
  } else {
    res
  }
}

