library(shiny)
library(tidyverse)
library(googleway)
library(janitor)
library(reticulate)
library(tidygeocoder)
library(DT)
source("secret_key.R")
source("pull_incentive_data.R")
options(tidygeocoder.quiet = TRUE,
        tidygeocoder.progress = FALSE)

# setwd("/Users/isaiahwestphalen/Desktop/didactic-octo-journey")
api_key <- gway_api

# KEEP COMMENTED OUT
# path_to_python <- "/Library/Frameworks/Python.framework/Versions/3.8/bin/python3"
# use_python(path_to_python)

requests <- import_from_path("requests", path = "/Library/Frameworks/Python.framework/Versions/3.8/lib/python3.8/site-packages/")
# requests <- import("requests")
pd <- import_from_path("pandas", path = "/Library/Frameworks/Python.framework/Versions/3.8/lib/python3.8/site-packages/")
# pd <- import("pandas")
source_python("nearest_ev_stations.py")

ui <- fluidPage(
  tags$h1("Nearest Public EV Charging Stations"),
  fluidRow(
    column(
      width = 3,
      textInput(inputId = "inputAddress", label = "Input an Address:",
                value = "600 Pennsylvania Avenue NW DC")
    ),
    column(
      width = 9,
      google_mapOutput(outputId = "map")
    )
  ),
  tags$h1("Possible Incentives & Rebates"),
  fluidRow(
    column(9,
           dataTableOutput("incentive_table")
    )
  )
)

server <- function(input, output) {

  data <- reactive({

    api_res <-  nearest_ev_stations(dict(list("location" = input$inputAddress)),
                                    max_distance = 10)

    start_info <- data.frame(info_show = paste0("Input: ",
                                                input$inputAddress),
                             latitude = api_res[[2]],
                             longitude = api_res[[3]],
                             color = "green",
                             stringsAsFactors = FALSE) %>%
      mutate(q = paste0(latitude, ",", longitude)) %>%
      reverse_geocode(lat = latitude,
                      long = longitude,
                      full_results = TRUE) %>%
      clean_names() %>%
      mutate(state_q = iso3166_2_lvl4 %>% str_replace("-", ","))

    api_res %>%
      pluck(1) %>%
      mutate(
        link = paste0("https://www.google.com/maps/dir/?api=1&origin=",
                      start_info %>% pull(q),
                      "&destination=",
                      latitude,
                      ",",
                      longitude,
                      "&travelmode=driving"),
        link_text = paste(toupper(street_address), state),
        info_show = paste0(
          '<a href="',
          link,
          '" target="_blank">',
          link_text,
          '</a>'
        ),
        color = "red") %>%
      bind_rows(start_info)

  })


  output$map <- renderGoogle_map({
    google_map(data = data(), key = api_key) %>%
      add_markers(lat = "latitude",
                  lon = "longitude",
                  colour = "color",
                  # mouse_over = "station_name",
                  info_window = "info_show")
  })

  output$incentive_table <- DT::renderDataTable(
    DT::datatable({
      data() %>%
        filter(!is.na(state_q)) %>%
        pull(state_q) %>%
        get_incentives(base_url_inc,
                       jurisdiction = .,
                       user_type = "IND",
                       technology = "ELEC",
                       incentive_type = "GNT,TAX,LOANS,RBATE,TOU,EXEM,OTHER") %>%
        mutate(reference_link = get_afv_url(id),
               title = paste0("<a href='", reference_link, "' target='_blank'>",
                              title,
                              "</a>")) %>%
        select(State = state, Title = title, Description = description)
    },
    extensions = c("Scroller", "Buttons"),
    escape = FALSE,
    options = list(
      autoWidth = TRUE,
      dom = "Bfrtip",
      buttons = list(list(extend = "colvis", columns = c(3))),
      scrollX = 400,
      scrollY = 400,
      scroller = TRUE,
      columnDefs = list(
        list(width = "500px", targets = c(2)),
        list(width = "100px", targets = c(1)),
        list(visible = FALSE, targets = c(3))))
    ))
}

shinyApp(ui = ui, server = server)
