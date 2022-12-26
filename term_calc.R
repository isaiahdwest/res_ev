
options(stringsAsFactors = FALSE)

input <- data.frame(
  vin = c("akjsfwbdb"),
  make = c("honda"),
  model = "odyssey",
  model_year = 2019,
  purchase_year = 2020,
  initial_cost = 50000,
  annual_vmt = 14000,
  efficiency  = 26,
  fuel_cost = 3,
  maint_rate = 0.09,
  co2_rate = 8.8, # kg / gal
  term = 12,
  type = "purchase",
)

input <- tribble(
  ~vin,~make,~model,~model_year,~purchase_year,~initial_cost,~depreciation_rate,~annual_vmt,~efficiency,~fuel_cost,~maint_rate,~co2_rate,~term,~type,
  "V59HLGJF","SUBARU","FORESTER",2019,2020,50000,.1,14000,32,3.418,0.05,8.8,12,"PURCHASE",
  "HFG88068","HONDA","ODYSSEY",2018,2020,40000,.1,15000,26,3.418,0.10,8.8,12,"PURCHASE"
)

# User input repl year
# calculated based on age / term
# right now
# adjust vehicle price depreciation rate ---

# compare new vehicle of existing make model OR of a different vehicle

ann_fuel <- function(vmt, eff) {
  vmt / eff
}

add_annual_fuel <- function(.data) {

  if ("annual_fuel" %in% names(.data)) {
    .data
  } else {
    .data %>%
      mutate(annual_fuel = ann_fuel(annual_vmt, efficiency))
  }

}

seq_years <- function(.data) {

  if ("years" %in% names(.data)) {
    .data
  } else {
    .data %>%
      group_by(vin) %>%
      mutate(years = paste0("year_", seq_len(term)) %>% list()) %>%
      unnest(years) %>%
      ungroup()
  }

}

residual

cost_seq <- function(.data) {

  .data %>%
    add_annual_fuel() %>%
    seq_years() %>%
    group_by(vin) %>%
    mutate(
      residual_value = case_when(
        type == "PURCHASE" ~ initial_cost * (1- depreciation_rate) ^ row_number(),
        TRUE ~ 0
      ),
      initial_cost = case_when(
        type == "PURCHASE" & years == "year_1" ~ initial_cost,
        type == "LEASE" ~ initial_cost,
        TRUE ~ 0
      ),
      cost_maint = annual_vmt * maint_rate,
      cost_fuel = fuel_cost * annual_fuel
    ) %>%
    ungroup()

}

ghg_seq <- function(.data) {

  if ("annual_co2" %in% names(.data)) {
    .data
  } else {

    .data %>%
      add_annual_fuel() %>%
      seq_years() %>%
      group_by(vin) %>%
      mutate(annual_co2 = co2_rate * annual_fuel / 1000) %>%
      ungroup()
  }

}

get_repl_year <- function(.data) {
  if ("repl_year" %in% names(.data)) {
    .data
  } else {
    .data %>%
      group_by(vin) %>%
      mutate(
        repl_year = model_year + term
      ) %>%
      ungroup()
  }
}

add_years <- function(.data) {
  .data %>%
    get_repl_year() %>%
    group_by(vin) %>%
    mutate(year = purchase_year + row_number() - 1) %>%
    ungroup()
}

get_tco <- function(.data) {

  if ("tco_year" %in% names(.data)) {
    .data
  } else {

    .data %>%
      group_by(vin) %>%
      mutate(tco_year = case_when(
        years == paste0("year_", term) & type == "PURCHASE" ~ initial_cost +  cost_maint + cost_fuel - residual_value,
        TRUE ~ initial_cost +  cost_maint + cost_fuel
      )) %>%
      ungroup()
  }
}

get_cum_tco <- function(.data) {

  .data %>%
    get_tco() %>%
    group_by(vin) %>%
    mutate(tco = cumsum(tco_year)) %>%
    ungroup()

}

get_cum_ghg <- function(.data) {

  .data %>%
    ghg_seq() %>%
    group_by(vin) %>%
    mutate(ghg_cum = cumsum(annual_co2)) %>%
    ungroup()

}

full_term <- function(.data) {
  .data %>%
    cost_seq() %>%
    ghg_seq() %>%
    add_years() %>%
    get_cum_tco() %>%
    get_cum_tco()

}

input %>%
  full_term() %>%
  ggplot(aes(x = year, y = tco, fill = vin)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(tco, 0)), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4)

