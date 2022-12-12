import requests
import pandas as pd
import secret_key

# Base API URL
base_url = f"https://developer.nrel.gov/api/alt-fuel-stations/v1/nearest.json?api_key={secret_key.afv_api}"

# format_params()
# - Takes a dictionary of AFV API commands and returns a single string to be added
# - to base url.
# x - a dictionary of param key value pairs. Default is an empty dictionary.
# Example: format_params({"hi":"there"})
# '&hi=there'
def format_params(x = {}):
    params = []
    for i in x.keys():
        params.append("&"+"=".join([i, x[i]]))
    return "".join(params)

# query_api()
# - Takes base url and api parameters to query AFV Fuel Station Locator.
# url - the AFV Fuel locator base url
# api_params - params to use to query the api, default parameter is {"location":"20500"}
# Example: query_api(api_params = {"location":"20500"})
def query_api(url = base_url, api_params = {"location":"20500"}):
    param_str = format_params(api_params)
    query_url = url+param_str 
    return requests.get(query_url)

# nearest_ev_stations()
# - Returns data frame of nearest EV fuel stations
def nearest_ev_stations(params = {"location":"20500"}, max_distance = 5):
    params["fuel_type"] = "ELEC"
    params["access_code"] = "public"
    res = query_api(api_params = params)
    fuel_stations = pd.DataFrame.from_dict(res.json()["fuel_stations"])
    relevant_fields = ["street_address", "city", "state", "zip", "access_code", "access_days_time",
                       "cards_accepted", "fuel_type_code",
                       "status_code", "station_name", 
                       "facility_type", "latitude",
                       "longitude", "ev_connector_types",
                       "ev_dc_fast_num", "ev_level2_evse_num", 
                       "ev_pricing", "distance"]
    return fuel_stations[fuel_stations.distance <= max_distance][relevant_fields], res.json()["latitude"], res.json()["longitude"]
