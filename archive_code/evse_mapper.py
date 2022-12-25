import gmplot
from nearest_ev_stations import nearest_ev_stations
import pandas as pd
import secret_key

def map_evse(location = "Washington DC", distance = 10, file_name = "evse_map"):
    res, lat, long = nearest_ev_stations(params={"location":location}, max_distance=distance)

    latitude_list = res['latitude'].values
    longitude_list = res['longitude'].values
  
    gmap3 = gmplot.GoogleMapPlotter(lat,
                                long, 16, apikey=secret_key.gmap_api)
  
    gmap3.scatter(latitude_list, longitude_list,
                              size = 40, marker = True,
             color = "red")
  
    gmap3.draw(f"{file_name}.html")
    return gmap3