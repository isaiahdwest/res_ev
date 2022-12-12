import requests
import pandas as pd
import secret_key

base_url = f"https://developer.nrel.gov/api/transportation-incentives-laws/v1.json?api_key={secret_key.afv_api}"

def query_incentives():
    return requests.get(base_url)
  
def get_incentives():
  res = pd.DataFrame.from_dict(query_incentives().json()["result"])
  return res  
