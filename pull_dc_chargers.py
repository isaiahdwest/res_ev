#!/usr/bin/env python

import pandas as pd
import requests
from nearest_ev_stations import nearest_ev_stations

dc_charge = nearest_ev_stations()[0]

dc_charge.to_csv("dc_charge.csv")
