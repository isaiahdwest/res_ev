#!/usr/bin/env python

import pandas as pd
from datetime import datetime
from nearest_ev_stations import nearest_ev_stations

now = datetime.now()

current_time = now.strftime("%H:%M:%S")

dc_charge = nearest_ev_stations()[0]

dc_charge["time_update"] = current_time

dc_charge.to_csv("dc_charge.csv")
