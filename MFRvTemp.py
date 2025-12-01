#cd Documents/GitHub/ThurstStand/data 

import pandas as pd
import numpy as np
import math

surface_area = math.pi * ((.05/2) ** 2)

def mass_flow_rate_calc(csv_file, velocity_ms=1):

    df = pd.read_csv(csv_file)
    
    df['density_kg_m3'] = df['Total_mass_density'] * 1000
    
    df['mass_flow_kg_s'] = df['density_kg_m3'] * velocity_ms * surface_area
    output_file = csv_file.replace('.csv', '_with_mass_flow.csv')
    df.to_csv(output_file, index=False)

    return df


def chamber_mfr_calc(torr_chamber, nozzle_exit_velocity, temp_k):
    pressure_pa = torr_chamber * 133.322
    R = 287.05  
    density_kg_m3 = 0  #pressure_pa / (R * temp_k)  4.6 * 10 ^-26
    mfr = density_kg_m3 * nozzle_exit_velocity * surface_area
    return mfr


def what_altitude(mass_flow_rate_chamber, csv_file='msise_atmospheric_data_clean_with_mass_flow.csv'):
    df = pd.read_csv(csv_file)
    closest_idx = np.argmin(np.abs(df['mass_flow_kg_s'] - mass_flow_rate_chamber))
    altitude = df.iloc[closest_idx]['Altitude_km']
    print(f"Chamber flow {mass_flow_rate_chamber:.2e} kg/s simulates {altitude:.1f} km altitude")
    return altitude


mass_flow_rate_calc(velocity_ms=8000, csv_file='msise_atmospheric_data_clean.csv')

# What Altitude is my chamber at? 1 torr, 376.41
#mass_flow_chamber = chamber_mfr_calc(1, 376.41, 352.64)
what_altitude(2.5 * 10**-4)

#We want a target altitude of 60 km by changing temperature_k

