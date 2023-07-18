import os
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import numpy as np
import matplotlib.cm as cm

# Set the directory path where the CSV files are stored
my_dir = "."

# Get a list of all CSV file names in the directory
my_files = [os.path.join(my_dir, f) for f in os.listdir(my_dir) if f.endswith('.csv')]

# Read all CSV files into a list of DataFrames using list comprehension
world_prcp_files = [pd.read_csv(f, header=0) for f in my_files]

# Concatenate all DataFrames into a single DataFrame using pd.concat()
world_prcp = pd.concat(world_prcp_files, ignore_index=True)

# Select the columns needed
world_prcp = world_prcp[['year', 'Country_Name', 'avg(Value)']]

# Rename the column names
world_prcp.columns = ['Year', 'Country_Name', 'Value']

# filt year 2022
world_prcp = world_prcp[world_prcp['Year'] == 2022]

# Data cleaning:

world_prcp = world_prcp[world_prcp['Country_Name'] != 'Sierra Leone']
world_prcp.loc[world_prcp['Country_Name'] == 'United States', 'Country_Name'] = 'United States of America'
world_prcp.loc[world_prcp['Country_Name'] == 'Solomon Islands', 'Country_Name'] = 'Solomon Is.'
world_prcp.loc[world_prcp['Country_Name'] == 'Gambia, The', 'Country_Name'] = 'Gambia'
world_prcp.loc[world_prcp['Country_Name'] == 'Falkland Islands (Islas Malvinas) [United Kingdom]', 'Country_Name'] = 'Falkland Is.'
world_prcp.loc[world_prcp['Country_Name'] == 'Western Sahara', 'Country_Name'] = 'W. Sahara'
world_prcp.loc[world_prcp['Country_Name'] == 'Congo (Brazzaville)', 'Country_Name'] = 'Congo'
world_prcp.loc[world_prcp['Country_Name'] == 'Greenland [Denmark]', 'Country_Name'] = 'Greenland'
world_prcp.loc[world_prcp['Country_Name'] == 'Korea, North', 'Country_Name'] = 'North Korea'
world_prcp.loc[world_prcp['Country_Name'] == 'Korea, South', 'Country_Name'] = 'South Korea'
world_prcp.loc[world_prcp['Country_Name'] == 'Macedonia', 'Country_Name'] = 'North Macedonia'
world_prcp.loc[world_prcp['Country_Name'] == "Cote D'Ivoire", 'Country_Name'] = "Côte d'Ivoire"

# save csv file
world_prcp.to_csv('world_prcp.csv')

world_prcp = pd.read_csv('world_prcp.csv')

world_map = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))
print(world_map['name'].unique())


# plot boxplot to detect outliers:
fig, ax = plt.subplots()
ax.boxplot(world_prcp['Value'], patch_artist=True,
           whiskerprops=dict(color='navy'), capprops=dict(color='navy'),
           medianprops=dict(color='black', linewidth=2))
ax.set_title('Boxplot Showing Average Rainfall Distribution Across the World in 2022')
ax.set_ylabel('Values')
ax.set_xlabel('Average Rainfall')
plt.show()


# Load the shapefile for the world map
world_map = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))
# Merge the shapefile with the precipitation data
merged_data = world_map.merge(world_prcp, left_on='name', right_on='Country_Name')

vmin = 0
vmax = 290
gamma = 0.45
# Create a choropleth map of the precipitation data
fig, ax = plt.subplots(figsize=(15, 8))
merged_data.plot(column='Value', cmap="Blues", legend=False, ax=ax, vmin=vmin, vmax=vmax,
                 norm=colors.PowerNorm(gamma=gamma, vmin= vmin, vmax=vmax),
                 edgecolor= 'black', linewidth=0.2)
ax.set_title('Average Rainfall in 2022')
cb = plt.colorbar(mappable=cm.ScalarMappable(norm=colors.PowerNorm(gamma=gamma, vmin= vmin, vmax=vmax), cmap="Blues"),
                  orientation='vertical', fraction=0.03, pad=0.04, aspect=30, ticks=np.linspace(vmin, vmax, 30))
cb.ax.tick_params(labelsize=6)
plt.show()
