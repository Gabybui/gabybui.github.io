import os
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

my_dir = "."

# Get a list of all CSV file names in the directory
my_files = [os.path.join(my_dir, f) for f in os.listdir(my_dir) if f.endswith('.csv')]

# Read all CSV files into a list of DataFrames using list comprehension
nz_daily_files = [pd.read_csv(f, header=0) for f in my_files]

# Concatenate all DataFrames into a single DataFrame using pd.concat()
nz_daily = pd.concat(nz_daily_files, ignore_index=True)

# Select the columns needed
nz_daily_temp = nz_daily[['Daily_ID', 'Year', 'Element', 'Value']]

# Rename the column names
nz_daily_temp.columns = ['station_code', 'year', 'type', 'value']

# save csv file
nz_daily_temp.to_csv('nz_daily_temp.csv')

nz_daily_temp = pd.read_csv('nz_daily_temp.csv')

# Group by station_code, year, and type, and compute the mean of value
nz_daily_temp = nz_daily_temp.groupby(['station_code', 'year', 'type']).mean().reset_index()

# pivot the table
nz_daily_temp = nz_daily_temp.pivot(index=['station_code', 'year'], columns='type', values='value').reset_index()
stations_list = nz_daily_temp.station_code.unique()
year_series = pd.Series(range(1940, 2024))
year_df = pd.DataFrame({'year': year_series})
station_dfs = []
for station in stations_list:
    station_df = nz_daily_temp[nz_daily_temp['station_code'] == station]
    station_df = year_df.merge(station_df, on='year', how='left').reset_index()
    station_df['station_code'] = station_df['station_code'].fillna(station)
    station_dfs.append(station_df)

nz_daily_temp = pd.concat(station_dfs, axis=0, ignore_index=True)
stations = nz_daily_temp['station_code'].unique()

nz_daily_temp_2 = pd.melt(nz_daily_temp, id_vars=['station_code', 'year'], value_vars=['TMIN', 'TMAX'], var_name='type', value_name='value')

# create line chart with gaps for missing values
fig, axs = plt.subplots(4, 4, figsize=(16, 16), sharex= 'all', sharey='all')

for i, station in enumerate(stations):
    row = i // 4  # calculate row index based on subplot index
    col = i % 4  # calculate column index based on subplot index
    data = nz_daily_temp[nz_daily_temp['station_code'] == station]  # filter data for current station
    axs[row, col].plot(data['year'], data['TMIN'], color='#18c6cc', label='TMIN')
    axs[row, col].plot(data['year'], data['TMAX'], color='#f0983a', label='TMAX')
    axs[row, col].set(ylabel='Temperature',
                      title='Station {}'.format(station))


# create a single legend outside the subplots
handles, labels = axs[0, 0].get_legend_handles_labels()
fig.legend(handles, labels, loc='lower center', ncol=2)

plt.tight_layout()
# axs.legend() # automatically adjust spacing between subplots
plt.show()

# Define custom color palette
my_palette = {'TMAX': '#f0983a', 'TMIN': '#18c6cc'}

# Create lineplot with custom color palette
p = sns.lineplot(x='year', y='value', hue='type', data=nz_daily_temp_2, palette=my_palette)

# Set the plot title and axis labels
p.set(title='TMAX/TMIN for all stations in New Zealand', xlabel='Year', ylabel='Values(tenths of degree Celsius)')

# Show the plot
plt.show()
