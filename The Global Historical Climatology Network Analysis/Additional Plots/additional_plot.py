import pandas as pd
import matplotlib.pyplot as plt

def data_size_change_plot():
    """Plot Data size change - Q1 Processing"""
    # Load the CSV file into a Pandas data frame
    df = pd.read_csv('data_size_change.csv')

    # Plot the data using matplotlib
    plt.plot(df.Year, df.Compressed_Size, label='Compressed Size')
    plt.plot(df.Year, df.Actual_Size, label='Actual Size')
    plt.xlabel('Year')
    plt.ylabel('Mb')
    plt.title('From small files to big data: The evolution of file sizes, 1750-2023')
    plt.grid(True)
    plt.legend()
    plt.show()

def element_type_plot():
    """Plot top 10 most popular elements type"""
    # read data from CSV file into a pandas DataFrame
    data = pd.read_csv('element_types.csv')

    # extract element types and station counts from DataFrame
    elements = data['Element']
    station_counts = data['Number_Stations_Used']

    # define core elements
    core_elements = ["PRCP", "SNOW", "SNWD", "TMAX", "TMIN"]

    # create list of colors for each bar
    colors = ['#d45f5f' if e in core_elements else '#4287f5' for e in elements]

    # create bar chart
    plt.grid(axis='y', alpha=0.5, linewidth=0.5, zorder=0)
    plt.bar(elements, station_counts, color=colors)
    # add text labels for station counts above each bar
    for i, count in enumerate(station_counts):
        plt.text(i, count + 5, str(count), ha='center')
    # set chart title and axis labels
    plt.title('Top 10 most used elements')
    plt.xlabel('Element types')
    plt.ylabel('Number of stations')

    # create legend for core and other element types
    core_patch = plt.Rectangle((0, 0), 1, 1, fc='#d45f5f', edgecolor='none')
    other_patch = plt.Rectangle((0, 0), 1, 1, fc='#4287f5', edgecolor='none')
    plt.legend([core_patch, other_patch], ['Core element', 'Other element'], loc='upper right')

    # display the chart
    plt.show()

def total_stations_per_country_plot():
    """Plot top 10 countries with the biggest number of stations"""
    # Read the data
    df2 = pd.read_csv('total_stations_per_country.csv')
    Country_Name = df2['Country_Name']
    Total_Stations = df2['Total_Stations']
    # Create the bar chart
    plt.bar(Country_Name, Total_Stations, color='#4287f5')
    # add text labels for station counts above each bar
    for i, num in enumerate(Total_Stations):
        plt.text(i, num + 5, str(num), ha='center')
    # Add labels and title
    plt.xlabel('Country')
    plt.ylabel('Number of Stations')
    plt.title('Top 10 Countries with the Highest Number of Stations')

    # Rotate x-tick labels
    plt.xticks(rotation=45)

    # Add grid
    plt.grid(axis='y', alpha=0.7)

    # Show the plot
    plt.show()

def total_stations_per_state_plot():
    """top 10 state with biggest number of stations"""
    # Read the data
    df3 = pd.read_csv('total_stations_per_state.csv')
    StateName = df3['StateName']
    Total_Stations = df3['Total_Stations']
    # Create the bar chart
    plt.bar(StateName, Total_Stations, color='#4287f5')
    # add text labels for station counts above each bar
    for i, num in enumerate(Total_Stations):
        plt.text(i, num + 5, str(num), ha='center')
    # Add labels and title
    plt.xlabel('State')
    plt.ylabel('Number of Stations')
    plt.title('Top 10 States with the Highest Number of Stations')

    # Rotate x-tick labels
    plt.xticks(rotation=45)

    # Add grid
    plt.grid(axis='y', alpha=0.7)

    # Show the plot
    plt.show()

def southern_hemisphere_plot():
    """Plot percentage of southern hemisphere total stations"""

    # Define the data
    total_stations = 124247
    southern_stations = 25337

    # Calculate the proportions
    southern_prop = southern_stations / total_stations
    northern_prop = 1 - southern_prop

    # Create the pie chart
    labels = ['Southern Hemisphere', 'Northern Hemisphere']
    sizes = [southern_prop, northern_prop]
    colors = ['#d45f5f', '#4287f5']
    plt.pie(sizes, labels=labels, colors=colors, autopct='%1.1f%%', startangle=90)

    # Add title
    plt.title('Proportion of Total Stations in the Southern Hemisphere')

    # Show the plot
    plt.show()

data_size_change_plot()
element_type_plot()
total_stations_per_country_plot()
total_stations_per_state_plot()
southern_hemisphere_plot()






