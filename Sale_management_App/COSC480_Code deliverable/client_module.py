"""
This client_module.py contains Client class, which is the blueprint for creating client object.
This module is imported to main.py, report_module.py and guimodule.py.
"""
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from prettytable import PrettyTable

class Client:
    """define client class"""
    def __init__(self, client_id, name, phone, province):
        """Creates a new client with the client ID, name, phone, and province attributes"""
        self.id = client_id
        self.name = name
        self.phone = phone
        self.province = province
        self.orders = []
        self.total = 0

    def client_details(self):
        """organises and return a table of client's details"""
        table = PrettyTable()
        table.field_names = ["Client Details"]
        table.add_row([f"Client's ID: {self.id}"])
        table.add_row([f"Name: {self.name}"])
        table.add_row([f"Phone number: {self.phone}"])
        table.add_row([f"Province: {self.province}"])
        table.add_row([f"Total Revenue (NZD): {self.total}"])
        return table

    def add_order(self, order_data):
        """fetches all orders of client object from order data and returns
        client's orders data"""
        self.orders = order_data.loc[order_data['Client ID'] == self.id, :]

    def total_overturn(self):
        """calculates total overturn of a particular client object"""
        client_data = self.orders[self.orders['Client ID'] == self.id]
        self.total = np.sum(client_data['Total'].values)

    def plot_top_5_products(self):
        """Plots bar chart from client's top 5 best-selling products"""
        client_data = self.orders[self.orders['Client ID'] == self.id]
        # Plot Top 5 Clients buying this product:
        top5_data = client_data[["Product ID", "Quantity"]]
        top5_data = top5_data.groupby(['Product ID'], as_index=False).sum().sort_values(by='Quantity',
                                                                                        ascending=False).head()
        axes = plt.axes()
        xs = top5_data['Product ID'].tolist()
        ys = top5_data['Quantity'].tolist()
        axes.bar(xs, ys, color='orange')
        axes.set_title(f"Top 5 Best-Selling Products Of Client {self.id}")
        axes.set_xlabel("Products")
        axes.set_ylabel("Units")
        axes.grid(True)
        plt.show()

    def plot_monthly_total(self):
        """plots monthly total for client overturn"""
        client_data = self.orders[self.orders['Client ID'] == self.id]
        client_data = client_data[['Date', 'Total']]
        client_data['Date'] = pd.to_datetime(client_data.Date)
        monthly_total = client_data.groupby(client_data.Date.dt.month, )['Total'].sum()
        month_labels = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'July', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
        month_empty = {1: 0, 2: 0, 3: 0, 4: 0, 5: 0, 6: 0, 7: 0, 8: 0, 9: 0, 10: 0, 11: 0, 12: 0}
        month_values = monthly_total.to_dict()
        month_dict = {**month_empty, **month_values}
        months = list(month_dict.keys())
        totals = list(month_dict.values())
        # plot bar chart:
        axes = plt.axes()
        xs = months
        ys = totals
        axes.bar(xs, ys, color='green')
        axes.set_title(f"Monthly Revenues of Client {self.id}")
        axes.set_xlabel("Month")
        axes.set_ylabel("NZD")
        axes.set_xticks(range(1,13))
        axes.set_xticklabels(month_labels)
        axes.grid(True)
        plt.show()


