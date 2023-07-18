"""
This client_module.py contains Product class, which is the blueprint for create product object.
This module is imported to main.py, report_module.py and guimodule.py.
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from prettytable import PrettyTable


class Product:
    """Creates a new product with the product ID, category, name,
    specification, unit and price attributes"""
    def __init__(self, product_id, category, name,specification,unit,price):
        self.id = product_id
        self.category = category
        self.name = name
        self.spec = specification
        self.unit = unit
        self.price = price
        self.orders = []
        self.total = 0

    def product_details(self):
        """organises and returns a table of product's details"""
        table = PrettyTable()
        table.field_names = ["Product Details"]
        table.add_row([f"Product's ID: {self.id}"])
        table.add_row([f"Category: {self.category}"])
        table.add_row([f"Name: {self.name}"])
        table.add_row([f"Unit: {self.unit}"])
        table.add_row([f"Price (NZD): {self.price}"])
        table.add_row(([f"Total Amount (NZD): {self.total}"]))
        return table

    def add_order(self, order_data):
        """fetches all orders of product object from order data and returns order data
        for the product"""
        self.orders = order_data.loc[order_data['Product ID'] == self.id, :]

    def total_overturn(self):
        """calculates total overturn of product object"""
        product_data = self.orders[self.orders['Product ID'] == self.id]
        self.total = np.sum(product_data['Total'].values)

    def plot_top_5_clients(self):
        """Plot top 5 best-sellers for the product"""
        product_data = self.orders[self.orders['Product ID'] == self.id]
        # Plot Top 5 Clients buying this product:
        top5_data = product_data[["Client ID", "Quantity"]]
        top5_data = top5_data.groupby(['Client ID'], as_index=False).sum().sort_values(by='Quantity',
                                                                                       ascending=False).head()
        axes = plt.axes()
        xs = top5_data['Client ID'].tolist()
        ys = top5_data['Quantity'].tolist()
        axes.bar(xs, ys, color='orange')
        axes.set_title(f"Top 5 Best-Buyers Of Product {self.id}")
        axes.set_xlabel("Clients")
        axes.set_ylabel("Units")
        axes.grid(True)
        plt.show()

    def plot_monthly_total(self):
        """plot monthly total for the product object"""
        product_data = self.orders[self.orders['Product ID'] == self.id]
        product_data = product_data[['Date', 'Total']]
        product_data['Date'] = pd.to_datetime(product_data.Date)
        monthly_total = product_data.groupby(product_data.Date.dt.month)['Total'].sum()
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
        axes.set_title(f"Monthly Revenue of {self.id}")
        axes.set_xlabel("Month")
        axes.set_ylabel("NZD")
        axes.set_xticks(range(1, 13))
        axes.set_xticklabels(month_labels)
        axes.grid(True)
        plt.show()










