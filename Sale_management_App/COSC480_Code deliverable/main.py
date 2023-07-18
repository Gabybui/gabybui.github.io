"""
Progame: Sales Management
Author: Giang Bui
Last Edit: 20/10/2022
Email: gabybui93@gmail.com

Description: The programme is designed for area sales manager in Ninh Thuan and Khanh Hoa provinces
Vietnam, which helps area sales manager estimate the values of a specific client or keep track
with how much a particular product is performming. Also, The programme facilitates doing annual
year report, and sort out the V.I.P clients that meet a particular target, so that can have a
good plan for next year.

Requirements: you need 4 csv files: Client data, Product data, Order data and monthly target, and
place it in the project folder (same with python main file).

You can open the program using Pycharm (Anaconda) or Wing IDE 101 or any Python IDE.
"""
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from prettytable import PrettyTable
from client_module import Client
from product_module import Product
from report_module import Report
from guimodule import SalesManagementGui
import tkinter as tk


def read_file(filename):
    """reads csv file and returns a pandas dataframe"""
    return pd.read_csv(filename)


def read_client(filename):
    """Reads client csv file and returns a list of client objects"""
    infile = open(filename)
    content = infile.read().splitlines()
    infile.close()
    lines = content[1:]
    client_list = []
    for line in lines:
        client_id,store_name,owner,phone,email,province = line.split(",")
        # create a list of client objects belong to Client class:
        client = Client(client_id, store_name, phone, province)
        client_list.append(client)
    return client_list


def read_product(filename):
    """reads product csv file and returns a list of product objects"""
    infile = open(filename)
    content = infile.read().splitlines()
    infile.close()
    lines = content[1:]
    product_list = []
    for line in lines:
        product_id, category, name,specification,unit,price = line.split(",")
        # create a list of product objects belong to Product class:
        product = Product(product_id, category, name, specification, unit, float(price))
        product_list.append(product)
    return product_list


def return_object(object_list, id):
    """takes an object list (client/product objects) and object ID, then returns an object"""
    for object in object_list:
        if object.id == id:
            return object


def main():
    """main function"""
    # Read csv files and create lists of client/product objects:
    client_list = read_client("client_data.csv")
    product_list = read_product("product_data.csv")
    # turn order data csv file to pandas dataframe
    order_data = read_file("order_data.csv")
    # read Monthly Target csv file and convert to pandas dataframe
    monthly_target = read_file("monthly_target.csv")
    # create report object
    report = Report(order_data, product_list, client_list, monthly_target)

    # Add order data into product object and calculate the total overturns of each product object:
    for product in product_list:
        product.add_order(order_data)
        product.total_overturn()

    # Add order data into client object and calculate the total overturns of each client object:
    for client in client_list:
        client.add_order(order_data)
        client.total_overturn()

    # Create window tkinter object

    window = tk.Tk()
    window.title("Student: Giang Bui - ID: 37306207")
    salesgui = SalesManagementGui(window, client_list, product_list, order_data, report)
    window.mainloop()


main()
# product_1 = return_object(product_list, "NPK012")
# print(product_1.total)
# product_1.print_product_details()
# product_1.plot_top_5_clients()
# product_1.plot_monthly_total()
# client_1 = return_object(client_list, "NT024")
# client_1.print_client_details()
# client_1.plot_top_5_products()
# client_1.plot_monthly_total()

# report.category_revenue()
# report.plot_category_revenue()
# report.province_revenue()
# report.plot_province_revenue()
# report.plot_monthly_revenue()
# report.plot_top_10_clients()
# report.plot_top_10_Products()
# report.print_diamond_clients(300000000)