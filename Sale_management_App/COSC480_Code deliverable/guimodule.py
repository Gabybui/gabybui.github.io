"""
This guimodule.py contains SalesManagementGui class, which is the blueprint for creating gui object.
This module is imported to main.py.
"""
import tkinter as tk
from tkinter.ttk import *
from tkinter import Frame
from art import art_line, welcome

GOOD_THRESHOLD = 30000

class SalesManagementGui:
    """define Sales Management GUI class"""

    def __init__(self, window, client_list, product_list, order_data, report):
        """Setup GUI on given window and get the necessary data"""
        self.client_list = client_list
        self.product_list = product_list
        self.report = report
        self.order_data = order_data

        # ================= CREATE TKINTER FRAMES ================================================

        # Create 2 Frame: header Frame and main Frame
        self.header = Frame(window, bg='#6cba9f', width=800, height=50)
        self.header.grid(row=0, column=0, columnspan=2)

        self.body = Frame(window, bg='white', width=800, height=600)
        self.body.grid(row=1, column=0, rowspan=2)

        # Setup header frame
        self.header.grid_propagate(0)
        self.header.grid_rowconfigure(0, weight=1)
        self.header.grid_columnconfigure(1, weight=1)
        self.label_header_text_2 = tk.Label(self.header, \
                                            text='SALES MANAGEMENT', \
                                            bg='#6cba9f', fg='white', \
                                            font=("Roboto", 25))
        self.label_header_text_2.grid(row=0, column=1)

        # Setup main frame
        self.body.grid_propagate(0)
        self.body.grid_rowconfigure(1, weight=1)
        self.body.grid_columnconfigure(1, weight=1)

        # Create 2 sub frames inside the main frame: top and bottom main frame
        # top main frame
        self.top_body = Frame(self.body, bg='white', \
                              highlightbackground="#5f9cad", \
                              highlightthickness=2, width=800, height=300)
        self.top_body.grid(row=0)
        self.top_body.grid_propagate(0)
        self.top_body.grid_rowconfigure(10, weight=1)
        self.top_body.grid_columnconfigure(8, weight=1)

        # bottom main frame
        self.bottom_body = Frame(self.body, bg='white', \
                                 width=800, height=480)
        self.bottom_body.grid(row=5, sticky='n')
        self.bottom_body.grid_propagate(0)
        self.bottom_body.grid_rowconfigure(10, weight=1)
        self.bottom_body.grid_columnconfigure(8, weight=1)

        # ========== set widget for displaying result=========

        # self.result_message = tk.Label(self.bottom_body, text='', bg='white')
        # self.result_message.grid(row=3, column=0)
        self.text_widget = tk.Text(self.bottom_body, height=30, width=100, fg="#a30b1b"
                                   )
        self.text_widget.pack()
        self.text_widget.insert('1.0', welcome)


        # ========== set widgets for clients report===========

        self.client_title = tk.Label(self.top_body, text="CLIENT INSPECTION",
                                     pady=4, font=('Roboto', 15, 'bold'))
        self.client_title.grid(row=0, column=0, columnspan=3)
        self.client_lab = tk.Label(self.top_body, text="Input ID:")
        self.client_lab.grid(row=1, column=0)
        self.client_entry = tk.Entry(self.top_body, width=15)
        self.client_entry.grid(row=1, column=1)
        self.client_btn = tk.Button(self.top_body, text='Inspect',
                                    command=self.print_client_info)
        self.client_btn.grid(row=1, column=2)

        self.client_top5_btn = tk.Button(self.top_body, text='Plot Top 5 Products',
                                         command=self.plot_top5_product)
        self.client_top5_btn.grid(row=2, column=1)
        self.client_monthly_btn = tk.Button(self.top_body, text='Plot Monthly Total',
                                            command=self.plot_client_monthly_total)
        self.client_monthly_btn.grid(row=3, column=1)

        # ========== set widgets for product report=========================

        self.product_title = tk.Label(self.top_body, text="PRODUCT INSPECTION",
                                      pady=4, font=('Roboto', 15, 'bold'))
        self.product_title.grid(row=5, column=0, columnspan=3)
        self.product_lab = tk.Label(self.top_body, text="Input ID:")
        self.product_lab.grid(row=6, column=0)
        self.product_entry = tk.Entry(self.top_body, width=15)
        self.product_entry.grid(row=6, column=1)
        self.product_btn = tk.Button(self.top_body, text='Inspect',
                                     command=self.print_product_info)
        self.product_btn.grid(row=6, column=2)

        self.product_top5_btn = tk.Button(self.top_body, text='Plot Top 5 Clients',
                                          command=self.plot_top5_clients)
        self.product_top5_btn.grid(row=8, column=1)
        self.product_monthly_btn = tk.Button(self.top_body, text='Plot Monthly Total',
                                             command=self.plot_product_monthly_total)
        self.product_monthly_btn.grid(row=9, column=1)

        # ========== set widgets for general report=========================

        self.storage_variable = tk.StringVar()
        self.report_title = tk.Label(self.top_body, text="SALES REPORT",
                                     font=('Roboto', 15, 'bold'))
        self.report_title.grid(row=0, column=8)
        self.report_opt1 = tk.Radiobutton(self.top_body,
                                          text="  Total revenue in categories         ",
                                          variable=self.storage_variable,
                                          value="category"
                                          )
        self.report_opt1.grid(row=1, column=8)
        self.report_opt2 = tk.Radiobutton(self.top_body,
                                          text="  Total revenue in provinces          ",
                                          variable=self.storage_variable,
                                          value="province"
                                          )
        self.report_opt2.grid(row=2, column=8)
        self.report_opt3 = tk.Radiobutton(self.top_body,
                                          text="  Monthly total revenue                 ",
                                          variable=self.storage_variable,
                                          value="month"
                                          )
        self.report_opt3.grid(row=3, column=8)
        self.report_opt4 = tk.Radiobutton(self.top_body,
                                          text="  Top 10 Best selling products      ",
                                          variable=self.storage_variable,
                                          value="product10"
                                          )
        self.report_opt4.grid(row=4, column=8)
        self.report_opt5 = tk.Radiobutton(self.top_body,
                                          text="  Top 10 Best Clients of the year   ",
                                          variable= self.storage_variable,
                                          value="client10"
                                          )
        self.report_opt5.grid(row=5, column=8)
        self.report_btn = tk.Button(self.top_body, text='PLOT',
                                    command=self.plot_annual_report)
        self.report_btn.grid(row=6, column=8)

        self.result_message = tk.Label(self.bottom_body, text='', bg='white')
        self.result_message.grid(row=0, column=0)
        self.threshold_lab = tk.Label(self.top_body,
                                      text="Enter revenue threshold (NZD) and check diamond clients")
        self.threshold_lab.grid(row=8, column=8)
        self.threshold_entry = tk.Entry(self.top_body, width=15)
        self.threshold_entry.grid(row=9, column=8)
        self.threshold_btn = tk.Button(self.top_body, text='Enter',
                                       command=self.print_vip_client)
        self.threshold_btn.grid(row=10, column=8)

    def print_client_info(self):
        """Prints client information"""
        client_id = self.client_entry.get()
        if client_id not in self.report.client_id_list():
            self.text_widget.delete('1.0', tk.END)
            self.text_widget.insert('1.0', art_line)
            self.text_widget.insert(tk.END,"This Client's ID is wrong. Please try again!")
            # self.result_message.configure(text='This ID is wrong\n Please try again', fg='red')
        else:
            client_info = self.get_client_info(client_id)
            self.text_widget.delete('1.0', tk.END)
            self.text_widget.insert('1.0', art_line)
            self.text_widget.insert(tk.END, client_info)

            # self.result_message.configure(text=client_info, fg='red')

    def get_client_info(self, client_id):
        """gets client information"""
        for client in self.client_list:
            if client.id == client_id:
                return client.client_details()

    def plot_top5_product(self):
        """Plots top 5 products of a particular client"""
        client_id = self.client_entry.get()
        for client in self.client_list:
            if client.id == client_id:
                client.plot_top_5_products()

    def plot_client_monthly_total(self):
        """Plots monthly total from the client inspected"""
        client_id = self.client_entry.get()
        for client in self.client_list:
            if client.id == client_id:
                client.plot_monthly_total()

    def print_product_info(self):
        """Prints product information"""
        product_id = self.product_entry.get()
        if product_id not in self.report.product_id_list():
            self.text_widget.delete('1.0', tk.END)
            self.text_widget.insert('1.0', art_line)
            self.text_widget.insert(tk.END, "This Product's ID is wrong. Please try again!")
        else:
            product_info = self.get_product_info(product_id)
            self.text_widget.delete('1.0', tk.END)
            self.text_widget.insert('1.0', art_line)
            self.text_widget.insert(tk.END, product_info)

    def get_product_info(self, product_id):
        """gets product information"""
        for product in self.product_list:
            if product.id == product_id:
                return product.product_details()

    def plot_top5_clients(self):
        """Plots top 5 clients buying this product"""
        product_id = self.product_entry.get()
        for product in self.product_list:
            if product.id == product_id:
                product.plot_top_5_clients()

    def plot_product_monthly_total(self):
        """Plots monthly total from the product inspected"""
        product_id = self.product_entry.get()
        for product in self.product_list:
            if product.id == product_id:
                product.plot_monthly_total()

    def plot_annual_report(self):
        """plotting total revenues in categories/months/provinces/top 10 products/Clients"""
        if self.storage_variable.get() == "category":
            self.report.category_revenue()
            self.report.plot_category_revenue()
        elif self.storage_variable.get() == "month":
            self.report.plot_monthly_revenue()
        elif self.storage_variable.get() == "province":
            self.report.province_revenue()
            self.report.plot_province_revenue()
        elif self.storage_variable.get() == "product10":
            self.report.plot_top_10_Products()
        elif self.storage_variable.get() == "client10":
            self.report.plot_top_10_clients()

    def print_vip_client(self):
        """Prints VIP client information"""
        threshold = self.threshold_entry.get()
        if threshold.isnumeric() == False:
            self.text_widget.delete('1.0', tk.END)
            self.text_widget.insert('1.0', art_line)
            self.text_widget.insert(tk.END, f"Please input an amount of money which is at least {GOOD_THRESHOLD} NZD !")
        elif float(threshold) >= GOOD_THRESHOLD:
            vip_list = self.report.print_diamond_clients(float(threshold))
            self.text_widget.delete('1.0', tk.END)
            self.text_widget.insert('1.0', art_line)
            self.text_widget.insert(tk.END, vip_list)
        else:
            self.text_widget.delete('1.0', tk.END)
            self.text_widget.insert('1.0', art_line)
            self.text_widget.insert(tk.END, f"Please input threshold at least {GOOD_THRESHOLD} NZD")
        # self.result_message.configure(text=vip_list, fg='red')


