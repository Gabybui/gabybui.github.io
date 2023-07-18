
"""
This report_module.py contains Report class, which is the blueprint for create report object.
This module is imported to main.py, guimodule.py.
This class will build several methods including:
- Calculating total Revenue
- Plotting Category revenue (pie chart)
- Plotting Province revenue (pie chart)
- Plotting Monthly revenue (plot bar chart)
- Plotting Top 10 Clients (plot bar chart)
- Plotting Top 10 Products (plot bar chart)
- Calculating VIP Clients whose revenue >= threshold
- Calculating months meet target (monthly target csv file) and plotting the result
"""
import pandas as pd
import matplotlib.pyplot as plt
from prettytable import PrettyTable


class Report:
    """Define Report Class"""
    def __init__(self, order_data, product_list, client_list, monthly_target):
        self.orders = order_data
        self.product_list = product_list
        self.client_list = client_list
        self.total = sum(order_data['Total'])
        self.category_total = {"NPK": 0, "Foliar Fertilizer": 0, "Organic": 0}
        self.province_total = {"Ninh Thuan": 0, "Khanh Hoa": 0}
        self.target = monthly_target

    def client_id_list(self):
        """gets list of client's IDs"""
        client_ids = []
        for client in self.client_list:
            client_ids.append(client.id)
        return client_ids

    def product_id_list(self):
        """gets list of product's IDs"""
        product_ids = []
        for product in self.product_list:
            product_ids.append(product.id)
        return product_ids

    def category_revenue(self):
        """Calculates total revenue for each product's category"""
        for product in self.product_list:
            if product.category == "NPK":
                self.category_total["NPK"] += product.total
            if product.category == "foliar fertilizer":
                self.category_total["Foliar Fertilizer"] += product.total
            if product.category == "Organic":
                self.category_total["Organic"] += product.total
        return self.category_total

    def province_revenue(self):
        """calculates total revenue for each province"""
        for client in self.client_list:
            if client.province == "Ninh Thuan":
                self.province_total["Ninh Thuan"] += client.total
            if client.province == "Khanh Hoa":
                self.province_total["Khanh Hoa"] += client.total
        return self.province_total

    def autopct_format(self, values):
        """Create pct values for bar plotting"""
        def special_format(pct):
            total = sum(values)
            value = int(round(pct * total / 100.0))
            return '{:.1f}%\n({v:d} NZD)'.format(pct, v=value)
        return special_format

    def plot_category_revenue(self):
        """Plots bar chart for category revenues"""
        labels = []
        total_amount = []
        colors = ['#ff9999', '#66b3ff', '#99ff99']
        for category, total in self.category_total.items():
            labels.append(category)
            total_amount.append(total)
        # Plot
        axes = plt.axes()
        plt.pie(total_amount, labels=labels, colors=colors, shadow=True, autopct=self.autopct_format(total_amount))
        axes.set_title("TOTAL REVENUE IN CATEGORIES")
        plt.show()

    def plot_province_revenue(self):
        """plots piechart based on province total revenue"""
        labels = []
        total_amount = []
        colors = ['#ff9999', '#66b3ff']
        for province, total in self.province_total.items():
            labels.append(province)
            total_amount.append(total)
        # Plot
        axes = plt.axes()
        plt.pie(total_amount, labels=labels, colors=colors, shadow=True, autopct=self.autopct_format(total_amount))
        axes.set_title("TOTAL REVENUE IN PROVINCES")
        plt.show()

    def plot_monthly_revenue(self):
        """plots bar chart for monthly revenue"""
        data = self.orders[['Date', 'Total']]
        data['Date'] = pd.to_datetime(data.Date)
        monthly_total = data.groupby(data.Date.dt.month)['Total'].sum()
        month_labels = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'July', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
        month_empty = {1: 0, 2: 0, 3: 0, 4: 0, 5: 0, 6: 0, 7: 0, 8: 0, 9: 0, 10: 0, 11: 0, 12: 0}
        month_values = monthly_total.to_dict()
        month_dict = {**month_empty, **month_values}
        months = list(month_dict.keys())
        totals = list(month_dict.values())
        targets = self.target['Target'].to_list()
        # plot bar chart:
        axes = plt.axes()
        xs = months
        ys2 = targets
        ys = totals
        axes.bar(xs, ys, color='blue', label="Reality")
        axes.plot(xs, ys2,'ro-', color='orange', label="Target")
        axes.set_title(f"Total Revenue In 12 Months", color='#333333')
        axes.set_xlabel("Month", color='#333333')
        axes.set_ylabel("NZD", color='#333333')
        axes.set_xticks(range(1, 13))
        axes.set_xticklabels(month_labels)
        axes.legend(loc='best')
        axes.grid(False)
        plt.show()

    def plot_top_10_clients(self):
        """Plots Top 10 Clients of the year:"""
        data = self.orders[["Client ID", "Total"]]
        top10_data = data.groupby(['Client ID'], as_index=False).sum().sort_values(by='Total',
                                                                                       ascending=False).head(10)
        axes = plt.axes()
        xs = top10_data['Client ID'].tolist()
        ys = top10_data['Total'].tolist()
        axes.bar(xs, ys, color='orange')
        axes.set_title(f"Top 10 Clients Of The Year")
        axes.set_xlabel("Clients")
        axes.set_ylabel("NZD")
        axes.grid(True)
        plt.show()

    def plot_top_10_Products(self):
        """Plots Top 10 Products of the year:"""
        data = self.orders[["Product ID", "Total"]]
        top10_data = data.groupby(['Product ID'], as_index=False).sum().sort_values(by='Total',
                                                                                       ascending=False).head(10)
        axes = plt.axes()
        xs = top10_data['Product ID'].tolist()
        ys = top10_data['Total'].tolist()
        axes.bar(xs, ys, color='orange')
        axes.set_title(f"Top 10 Products Of The Year")
        axes.set_xlabel("Products")
        axes.set_ylabel("NZD")
        axes.set_xticks(range(0, 10))
        axes.set_xticklabels(xs, rotation=45)
        axes.grid(True)
        plt.show()

    def print_diamond_clients(self, revenue_threshold):
        """Returns a list of clients with revenue equal or more than threshold"""
        table = PrettyTable()
        table.field_names = ["Client ID", "Name", "Province", "Total Revenue"]
        diamond_clients = []
        for client in self.client_list:
            if client.total >= revenue_threshold:
                diamond_clients.append(client)
        for client in diamond_clients:
            table.add_row([client.id, client.name, client.province, client.total])
        table.align["Total Revenue"] = "r"
        return table


