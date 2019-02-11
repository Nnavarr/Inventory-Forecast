import numpy as np
import pandas as pd

# Import Data from Excel Sheet ----
f16_hitch_data = pd.read_excel("//adfs01.uhi.amerco/departments/mia/group/MIA"
                              "/Noe"
                           "/Projects/Hitch Inventory/Hitch FY Data.xlsx",
                           sheet_name='FY16 Data')

f17_hitch_data = pd.read_excel("//adfs01.uhi.amerco/departments/mia/group/MIA"
                              "/Noe"
                           "/Projects/Hitch Inventory/Hitch FY Data.xlsx",
                           sheet_name='FY17 Data')

f18_hitch_data = pd.read_excel("//adfs01.uhi.amerco/departments/mia/group/MIA"
                              "/Noe"
                           "/Projects/Hitch Inventory/Hitch FY Data.xlsx",
                           sheet_name='FY18 Data')

f19_hitch_data = pd.read_excel("//adfs01.uhi.amerco/departments/mia/group/MIA"
                              "/Noe"
                           "/Projects/Hitch Inventory/Hitch FY Data.xlsx",
                           sheet_name='FY19 Data')

# Create Row Index values for units and sales ----
unit_index = [0,1,3,5,7,9,11,13,15,17,19,21,23]
sales_index = [0,2,4,6,8,10,12,14,16,18,20,22,24]

# Split Unit / Sales Data frames ----
f16_hitch_units = f16_hitch_data.iloc[:, unit_index]
f17_hitch_units = f17_hitch_data.iloc[:, unit_index]
f18_hitch_units = f18_hitch_data.iloc[:, unit_index]
f19_hitch_units = f19_hitch_data.iloc[:, unit_index]

f16_hitch_sales = f16_hitch_data.iloc[:, sales_index]
f17_hitch_sales = f17_hitch_data.iloc[:, sales_index]
f18_hitch_sales = f18_hitch_data.iloc[:, sales_index]
f19_hitch_sales = f19_hitch_data.iloc[:, sales_index]

# Initial Data check ; equivalent rows ----
assert f16_hitch_units.shape[0] == f17_hitch_units.shape[0] == \
       f18_hitch_units.shape[0] == f19_hitch_units.shape[0]

assert f16_hitch_sales.shape[0] == f17_hitch_sales.shape[0] == \
       f18_hitch_sales.shape[0] == f19_hitch_sales.shape[0]

# Merge units DF ----
hitch_units = pd.merge(left=f16_hitch_units, right=f17_hitch_units, on='Part Number')
hitch_units = pd.merge(left=hitch_units, right=f18_hitch_units, on='Part '
                                                                   'Number')
hitch_units = pd.merge(left=hitch_units, right=f19_hitch_units, on='Part '
                                                                   'Number')

# Merge sales DF ----
hitch_sales = pd.merge(left=f16_hitch_sales, right=f17_hitch_sales, on='Part '
                                                                       'Number')
hitch_sales = pd.merge(left=hitch_sales, right=f18_hitch_sales, on='Part '
                                                                   'Number')
hitch_sales = pd.merge(left=hitch_sales, right=f19_hitch_sales, on='Part '
                                            'Number')

# Remove missing data (March 2019) ----
hitch_units.drop(hitch_units.columns[48], axis=1, inplace=True)
hitch_sales.drop(hitch_sales.columns[48], axis=1, inplace=True)

# Melt Dataframes into a work friendly version ----
melted_hitch_sales = hitch_sales.melt(id_vars= 'Part Number',
                                      var_name='Date', value_name='Revenue')
melted_hitch_units = hitch_units.melt(id_vars= 'Part Number',
                                      var_name='Date', value_name='Units')

# Remove Timestamp from Date column ----
melted_hitch_sales['Date'] = pd.to_datetime(melted_hitch_sales[
                                                'Date']).dt.date
melted_hitch_units['Date'] = pd.to_datetime(melted_hitch_units[
                                                'Date']).dt.date

# Export Data to CSV ----
melted_hitch_sales.to_csv('Z:/group/MIA/Noe/Projects/Hitch '
                          'Inventory/Data/hitch_sales.csv', index=False)

melted_hitch_units.to_csv('Z:/group/MIA/Noe/Projects/Hitch '
                          'Inventory/Data/hitch_units.csv', index=False)

