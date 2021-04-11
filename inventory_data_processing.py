import numpy as np
import pandas as pd

"""
Data processing for hitch data spreadsheets
Author: Noe Navarro
Date: 4/10/2021
"""
def df_split(file):
    """
    Author: Noe Navarro
    Date: 4/10/2021

    param1: file [str]; excel filepath
    return: list of pandas dataframe for units and sales separately
    """
    fiscal_years = ['FY16 Data', 'FY17 Data', 'FY18 Data', 'FY19 Data']
    df_unit_cont = []
    df_sale_cont = []
    for fy in fiscal_years:
        # read individual sheets
        df = pd.read_excel(file, sheet_name = fy)

        # establish unit / sales indices
        unit_index = [0,1,3,5,7,9,11,13,15,17,19,21,23]
        sales_index = [0,2,4,6,8,10,12,14,16,18,20,22,24]

        # split dataframe into two parts
        df_units = df[:, unit_index]
        df_sales = df[:, sales_index]

        # append to containers
        df_unit_cont.append(df_units)
        df_sale_cont.append(df_sales)

    return df_unit_cont, df_sale_cont

def df_concat(unit_list, sales_list):
    """
    Author: Noe Navarro
    Date: 4/10/2021

    param1: unit_list [list]; list of unit dataframes after being passed through the "data_processing" func
    param2: sales_list [list]; same as above, just sales data
    return: single dataframe for processed data
    """
    # concat into individual DFs
    df_unit = pd.concat(unit_list)
    df_sales = pd.concat(sales_list)

    # update date to datetime
    df_unit.Date = pd.to_datetime(df_unit.Date, format='%Y-%m-%d')
    df_sales.Date = pd.to_datetime(df_unit.Date, format='%Y-%m-%d')

    return df_unit, df_sales



if __name__ == '__main__':

    # prompt user for excel filepath
    file = input('What is the excel filepath? Please enter as a standard drive path (no need for quotes) >')

    # separate into unit and sales list respectively
    unit_list, sales_list = df_split(file)

    # concat fiscal years into single df
    units, sales = df_concat(unit_list, sales_list)

    # export to csv
    export_path = input('Where should the csv files export to? (please enter path) >')
    units.to_csv(export_path, index=False)
    sales.to_csv(export_path, index=False)
