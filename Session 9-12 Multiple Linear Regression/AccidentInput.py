import os
import pandas as pd

def file_inputl(my_path):
    """
    Function to read all CSV files from a given directory into a list of DataFrames.
    
    Parameters:
    my_path (str): Path to the directory containing the CSV files.
    
    Returns:
    list: A list of pandas DataFrames.
    """
    my_dir = os.getcwd()
    os.chdir(my_path)
    my_files = [file for file in os.listdir() if file.endswith(".csv")]
    acts = [pd.read_csv(file) for file in my_files]
    os.chdir(my_dir)
    return acts

def combine_data(data_list, vars):
    """
    Function to combine multiple DataFrames from a list into a single DataFrame 
    based on specified columns.
    
    Parameters:
    data_list (list): List of pandas DataFrames.
    vars (list): List of column names to include in the final DataFrame.
    
    Returns:
    pd.DataFrame: A combined DataFrame with the specified columns.
    """
    df = data_list[0][vars]
    for i in range(1, len(data_list)):
        df = pd.concat([df, data_list[i][vars]], ignore_index=True)
    return df
