import pandas as pd
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
import numpy as np
import time
start_time = time.time()


def main():
    # Read data in
    df = pd.read_csv('Full_Data.csv', parse_dates = ['DAY'])
    days_back = 7
    days_forward = 30
    year_back = False

#################################################
    clean_df = data_clean(df)
    X, y = data_prep(clean_df, days_back, days_forward, year_back)
    build_model(X, y, days_back, days_forward, year_back)
    
def data_clean(df):
    # Remove columns with PGTM, WDF5, WSF5, TAVG. 
    # Much of these columns is missing. We have TMIN and TMAX, 
    # so TAVG is unnecessary on top of those. The other 4 columns
    # have to do with direction of wind speed, time of peak
    # wind gust, fastest 5 second wind speed and direction
    # of fastest 5 second wind speed. All these columns had missing
    # data and were unnecessary with the other weather factors
    # that we have. 
    col_to_keep = [x for x in list(df) if 'PGTM' 
                  not in x and 'WDF5' not in x and 'WSF5' not in x 
                  and 'TAVG' not in x]
    df = df[col_to_keep]
    
    # Keep only imputed population columns
    pop_col_to_keep = [x for x in list(df) if 'POP' not in x or 'IMPUTED' in x ]
    df = df[pop_col_to_keep]
    
    # Fill na with 0
    # Biomass and Solar gen have some NAs at the beginning, they can be filled 
    # with 0 without worry.
    # WT columns have quite a few NAs. This is fine. They are one hot encoded
    # columns with 1 as true and NA as false. Fill NA with 0 and this is fixed.
    # East_PRCP, East_TMAX and East_TMIN have a small number of NAs (< 10 each). 
    # AWND also has a small number of NAs for each zone (1 or 2 for each zone).
    # These were addressed in the excel file by taking the average of the value
    # before and after for TMIN and TMAX. The one missing precip day was assumed
    # to have 0 precip.
    for x in list(df):
        print(x, df[x].isna().sum())
    df = df.fillna(0)
    
    # Add variable Day of Week
    df['DayofWeek'] = df['DAY'].dt.dayofweek

    return df

def data_prep(df, days_back = 7, days_forward = 30, year_back = True):
    """[summary]

    Args:
        df (pandas dataframe): Output from data clean function
        days_back (int, optional): Number of days to go back to predict the days forward. Defaults to 7.
        days_forward (int, optional): Number of future days to predict. Defaults to 30.
        year_back (bool, optional): Takes last years data and uses it, along with this year's data
        to predict the number of days forward. Defaults to True.

    Returns:
        X: Inputs for LSTM network
        y: Outputs for LSTM network
    """
    X, y = list(), list()
    n = range(366 + days_back, len(df))
    if year_back == True:
        for window_start in n:
            LY_window_start = window_start - 365
            LY_window_end = window_start - 365 + days_back
            past_end = window_start + days_back
            future_end = past_end + days_forward
            if future_end > len(df):
                break
        # slicing the past and future parts of the window
            LY_window = df[LY_window_start:LY_window_end]
            TY_past = df[window_start:past_end]
            past = LY_window.append(TY_past)
            future = df[past_end:future_end]
            X.append(past)
            y.append(future)

    else:
        for window_start in range(len(df)):
            past_end = window_start + days_back
            future_end = past_end + days_forward
            if future_end > len(df):
                break
        # slicing the past and future parts of the window
            past = df[window_start:past_end] 
            future = df[past_end:future_end]
            X.append(past)
            y.append(future)
            if window_start < 5 or window_start > (len(df) - 35):
                print(past)
                print(future)

            
    return np.array(X), np.array(y)
    
def build_model(df, days_back = 7, days_forward = 30, year_back = True):
    if year_back == True:
        num_input = days_back * 2
    else:
        num_input = days_back
    num_output = days_forward
    n = len(df)
    train_df = df[0:int(n*0.7)]
    val_df = df[int(n*0.7):int(n*0.9)]
    test_df = df[int(n*0.9):0]
    num_features = df.shape[1]
    train_mean = train_df.mean()
    train_std = train_df.std()

    train_df = (train_df - train_mean) / train_std
    val_df = (val_df - train_mean) / train_std
    test_df = (test_df - train_mean) / train_std
 
    
if __name__ == '__main__':
    main()
    print("--- %s seconds ---" % (time.time() - start_time))