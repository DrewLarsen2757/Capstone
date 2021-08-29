import pandas as pd
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
import numpy as np

def main():
    # Read data in
    df = pd.read_csv('Full_Data.csv')

#################################################
    clean_df = data_prep(df)
    build_model(clean_df)
    
def data_prep(df):
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
    print(len(list(df)))
    
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

    return df
    
def build_model(df):
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