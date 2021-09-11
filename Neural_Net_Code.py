import pandas as pd
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers, Sequential, callbacks
from tensorflow.keras.layers import Dense, LSTM, Dropout
import numpy as np
import time
from sklearn.preprocessing import MinMaxScaler
start_time = time.time()


def main():
    # Read data in
    df = pd.read_csv('Full_Data.csv', parse_dates = ['DAY'])
    days_back = 7
    days_forward = 30
    train = .7
    year_back = True

#################################################
    clean_df = data_clean(df)
    build_model(clean_df, days_back, days_forward, year_back, train = .7)
    
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
    # to have 0 preci, index_col=['DAY']p.
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
            y.append(future['TOTAL_LOAD'])

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
    
    
            
    return np.array(X).astype('float32'), np.array(y).astype('float32')
    
def build_model(df, days_back = 7, days_forward = 30, year_back = True, train = .7):
    # test train splits
    df = df.drop(columns = ['DAY'])
    df = df.replace(',', '', regex = True)
    df_train = df[0:round(len(df)*.7)]
    df_test = df[round(len(df)*.7):len(df)]
    scaler = MinMaxScaler(feature_range=(0,1))
    scaler_y = MinMaxScaler(feature_range=(0,1))
    trained_scaler = scaler.fit(df_train)
    trained_scaler_y = scaler_y.fit(np.asarray(df_train['TOTAL_LOAD']).reshape(-1,1))

    train_norm = trained_scaler.transform(df_train)
    test_norm = trained_scaler.transform(df_test)

    df_norm_train = pd.DataFrame(train_norm, columns = list(df_train))
    df_norm_test = pd.DataFrame(test_norm, columns = list(df_train))
    

    X_train, y_train = data_prep(df_norm_train, days_back, days_forward, year_back)
    X_test, y_test = data_prep(df_norm_test, days_back, days_forward, year_back)


    model = Sequential()
    model.add(LSTM(units = 200, activation = 'relu',
                input_shape = (X_train.shape[1], X_train.shape[2])))
    model.add(Dropout(0.2))
    model.add(Dense(units = 100, activation = 'relu'))
    model.add(Dropout(0.2))
    model.add(Dense(days_forward))
    #Compile model
    model.compile(loss='mse', optimizer='adam')
    early_stop = keras.callbacks.EarlyStopping(monitor = 'loss',
                                               patience = 10)
    fit_model = model.fit(X_train, y_train, epochs = 10000,  
                        batch_size = 16, 
                        shuffle = False, callbacks = [early_stop])    
    
    prediction = model.predict(X_test)
    prediction = trained_scaler_y.inverse_transform(prediction)
    actual = trained_scaler_y.inverse_transform(y_test)
    evaluate_prediction(prediction, actual, 'LSTM')

    print('done')

def evaluate_prediction(predictions, actual, model_name):
    errors = predictions - actual
    mse = np.square(errors).mean()
    rmse = np.sqrt(mse)
    mae = np.abs(errors).mean()
    print(model_name + ':')
    print('Mean Absolute Error: {:.4f}'.format(mae))
    print('Mean Square Error: {:.4f}'.format(mse))
    print('')

    
if __name__ == '__main__':
    main()
    print("--- %s seconds ---" % (time.time() - start_time))