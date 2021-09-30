import pandas as pd
import tensorflow as tf
physical_devices = tf.config.list_physical_devices('GPU') 
for device in physical_devices:
    tf.config.experimental.set_memory_growth(device, True)
from tensorflow import keras
from tensorflow.keras import layers, Sequential, callbacks
from tensorflow.keras.layers import Dense, LSTM, Dropout
import numpy as np
import time
from sklearn.preprocessing import MinMaxScaler
import datetime as dt
start_time = time.time()


def main():
    # Read data in
    df = pd.read_csv('Full_Data.csv', parse_dates = ['DAY'])
#    days_back = [7]
#    days_forward = [30]
#    year_back = [False]
#    days_back = [7, 14]
#    days_forward = [7, 151]
#    target_list = ['COAST_LOAD','EAST_LOAD','FAR_WEST_LOAD','NORTH_LOAD',
#                   'NORTH_CENTRAL_LOAD','SOUTHERN_LOAD','SOUTH_CENTRAL_LOAD',
#                   'WEST_LOAD', 'GEN_TOTAL']
#    train = .7
#    year_back = [True, False]

#   best results dictionaries
    best_results_short = {'COAST_LOAD':{'days_forward':7, 'days_back':14,'year_back':False, 'nodes1':64, 'nodes2':32, 'dense_layer':True},
                    'EAST_LOAD':{'days_forward':7, 'days_back':7,'year_back':True, 'nodes1':256, 'nodes2':128, 'dense_layer':False},
                    'FAR_WEST_LOAD':{'days_forward':7, 'days_back':7,'year_back':False, 'nodes1':128, 'nodes2':64, 'dense_layer':False},
                    'NORTH_CENTRAL_LOAD':{'days_forward':7, 'days_back':7,'year_back':True, 'nodes1':128, 'nodes2':64, 'dense_layer':False},
                    'NORTH_LOAD':{'days_forward':7, 'days_back':7,'year_back':True, 'nodes1':128, 'nodes2':64, 'dense_layer':False},
                    'SOUTHERN_LOAD':{'days_forward':7, 'days_back':14,'year_back':False, 'nodes1':256, 'nodes2':128, 'dense_layer':True},
                    'SOUTH_CENTRAL_LOAD':{'days_forward':7, 'days_back':7,'year_back':True, 'nodes1':64, 'nodes2':32, 'dense_layer':True},
                    'WEST_LOAD':{'days_forward':7, 'days_back':7,'year_back':False, 'nodes1':128, 'nodes2':64, 'dense_layer':False},
                    'GEN_TOTAL':{'days_forward':7, 'days_back':14,'year_back':True, 'nodes1':128, 'nodes2':64, 'dense_layer':False}}
    
    best_results_long = {'COAST_LOAD':{'days_forward':151, 'days_back':7,'year_back':True, 'nodes1':64, 'nodes2':32, 'dense_layer':True},
                    'EAST_LOAD':{'days_forward':151, 'days_back':7,'year_back':True, 'nodes1':256, 'nodes2':128, 'dense_layer':False},
                    'FAR_WEST_LOAD':{'days_forward':151, 'days_back':7,'year_back':False, 'nodes1':128, 'nodes2':64, 'dense_layer':True},
                    'NORTH_CENTRAL_LOAD':{'days_forward':151, 'days_back':14,'year_back':True, 'nodes1':256, 'nodes2':128, 'dense_layer':False},
                    'NORTH_LOAD':{'days_forward':151, 'days_back':7,'year_back':True, 'nodes1':256, 'nodes2':128, 'dense_layer':True},
                    'SOUTHERN_LOAD':{'days_forward':151, 'days_back':7,'year_back':True, 'nodes1':64, 'nodes2':32, 'dense_layer':True},
                    'SOUTH_CENTRAL_LOAD':{'days_forward':151, 'days_back':7,'year_back':True, 'nodes1':128, 'nodes2':64, 'dense_layer':False},
                    'WEST_LOAD':{'days_forward':151, 'days_back':14,'year_back':True, 'nodes1':128, 'nodes2':64, 'dense_layer':True},
                    'GEN_TOTAL':{'days_forward':151, 'days_back':14,'year_back':True, 'nodes1':64, 'nodes2':32, 'dense_layer':True}}

#################################################
    # control for functions. 
    clean_df, df_test = data_clean(df)
#    grid_search(clean_df, target_list, days_back, days_forward, year_back, train)
    final_model(clean_df, df_test, best_results_short, best_results_long)
    
    
    
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
    df_test = df[df['DAY'] >= dt.datetime.strptime('2020-12-17', '%Y-%m-%d')]
    df = df[df['DAY'] < dt.datetime.strptime('2021-01-01', '%Y-%m-%d')]
    
    # Add variable Day of Week
    df['DayofWeek'] = df['DAY'].dt.dayofweek
    df_test['DayofWeek'] = df_test['DAY'].dt.dayofweek
    
    return df, df_test

def data_prep(df, target, days_back, days_forward, year_back):
    """windowing function

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
    # window generator. Splits data into inputs and outputs. 
    # inputs: Takes the amount of days back and if year_back is true, 
    # takes the amount of days back last year and turns them into an
    # input for a single day.
    # outputs: each input is tied to an output that is days_forward 
    # long. The default is 30 days, so each input by default predicts
    # 30 days worth of data.
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
            y.append(future[target])

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
            y.append(future[target])
    
    
            
    return np.array(X).astype('float32'), np.array(y).astype('float32')
    
def final_model(df, df_eval, best_results_short, best_results_long):
    """This function finishes preprocessing, scales the data, builds the model and 
    outputs model performance metrics.

    Args:
        df (dataframe): Cleaned dataframe ready for windowing. Output from data clean
        days_back (int, optional): Number of days to be used as input. Defaults to 7.
        days_forward (int, optional): Number of days to predict. Defaults to 30.
        year_back (bool, optional): If True, takes data from last year in as input as well. 
        Defaults to True.
        train (float, optional): Create train / test split. Defaults to .7.
    """
    # Finish data preprocessing prior to windowing
    
    df = df.drop(columns = ['DAY'])
    df = df.replace(',', '', regex = True)
    df_eval = df_eval.replace(',', '', regex = True)
    # Split into train test split
    df_train = df[0:round(len(df)*.7)]
    df_test = df[round(len(df)*.7):len(df)]
    # create Scalers for data data
    scaler = MinMaxScaler(feature_range=(0,1))
    scaler_y = MinMaxScaler(feature_range=(0,1))
    trained_scaler = scaler.fit(df_train)
    # lists to hold results
    results_list = []
    daily_results = []
# run NN for short term results
    for key in best_results_short.keys():
        # scale test and train data
        trained_scaler_y = scaler_y.fit(np.asarray(df_train[key]).reshape(-1,1))
        train_norm = trained_scaler.transform(df_train)
        test_norm = trained_scaler.transform(df_test)
        # Create normalized data frames for windowing
        df_norm_train = pd.DataFrame(train_norm, columns = list(df_train))
        df_norm_test = pd.DataFrame(test_norm, columns = list(df_train))

        # Window the data to create X and y train and test data
        X_train, y_train = data_prep(df_norm_train, key, best_results_short[key]['days_back'], best_results_short[key]['days_forward'], best_results_short[key]['year_back'])
        X_test, y_test = data_prep(df_norm_test, key, best_results_short[key]['days_back'], best_results_short[key]['days_forward'], best_results_short[key]['year_back'])

        # Build the model. 1 LSTM layer, 1 dense layer (if necessary), 
        # then a prediction layer
        # dropout of 0.2 in between layers
        model = Sequential()
        model.add(LSTM(units = best_results_short[key]['nodes1'], activation = 'relu',
                    input_shape = (X_train.shape[1], X_train.shape[2])))
        model.add(Dropout(0.2))
        if best_results_short[key]['dense_layer'] == True:
            model.add(Dense(units = best_results_short[key]['nodes2'], activation = 'relu'))
            model.add(Dropout(0.2))
        model.add(Dense(best_results_short[key]['days_forward']))
        #Compile and fit model
        model.compile(loss='mse', optimizer='adam')
        early_stop = keras.callbacks.EarlyStopping(monitor = 'loss',
                                                patience = 10)
        fit_model = model.fit(X_train, y_train, epochs = 10000,  
                            batch_size = 32, 
                            shuffle = False, callbacks = [early_stop])    
        
        # create predictions, get actuals, evaluate using 
        # mae and mse for the whole model, and just for the next
        # 30 days.
        prediction_test = model.predict(X_test)
        prediction_test = trained_scaler_y.inverse_transform(prediction_test)
        actual_test = trained_scaler_y.inverse_transform(y_test)
        test_mse = evaluate_prediction(prediction_test, actual_test)

        prediction_train = model.predict(X_train)
        prediction_train = trained_scaler_y.inverse_transform(prediction_train)
        actual_train = trained_scaler_y.inverse_transform(y_train)
        train_mse = evaluate_prediction(prediction_train, actual_train)                        
        # evaluation data dataframe creation and scaling. 
        # this is for 1/1/2021 - 1/7/2021
        X_eval_norm = df_eval[(dt.datetime.strptime('2021-01-01', '%Y-%m-%d') - 
                              dt.timedelta(days = best_results_short[key]['days_back']) <= 
                              df_eval['DAY']) & (df_eval['DAY'] < dt.datetime.strptime('2021-01-01', '%Y-%m-%d'))]
        X_eval_norm = X_eval_norm.drop(columns = ['DAY'])
        X_eval_norm = trained_scaler.transform(X_eval_norm)
        y_eval_norm = df_eval[(dt.datetime.strptime('2021-01-01', '%Y-%m-%d')  <= 
                              df_eval['DAY']) & (df_eval['DAY'] < dt.datetime.strptime('2021-01-01', '%Y-%m-%d') + 
                              dt.timedelta(days = best_results_short[key]['days_forward']))]
        y_eval_norm = y_eval_norm.drop(columns = ['DAY'])
        y_eval_norm = trained_scaler.transform(y_eval_norm)
        y_eval_norm = pd.DataFrame(y_eval_norm, columns = list(df_train))
        y_eval_norm = y_eval_norm[key]
        # evaluation data dataframe creation and scaling. 
        # this is for 2/13/2021 - 2/19/2021
        X_eval_storm = df_eval[(dt.datetime.strptime('2021-02-13', '%Y-%m-%d') - 
                              dt.timedelta(days = best_results_short[key]['days_back']) <= 
                              df_eval['DAY']) & (df_eval['DAY'] < dt.datetime.strptime('2021-02-13', '%Y-%m-%d'))]
        X_eval_storm = X_eval_storm.drop(columns = ['DAY'])
        X_eval_storm = trained_scaler.transform(X_eval_storm)
        y_eval_storm = df_eval[(dt.datetime.strptime('2021-02-13', '%Y-%m-%d')  <= 
                              df_eval['DAY']) & (df_eval['DAY'] < dt.datetime.strptime('2021-02-13', '%Y-%m-%d') + 
                              dt.timedelta(days = best_results_short[key]['days_forward']))]
        y_eval_storm = y_eval_storm.drop(columns = ['DAY'])
        y_eval_storm = trained_scaler.transform(y_eval_storm)
        y_eval_storm = pd.DataFrame(y_eval_storm, columns = list(df_train))
        y_eval_storm = y_eval_storm[key]  
        
        # Turn evaluation data into numpy arrays and expand dimensions
        # for prediction
        X_eval_norm = np.array(X_eval_norm).astype('float32')
        y_eval_norm = np.array(y_eval_norm).astype('float32')
        X_eval_storm = np.array(X_eval_storm).astype('float32')
        y_eval_storm = np.array(y_eval_storm).astype('float32')    
        X_eval_norm = np.expand_dims(X_eval_norm, axis = 0)
        y_eval_norm = np.expand_dims(y_eval_norm, axis = 0)
        X_eval_storm = np.expand_dims(X_eval_storm, axis = 0)
        y_eval_storm = np.expand_dims(y_eval_storm, axis = 0)
        
        # create predictions with evaluation data: 
        # this is for 1/1/2021 - 1/7/2021
        prediction_norm = model.predict(X_eval_norm)
        preds_norm = trained_scaler_y.inverse_transform(prediction_norm)
        actual_norm = trained_scaler_y.inverse_transform(y_eval_norm)
        norm_mse = evaluate_prediction(preds_norm, actual_norm)

        # create predictions with evaluation data: 
        # this is for 2/13/2021 - 2/19/2021
        prediction_storm = model.predict(X_eval_storm)
        preds_storm = trained_scaler_y.inverse_transform(prediction_storm)
        actual_storm = trained_scaler_y.inverse_transform(y_eval_storm)
        storm_mse = evaluate_prediction(preds_storm, actual_storm) 
        
        # put results into dictionary lists
        daily_results.append({'target':key, 'length':'short', 'normal_preds':preds_norm, 'storm_preds':preds_storm})         
        results_list.append({'target':key, 'length':'short', 'train_mse':train_mse, 'test_mse':test_mse, 'norm_ase':norm_mse, 'storm_ase':storm_mse})
        print(results_list)

# run NN for long term prediction
    for key in best_results_long.keys():
        trained_scaler_y = scaler_y.fit(np.asarray(df_train[key]).reshape(-1,1))
        train_norm = trained_scaler.transform(df_train)
        test_norm = trained_scaler.transform(df_test)
        # Create normalized data frames for windowing
        df_norm_train = pd.DataFrame(train_norm, columns = list(df_train))
        df_norm_test = pd.DataFrame(test_norm, columns = list(df_train))

        # Window the data to create X and y train and test data
        X_train, y_train = data_prep(df_norm_train, key, best_results_long[key]['days_back'], best_results_long[key]['days_forward'], best_results_long[key]['year_back'])
        X_test, y_test = data_prep(df_norm_test, key, best_results_long[key]['days_back'], best_results_long[key]['days_forward'], best_results_long[key]['year_back'])

        # Build the model. 1 LSTM layer, 1 dense layer, then a prediction layer
        # dropout of 0.2 in between layers
        model = Sequential()
        model.add(LSTM(units = best_results_long[key]['nodes1'], activation = 'relu',
                    input_shape = (X_train.shape[1], X_train.shape[2])))
        model.add(Dropout(0.2))
        if best_results_long[key]['dense_layer'] == True:
            model.add(Dense(units = best_results_long[key]['nodes2'], activation = 'relu'))
            model.add(Dropout(0.2))
        model.add(Dense(best_results_long[key]['days_forward']))
        #Compile and fit model
        model.compile(loss='mse', optimizer='adam')
        early_stop = keras.callbacks.EarlyStopping(monitor = 'loss',
                                                patience = 10)
        fit_model = model.fit(X_train, y_train, epochs = 10000,  
                            batch_size = 32, 
                            shuffle = False, callbacks = [early_stop])    
        
        # create predictions, get actuals, evaluate using 
        # mae and mse for the whole model, and just for the next
        # 30 days.
        prediction_test = model.predict(X_test)
        prediction_test = trained_scaler_y.inverse_transform(prediction_test)
        actual_test = trained_scaler_y.inverse_transform(y_test)
        test_mse = evaluate_prediction(prediction_test, actual_test)

        prediction_train = model.predict(X_train)
        prediction_train = trained_scaler_y.inverse_transform(prediction_train)
        actual_train = trained_scaler_y.inverse_transform(y_train)
        train_mse = evaluate_prediction(prediction_train, actual_train)                        
        
        # evaluation data dataframe creation and scaling. 
        # this is for 1/1/2021 - 5/31/2021
        X_eval_long = df_eval[(dt.datetime.strptime('2021-01-01', '%Y-%m-%d') - 
                              dt.timedelta(days = best_results_long[key]['days_back']) <= 
                              df_eval['DAY']) & (df_eval['DAY'] < dt.datetime.strptime('2021-01-01', '%Y-%m-%d'))]
        X_eval_long = X_eval_long.drop(columns = ['DAY'])
        X_eval_long = trained_scaler.transform(X_eval_long)
        y_eval_long = df_eval[(dt.datetime.strptime('2021-01-01', '%Y-%m-%d')  <= 
                              df_eval['DAY']) & (df_eval['DAY'] < dt.datetime.strptime('2021-01-01', '%Y-%m-%d') + 
                              dt.timedelta(days = best_results_long[key]['days_forward']))]
        y_eval_long = y_eval_long.drop(columns = ['DAY'])
        y_eval_long = trained_scaler.transform(y_eval_long)
        y_eval_long = pd.DataFrame(y_eval_long, columns = list(df_train))
        y_eval_long = y_eval_long[key]
        
        # Turn evaluation data into numpy arrays and expand dimensions
        # for prediction        
        X_eval_long = np.array(X_eval_long).astype('float32')
        y_eval_long = np.array(y_eval_long).astype('float32') 
        X_eval_long = np.expand_dims(X_eval_long, axis = 0)
        y_eval_long = np.expand_dims(y_eval_long, axis = 0)  
    
        # create predictions with evaluation data: 
        # this is for 2/13/2021 - 2/19/2021
        prediction_long = model.predict(X_eval_long)
        preds_long = trained_scaler_y.inverse_transform(prediction_long)
        actual_long = trained_scaler_y.inverse_transform(y_eval_long)
        long_mse = evaluate_prediction(preds_long, actual_long) 

        # put results into dictionary list
        daily_results.append({'target':key, 'length':'long', 'long_preds':preds_long})         
        results_list.append({'target':key, 'length':'long', 'train_mse':train_mse, 'test_mse':test_mse, 'long_ase':long_mse})
        print(results_list)
    # save results to csv
    results_df = pd.DataFrame(results_list)
    daily_df = pd.DataFrame(daily_results)
    results_df.to_csv('eval_resultsdf.csv')
    daily_df.to_csv('daily_df.csv')

def evaluate_prediction(predictions, actual):
    # various evaluation metrics
    # MSE is the one that ends up being used
    errors = predictions - actual
    mse_1 = np.square(errors[len(errors) - 1]).mean()
    mae_1 = np.abs(errors[len(errors) - 1]).mean()
    mse = np.square(errors).mean()
    rmse = np.sqrt(mse)
    mae = np.abs(errors).mean()
    
    return mse

def grid_search(df, target_list, days_list, fwd_list, year_list, train):
    df = df.drop(columns = ['DAY'])
    df = df.replace(',', '', regex = True)
    # Split into train test split
    df_train = df[0:round(len(df)*train)]
    df_test = df[round(len(df)*train):len(df)]
    # Scale data
    scaler = MinMaxScaler(feature_range=(0,1))
    trained_scaler = scaler.fit(df_train)
    results_list = []
    for target in target_list:
        scaler_y = MinMaxScaler(feature_range=(0,1))
        trained_scaler_y = scaler_y.fit(np.asarray(df_train[target]).reshape(-1,1))
        train_norm = trained_scaler.transform(df_train)
        test_norm = trained_scaler.transform(df_test)
        # Create normalized data frames for windowing
        df_norm_train = pd.DataFrame(train_norm, columns = list(df_train))
        df_norm_test = pd.DataFrame(test_norm, columns = list(df_train))
        for days_forward in fwd_list:    
            for year_back in year_list:
                for days_back in days_list:
                    # Window the data to create X and y train and test data
                    X_train, y_train = data_prep(df_norm_train, target, days_back, days_forward, year_back)
                    X_test, y_test = data_prep(df_norm_test, target, days_back, days_forward, year_back)

                    # Build the model. 1 LSTM layer, 1 dense layer, then a prediction layer
                    # dropout of 0.2 in between layers
                    
                    for nodes in [[64,32], [128, 64], [256, 128]]:
                        for dense_layer in [True, False]:
                            for x in range(5):
                                model = Sequential()
                                model.add(LSTM(units = nodes[0], activation = 'relu',
                                            input_shape = (X_train.shape[1], X_train.shape[2])))
                                model.add(Dropout(0.2))
                                if dense_layer == True:
                                    model.add(Dense(units = nodes[1], activation = 'relu'))
                                    model.add(Dropout(0.2))
                                model.add(Dense(days_forward))
                                #Compile and fit model
                                model.compile(loss='mse', optimizer='adam')
                                early_stop = keras.callbacks.EarlyStopping(monitor = 'loss',
                                                                        patience = 10)
                                fit_model = model.fit(X_train, y_train, epochs = 10000,  
                                                    batch_size = 32, 
                                                    shuffle = False, callbacks = [early_stop])    
                                
                                # create predictions, get actuals, evaluate using 
                                # mae and mse for the whole model, and just for the next
                                # 30 days.
                                prediction_test = model.predict(X_test)
                                prediction_test = trained_scaler_y.inverse_transform(prediction_test)
                                actual_test = trained_scaler_y.inverse_transform(y_test)
                                test_mse = evaluate_prediction(prediction_test, actual_test)

                                prediction_train = model.predict(X_train)
                                prediction_train = trained_scaler_y.inverse_transform(prediction_train)
                                actual_train = trained_scaler_y.inverse_transform(y_train)
                                train_mse = evaluate_prediction(prediction_train, actual_train)                        
                                
                                # put results into results list of dictionaries
                                results_list.append({'target':target, 'days_forward':days_forward, 'days_back':days_back, 
                                                    'year_back':year_back, 'nodes1':nodes[0], 'nodes2':nodes[1], 'x':x,
                                                    'dense_layer':dense_layer, 'train_mse':train_mse, 'test_mse':test_mse})
                                print(results_list)
                                
        # turn results dictionary  into dataframe
        results_df = pd.DataFrame(results_list)
        results_df.to_csv('resultsdf_new.csv')
    
if __name__ == '__main__':
    main()
    print("--- %s seconds ---" % (time.time() - start_time))