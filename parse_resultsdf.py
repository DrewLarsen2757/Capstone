import pandas as pd

def main():
    df = pd.read_csv('resultsdf_new.csv')
    df = df.drop(columns = ['Unnamed: 0', 'x'])
    groupdf = df.groupby(['target', 'days_forward', 'days_back', 'year_back', 'nodes1', 'nodes2', 'dense_layer']).mean().reset_index()
    topdf = pd.DataFrame(columns = list(groupdf))
    for x in groupdf['target'].unique():
        targetdf14 = groupdf[(groupdf['target'] == x) & (groupdf['days_forward'] == 7)]
        top3train14 = targetdf14.sort_values(by = ['train_mse']).head(3)
        top3test14 = targetdf14.sort_values(by = ['test_mse']).head(3)
        targetdf365 = groupdf[(groupdf['target'] == x) & (groupdf['days_forward'] == 151)]
        top3train365 = targetdf365.sort_values(by = ['train_mse']).head(3)
        top3test365 = targetdf365.sort_values(by = ['test_mse']).head(3)
        topdf = topdf.append(top3train14)
        topdf = topdf.append(top3test14)
        topdf = topdf.append(top3train365)
        topdf = topdf.append(top3test365)
        
    
    topdf.to_csv('topdf.csv')
    
    
    
if __name__ == '__main__':
#    main()
    parse_ercot()