import pandas as pd
import time
start_time = time.time()




def main():
    north_counties = ['wheeler','collingsworth','hall','donley', 'briscoe', 'childress', 'Floyd', 'Motley', 'Cottle', 'Crosby', 'Dickens', 'King', 'Knox', 'Garza', 'Kent', 'Stonewall', 'Haskell','Wilbarger', 'Baylor', 'Wichita', 'Archer', 'Clay', 'Montague', 'Cooke', 'Grayson', 'Fannin', 'Lamar', 'Red River', 'Bowie']
    far_west_counties = ['borden', 'howard', 'glasscock', 'reagan', 'crockett', 'dawson', 'andrews', 'martin', 'loving', 'winkler', 'ector', 'midland', 'ward', 'crane', 'upton', 'el paso', 'hudspeth', 'culberson', 'reeves', 'pecos', 'terrell', 'brewster', 'presidio', 'jeff davis']
    west_counties = ['scurry', 'fisher', 'jones', 'mitchell', 'nolan', 'taylor', 'sterling', 'coke', 'runnels', 'coleman', 'irion', 'tom green', 'concho', 'mcculloch', 'san saba', 'lampasas', 'schleicher', 'menard', 'mason', 'uaro', 'val verde', 'sutton', 'kimble', 'gillespie', 'edwards', 'real', 'kerr', 'kinney', 'uvalde']
    southern_counties = ['maverick', 'zavala', 'frio', 'alascosa','live oak', 'bee', 'goliad', 'refugio','arkansas','dimmit', 'la salle', 'mcmullen', 'webb', 'duval', 'jim wells', 'san patricio', 'nueces', 'kleberg', 'zapata', 'jim hogg', 'brooks', 'kenedy', 'starr', 'hidalgo','willacy', 'cameron']
    north_central_counties = ['throckmorton', 'shackelford', 'callahan', 'brown', 'eastland', 'stephens', 'young', 'jack', 'palo pinto', 'erath', 'somervell', 'hamilton', 'coryell', 'bell', 'falls', 'mclennan', 'bosque', 'hood', 'parker', 'wise', 'denton', 'tarrant', 'johnson', 'hill', 'limestone', 'freestone', 'navarro', 'ellis', 'dallas', 'denton', 'collin', 'rockwell', 'kaufman', 'hunt']
    south_central_counties = ['burnet', 'blanco', 'kendall', 'bandera', 'medina', 'williamson', 'travis', 'hays', 'cornal', 'bexar', 'bastrop', 'caldwell', 'guadalupe', 'wilson', 'karnes', 'gonzales', 'fayette', 'lee', 'milam', 'burleson', 'washington', 'austin', 'colorado', 'lavaca', 'de witt', 'karnes']
    east_counties = ['delta', 'hopkins', 'rains', 'van zandt', 'henderson', 'anderson', 'freestone', 'leon', 'robertson', 'brazos', 'grimes', 'madison', 'houston', 'trinity', 'walker', 'cherokee', 'smith', 'wood', 'franklin', 'titus', 'morris', 'camp', 'upshur', 'gregg', 'rusk', 'nacogdoches', 'angelina', 'san augustine', 'sabine', 'shelby', 'panola', 'harrison','marion', 'cass']
    coast_counties = ['waller','fort bend', 'wharton', 'jackson', 'victoria', 'calhoun', 'matagorda', 'brazoria', 'harris', 'montgomery', 'san jacinto', 'liberty', 'chambers', 'galveston', 'jefferson', 'hardin', 'polk', 'tyler', 'jasper', 'newton', 'orange']
    df = pd.DataFrame({'north':pd.Series(north_counties), 'far_west':pd.Series(far_west_counties), 'west':pd.Series(west_counties), 'southern':pd.Series(southern_counties), 'north_central':pd.Series(north_central_counties), 'south_central':pd.Series(south_central_counties), 'east':pd.Series(east_counties), 'coast':pd.Series(coast_counties)})
    df = df.stack().reset_index()
    df = df[[0, 'level_1']]
    df = df.rename(columns = {0:'County', 'level_1':'Weather_Zone'})
    df.to_csv('County_WeatherZone_Match.csv')

    
if __name__ == '__main__':
    main()
    print("--- %s seconds ---" % (time.time() - start_time))