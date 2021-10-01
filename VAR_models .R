###################### VAR ###########################

library(tidyverse)
library(tswge)
library(vars)

##https://www.rdocumentation.org/packages/vars/versions/1.5-3/topics/VARselect
####Load Dataset

df <-read.csv(file="C:/users/joshua/Desktop/Cleaned_Data_for_NN.csv")
df$DAY <- as.Date(df$DAY,format="%m/%d/%Y")

## Load Forecasting
## testing on Coastal zone (COAST)
coast_data <- df %>% dplyr::select('DAY','COAST_LOAD','COAST_TMAX', 'COAST_TMIN', 'COAST_POP_IMPUTED')


## create Matrix of variables as vectors, omitting last year of data. Using just temperature and population below.

## Options:
## 2021 data (Jan-May): [4019:4169]
## hold out 2021: [1:4018]
## last year of data: [3805:4169]
## last 2 years of data: [3440:4169]
## hold out last year, use 3 years prior: [2709:3804]
## hold out last year, use all data prior [1:3804]
## 12/31/2020: [4018]


cload = coast_data$COAST_LOAD[1:4018]
ctmax = coast_data$COAST_TMAX[1:4018]
ctmin = coast_data$COAST_TMIN[1:4018]
cpop = coast_data$COAST_POP_IMPUTED[1:4018]

X = cbind(cload, ctmax, ctmin, cpop)

#check what AIC picks for p
VARselect(X, lag.max = 10, type = "const",season = 365, exogen = NULL)

lsfit=VAR(X,p=10,type="const", season=365)
summary(lsfit)

## predictions
preds=predict(lsfit,n.ahead=151)

##if forecast predictions for just cload are needed - preds$fcst$cload

plot(seq(1,4169,1),coast_data$COAST_LOAD, type = "b")
points(seq(4019,4169,1),preds$fcst$cload[1:151,1],type = "b", pch = 15, col='red')

#shorter span
plot(seq(3804,4169,1),coast_data$COAST_LOAD[3804:4169], type = "b", main = "VAR 1/1/21 - 5/31/21 Forecast Validation: Coastal Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4019,4169,1),preds$fcst$cload[1:151,1],type = "b", pch = 15, col='red')


#########################




####### Short term forecasts - no population, based on previous 365 days of data ######

## Import ERCOT 7 day forecasts for comparison
ercot_jan1 <- read.csv(file="C:/users/joshua/Desktop/ERCOT_7DAY_JAN1.csv")
ercot_feb13 <- read.csv(file="C:/users/joshua/Desktop/ERCOT_7DAY_FEB13.csv")

ercot_j_COAST <- ercot_jan1$Coast[2:8]
ercot_f_COAST <- ercot_feb13$Coast[2:8]
ercot_j_EAST <- ercot_jan1$East[2:8]
ercot_f_EAST <- ercot_feb13$East[2:8]
ercot_j_FAR_WEST <- ercot_jan1$FarWest[2:8]
ercot_f_FAR_WEST <- ercot_feb13$FarWest[2:8]
ercot_j_SOUTHERN <- ercot_jan1$Southern[2:8] 
ercot_f_SOUTHERN <- ercot_feb13$Southern[2:8]
ercot_j_SOUTH_CENTRAL <- ercot_jan1$SouthCentral[2:8] 
ercot_f_SOUTH_CENTRAL <- ercot_feb13$SouthCentral[2:8]
ercot_j_NORTH_CENTRAL <- ercot_jan1$NorthCentral[2:8] 
ercot_f_NORTH_CENTRAL <- ercot_feb13$NorthCentral[2:8]
ercot_j_NORTH <- ercot_jan1$North[2:8] 
ercot_f_NORTH <- ercot_feb13$North[2:8]
ercot_j_WEST <- ercot_jan1$West[2:8]
ercot_f_WEST <- ercot_feb13$West[2:8]



## COAST ##

## 2/13-2/19/2021 predictions [4061:4067]
cload = coast_data$COAST_LOAD[3696:4061]
ctmax = coast_data$COAST_TMAX[3696:4061]
ctmin = coast_data$COAST_TMIN[3696:4061]
X = cbind(cload, ctmax, ctmin)
VARselect(X, lag.max = 10, type = "const", exogen = NULL) #check what AIC picks for p

lsfit=VAR(X,p=6,type="const")
summary(lsfit)
## predictions
preds=predict(lsfit,n.ahead=7)
## plot
plot(seq(4000,4067,1),coast_data$COAST_LOAD[4000:4067], type = "b", main = "VAR 7-Day Forecast Validation: Coastal Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4062,4068,1),preds$fcst$cload[1:7,1],type = "b", pch = 15, col='red')


## 1/1/2021 - 1/7/2021 predictions
cload2 = coast_data$COAST_LOAD[3653:4018]
ctmax2 = coast_data$COAST_TMAX[3653:4018]
ctmin2 = coast_data$COAST_TMIN[3653:4018]
X2 = cbind(cload2, ctmax2, ctmin2)
VARselect(X2, lag.max = 10, type = "const", exogen = NULL) #check what AIC picks for p

lsfit2=VAR(X2,p=8,type="const")
summary(lsfit2)
## predictions
preds2=predict(lsfit2,n.ahead=7)
## plot
plot(seq(3950,4025,1),coast_data$COAST_LOAD[3950:4025], type = "b",main = "VAR 7-Day Forecast Validation: Coastal Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4019,4025,1),preds2$fcst$cload[1:7,1],type = "b", pch = 15, col='red')

## ASE of each 7 day forecast vs actual
ASE_COAST_JAN1 <- mean((coast_data$COAST_LOAD[4019:4025] - preds2$fcst$cload[1:7])^2)
ASE_COAST_FEB13 <- mean((coast_data$COAST_LOAD[4062:4068] - preds$fcst$cload[1:7])^2)




#### EAST ####

## 2/13-2/19/2021 predictions [4061:4067]
cload = df$EAST_LOAD[3696:4061]
ctmax = df$EAST_TMAX[3696:4061]
ctmin = df$EAST_TMIN[3696:4061]
X2 = cbind(cload, ctmax, ctmin)
VARselect(X2, lag.max = 10, type = "const", exogen = NULL) #check what AIC picks for p

lsfit2=VAR(X2,p=6,type="const")
summary(lsfit2)
## predictions
preds2=predict(lsfit2,n.ahead=7)
## plot
plot(seq(4000,4067,1),df$EAST_LOAD[4000:4067], type = "b", main = "VAR 7-Day Forecast Validation: Eastern Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4062,4068,1),preds2$fcst$cload[1:7,1],type = "b", pch = 15, col='red')


## 1/1/2021 - 1/7/2021 predictions
cload = df$EAST_LOAD[3653:4018]
ctmax = df$EAST_TMAX[3653:4018]
ctmin = df$EAST_TMIN[3653:4018]
X2 = cbind(cload, ctmax, ctmin)
VARselect(X2, lag.max = 10, type = "const") #check what AIC picks for p

lsfit2=VAR(X2,p=7,type="const")
summary(lsfit2)
## predictions
preds2=predict(lsfit2,n.ahead=7)
## plot
plot(seq(3950,4025,1),df$EAST_LOAD[3950:4025], type = "b", main = "VAR 7-Day Forecast Validation: Eastern Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4019,4025,1),preds2$fcst$cload[1:7,1],type = "b", pch = 15, col='red')

## ASE of each 7 day forecast vs actual
ASE_EAST_JAN1 <- mean((df$EAST_LOAD[4019:4025] - preds2$fcst$cload[1:7])^2)
ASE_EAST_FEB13 <- mean((df$EAST_LOAD[4062:4068] - preds$fcst$cload[1:7])^2)




#### FAR WEST ####

## 2/13-2/19/2021 predictions [4061:4067]
cload = df$FAR_WEST_LOAD[3696:4061]
ctmax = df$FAR_WEST_TMAX[3696:4061]
ctmin = df$FAR_WEST_TMIN[3696:4061]
X2 = cbind(cload, ctmax, ctmin)
VARselect(X2, lag.max = 10, type = "const", exogen = NULL) #check what AIC picks for p

lsfit2=VAR(X2,p=3,type="const")
summary(lsfit2)
## predictions
preds2=predict(lsfit2,n.ahead=7)
## plot
plot(seq(4000,4067,1),df$FAR_WEST_LOAD[4000:4067], type = "b", main = "VAR 7-Day Forecast Validation: Far West Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4062,4068,1),preds2$fcst$cload[1:7,1],type = "b", pch = 15, col='red')


## 1/1/2021 - 1/7/2021 predictions
cload = df$FAR_WEST_LOAD[3653:4018]
ctmax = df$FAR_WEST_TMAX[3653:4018]
ctmin = df$FAR_WEST_TMIN[3653:4018]
X2 = cbind(cload, ctmax, ctmin)
VARselect(X2, lag.max = 10, type = "const") #check what AIC picks for p

lsfit2=VAR(X2,p=5,type="const")
summary(lsfit2)
## predictions
preds2=predict(lsfit2,n.ahead=7)
## plot
plot(seq(3950,4025,1),df$FAR_WEST_LOAD[3950:4025], type = "b", main = "VAR 7-Day Forecast Validation: Far West Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4019,4025,1),preds2$fcst$cload[1:7,1],type = "b", pch = 15, col='red')

## ASE of each 7 day forecast vs actual
ASE_FAR_WEST_JAN1 <- mean((df$FAR_WEST_LOAD[4019:4025] - preds2$fcst$cload[1:7])^2)
ASE_FAR_WEST_FEB13 <- mean((df$FAR_WEST_LOAD[4062:4068] - preds$fcst$cload[1:7])^2)



#### WEST ####

## 2/13-2/19/2021 predictions [4061:4067]
cload = df$WEST_LOAD[3696:4061]
ctmax = df$WEST_TMAX[3696:4061]
ctmin = df$WEST_TMIN[3696:4061]
X2 = cbind(cload, ctmax, ctmin)
VARselect(X2, lag.max = 10, type = "const", exogen = NULL) #check what AIC picks for p

lsfit2=VAR(X2,p=3,type="const")
summary(lsfit2)
## predictions
preds2=predict(lsfit2,n.ahead=7)
## plot
plot(seq(4000,4067,1),df$WEST_LOAD[4000:4067], type = "b", main = "VAR 7-Day Forecast Validation: West Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4062,4068,1),preds2$fcst$cload[1:7,1],type = "b", pch = 15, col='red')


## 1/1/2021 - 1/7/2021 predictions
cload = df$WEST_LOAD[3653:4018]
ctmax = df$WEST_TMAX[3653:4018]
ctmin = df$WEST_TMIN[3653:4018]
X2 = cbind(cload, ctmax, ctmin)
VARselect(X2, lag.max = 10, type = "const") #check what AIC picks for p

lsfit2=VAR(X2,p=5,type="const")
summary(lsfit2)
## predictions
preds2=predict(lsfit2,n.ahead=7)
## plot
plot(seq(3950,4025,1),df$WEST_LOAD[3950:4025], type = "b", main = "VAR 7-Day Forecast Validation: Far West Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4019,4025,1),preds2$fcst$cload[1:7,1],type = "b", pch = 15, col='red')

## ASE of each 7 day forecast vs actual
ASE_WEST_JAN1 <- mean((df$WEST_LOAD[4019:4025] - preds2$fcst$cload[1:7])^2)
ASE_WEST_FEB13 <- mean((df$WEST_LOAD[4062:4068] - preds$fcst$cload[1:7])^2)




#### NORTH ####

## 2/13-2/19/2021 predictions [4061:4067]
cload = df$NORTH_LOAD[3696:4061]
ctmax = df$NORTH_TMAX[3696:4061]
ctmin = df$NORTH_TMIN[3696:4061]
X2 = cbind(cload, ctmax, ctmin)
VARselect(X2, lag.max = 10, type = "const", exogen = NULL) #check what AIC picks for p

lsfit2=VAR(X2,p=3,type="const")
summary(lsfit2)
## predictions
preds2=predict(lsfit2,n.ahead=7)
## plot
plot(seq(4000,4067,1),df$NORTH_LOAD[4000:4067], type = "b", main = "VAR 7-Day Forecast Validation: North Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4062,4068,1),preds2$fcst$cload[1:7,1],type = "b", pch = 15, col='red')


## 1/1/2021 - 1/7/2021 predictions
cload = df$NORTH_LOAD[3653:4018]
ctmax = df$NORTH_TMAX[3653:4018]
ctmin = df$NORTH_TMIN[3653:4018]
X2 = cbind(cload, ctmax, ctmin)
VARselect(X2, lag.max = 10, type = "const") #check what AIC picks for p

lsfit2=VAR(X2,p=5,type="const")
summary(lsfit2)
## predictions
preds2=predict(lsfit2,n.ahead=7)
## plot
plot(seq(3950,4025,1),df$NORTH_LOAD[3950:4025], type = "b", main = "VAR 7-Day Forecast Validation: North Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4019,4025,1),preds2$fcst$cload[1:7,1],type = "b", pch = 15, col='red')

## ASE of each 7 day forecast vs actual
ASE_NORTH_JAN1 <- mean((df$NORTH_LOAD[4019:4025] - preds2$fcst$cload[1:7])^2)
ASE_NORTH_FEB13 <- mean((df$NORTH_LOAD[4062:4068] - preds$fcst$cload[1:7])^2)




#### NORTH CENTRAL ####

## 2/13-2/19/2021 predictions [4061:4067]
cload = df$NORTH_CENTRAL_LOAD[3696:4061]
ctmax = df$NORTH_CENTRAL_TMAX[3696:4061]
ctmin = df$NORTH_CENTRAL_TMIN[3696:4061]
X2 = cbind(cload, ctmax, ctmin)
VARselect(X2, lag.max = 10, type = "const", exogen = NULL) #check what AIC picks for p

lsfit2=VAR(X2,p=3,type="const")
summary(lsfit2)
## predictions
preds2=predict(lsfit2,n.ahead=7)
## plot
plot(seq(4000,4067,1),df$NORTH_CENTRAL_LOAD[4000:4067], type = "b", main = "VAR 7-Day Forecast Validation: North Central Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4062,4068,1),preds2$fcst$cload[1:7,1],type = "b", pch = 15, col='red')


## 1/1/2021 - 1/7/2021 predictions
cload = df$NORTH_CENTRAL_LOAD[3653:4018]
ctmax = df$NORTH_CENTRAL_TMAX[3653:4018]
ctmin = df$NORTH_CENTRAL_TMIN[3653:4018]
X2 = cbind(cload, ctmax, ctmin)
VARselect(X2, lag.max = 10, type = "const") #check what AIC picks for p

lsfit2=VAR(X2,p=7,type="const")
summary(lsfit2)
## predictions
preds2=predict(lsfit2,n.ahead=7)
## plot
plot(seq(3950,4025,1),df$NORTH_CENTRAL_LOAD[3950:4025], type = "b", main = "VAR 7-Day Forecast Validation: North Central Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4019,4025,1),preds2$fcst$cload[1:7,1],type = "b", pch = 15, col='red')

## ASE of each 7 day forecast vs actual
ASE_NORTH_CENTRAL_JAN1 <- mean((df$NORTH_CENTRAL_LOAD[4019:4025] - preds2$fcst$cload[1:7])^2)
ASE_NORTH_CENTRAL_FEB13 <- mean((df$NORTH_CENTRAL_LOAD[4062:4068] - preds$fcst$cload[1:7])^2)




#### SOUTH CENTRAL ####

## 2/13-2/19/2021 predictions [4061:4067]
cload = df$SOUTH_CENTRAL_LOAD[3696:4061]
ctmax = df$SOUTH_CENTRAL_TMAX[3696:4061]
ctmin = df$SOUTH_CENTRAL_TMIN[3696:4061]
X2 = cbind(cload, ctmax, ctmin)
VARselect(X2, lag.max = 10, type = "const", exogen = NULL) #check what AIC picks for p

lsfit2=VAR(X2,p=3,type="const")
summary(lsfit2)
## predictions
preds2=predict(lsfit2,n.ahead=7)
## plot
plot(seq(4000,4067,1),df$SOUTH_CENTRAL_LOAD[4000:4067], type = "b", main = "VAR 7-Day Forecast Validation: SOUTH Central Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4062,4068,1),preds2$fcst$cload[1:7,1],type = "b", pch = 15, col='red')


## 1/1/2021 - 1/7/2021 predictions
cload = df$SOUTH_CENTRAL_LOAD[3653:4018]
ctmax = df$SOUTH_CENTRAL_TMAX[3653:4018]
ctmin = df$SOUTH_CENTRAL_TMIN[3653:4018]
X2 = cbind(cload, ctmax, ctmin)
VARselect(X2, lag.max = 10, type = "const") #check what AIC picks for p

lsfit2=VAR(X2,p=6,type="const")
summary(lsfit2)
## predictions
preds2=predict(lsfit2,n.ahead=7)
## plot
plot(seq(3950,4025,1),df$SOUTH_CENTRAL_LOAD[3950:4025], type = "b", main = "VAR 7-Day Forecast Validation: SOUTH Central Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4019,4025,1),preds2$fcst$cload[1:7,1],type = "b", pch = 15, col='red')

## ASE of each 7 day forecast vs actual
ASE_SOUTH_CENTRAL_JAN1 <- mean((df$SOUTH_CENTRAL_LOAD[4019:4025] - preds2$fcst$cload[1:7])^2)
ASE_SOUTH_CENTRAL_FEB13 <- mean((df$SOUTH_CENTRAL_LOAD[4062:4068] - preds$fcst$cload[1:7])^2)






#### SOUTH ####

## 2/13-2/19/2021 predictions [4061:4067]
cload = df$SOUTHERN_LOAD[3696:4061]
ctmax = df$SOUTH_TMAX[3696:4061]
ctmin = df$SOUTH_TMIN[3696:4061]
X2 = cbind(cload, ctmax, ctmin)
VARselect(X2, lag.max = 10, type = "const", exogen = NULL) #check what AIC picks for p

lsfit2=VAR(X2,p=4,type="const")
summary(lsfit2)
## predictions
preds2=predict(lsfit2,n.ahead=7)
## plot
plot(seq(4000,4067,1),df$SOUTHERN_LOAD[4000:4067], type = "b", main = "VAR 7-Day Forecast Validation: Southern Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4062,4068,1),preds2$fcst$cload[1:7,1],type = "b", pch = 15, col='red')


## 1/1/2021 - 1/7/2021 predictions
cload = df$SOUTHERN_LOAD[3653:4018]
ctmax = df$SOUTH_TMAX[3653:4018]
ctmin = df$SOUTH_TMIN[3653:4018]
X2 = cbind(cload, ctmax, ctmin)
VARselect(X2, lag.max = 10, type = "const") #check what AIC picks for p

lsfit2=VAR(X2,p=6,type="const")
summary(lsfit2)
## predictions
preds2=predict(lsfit2,n.ahead=7)
## plot
plot(seq(3950,4025,1),df$SOUTHERN_LOAD[3950:4025], type = "b", main = "VAR 7-Day Forecast Validation: Southern Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4019,4025,1),preds2$fcst$cload[1:7,1],type = "b", pch = 15, col='red')

## ASE of each 7 day forecast vs actual
ASE_SOUTHERN_JAN1 <- mean((df$SOUTHERN_LOAD[4019:4025] - preds2$fcst$cload[1:7])^2)
ASE_SOUTHERN_FEB13 <- mean((df$SOUTHERN_LOAD[4062:4068] - preds$fcst$cload[1:7])^2)





#### VAR 7-Day ASE Values ####

## Winter Storm
ASE_COAST_FEB13
ASE_EAST_FEB13
ASE_FAR_WEST_FEB13
ASE_NORTH_CENTRAL_FEB13
ASE_NORTH_FEB13
ASE_SOUTH_CENTRAL_FEB13
ASE_SOUTHERN_FEB13
ASE_WEST_FEB13

## Jan 1 2021
ASE_COAST_JAN1
ASE_EAST_JAN1
ASE_FAR_WEST_JAN1
ASE_NORTH_CENTRAL_JAN1
ASE_NORTH_JAN1
ASE_SOUTH_CENTRAL_JAN1
ASE_SOUTHERN_JAN1
ASE_WEST_JAN1









#################################### LONG TERM MODELS ###########################################


#### COAST ####

## 151 Day Forecast starting 1/1/2021##
## Use last 3 years: [2923:4018]
## Use last 2 years: [3288:4018]

cload = df$COAST_LOAD[3288:4018]
ctmax = df$COAST_TMAX[3288:4018]
ctmin = df$COAST_TMIN[3288:4018]
cpop = df$COAST_POP_IMPUTED[3288:4018]

X = cbind(cload, ctmax, ctmin, cpop)

#check what AIC picks for p
VARselect(X, lag.max = 10, type = "const",season = 365, exogen = NULL)

lsfit=VAR(X,p=4,type="const", season=365)
summary(lsfit)

## predictions
preds=predict(lsfit,n.ahead=151)

#plot
plot(seq(3804,4169,1),df$COAST_LOAD[3804:4169], type = "b", main = "VAR 1/1/21 - 5/31/21 Forecast Validation: Coastal Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4019,4169,1),preds$fcst$cload[1:151,1],type = "b", pch = 15, col='red')

## ASE of 151 day forecast vs actual
ASE_COAST_LONG <- mean((df$COAST_LOAD[4019:4169] - preds$fcst$cload[1:151])^2)




#### EAST ####

## 151 Day Forecast starting 1/1/2021##

cload = df$EAST_LOAD[3288:4018]
ctmax = df$EAST_TMAX[3288:4018]
ctmin = df$EAST_TMIN[3288:4018]
cpop = df$EAST_POP_IMPUTED[3288:4018]
X = cbind(cload, ctmax, ctmin, cpop)

#check what AIC picks for p
VARselect(X, lag.max = 20, type = "const",season = 365, exogen = NULL)

lsfit=VAR(X,p=18,type="const", season=365)
#summary(lsfit)
## predictions
preds=predict(lsfit,n.ahead=151)

#plot
plot(seq(3804,4169,1),df$EAST_LOAD[3804:4169], type = "b", main = "VAR 1/1/21 - 5/31/21 Forecast Validation: EAST Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4019,4169,1),preds$fcst$cload[1:151,1],type = "b", pch = 15, col='red')

## ASE of 151 day forecast vs actual
ASE_EAST_LONG <- mean((df$EAST_LOAD[4019:4169] - preds$fcst$cload[1:151])^2)







#### FAR_WEST ####

## 151 Day Forecast starting 1/1/2021## *Uses 3 years*

cload = df$FAR_WEST_LOAD[2923:4018]
ctmax = df$FAR_WEST_TMAX[2923:4018]
ctmin = df$FAR_WEST_TMIN[2923:4018]
cpop = df$FAR_WEST_POP_IMPUTED[2923:4018]
X = cbind(cload, ctmax, ctmin, cpop)

#check what AIC picks for p
VARselect(X, lag.max = 19, type = "const",season = 365, exogen = NULL)

lsfit=VAR(X,p=9,type="const", season=365)
#summary(lsfit)
## predictions
preds=predict(lsfit,n.ahead=151)

#plot
plot(seq(3804,4169,1),df$FAR_WEST_LOAD[3804:4169], type = "b", main = "VAR 1/1/21 - 5/31/21 Forecast Validation: FAR_WEST Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4019,4169,1),preds$fcst$cload[1:151,1],type = "b", pch = 15, col='red')

## ASE of 151 day forecast vs actual
ASE_FAR_WEST_LONG <- mean((df$FAR_WEST_LOAD[4019:4169] - preds$fcst$cload[1:151])^2)






#### NORTH ####

## 151 Day Forecast starting 1/1/2021##

cload = df$NORTH_LOAD[3288:4018]
ctmax = df$NORTH_TMAX[3288:4018]
ctmin = df$NORTH_TMIN[3288:4018]
cpop = df$NORTH_POP_IMPUTED[3288:4018]
X = cbind(cload, ctmax, ctmin, cpop)

#check what AIC picks for p
VARselect(X, lag.max = 20, type = "const",season = 365, exogen = NULL)

lsfit=VAR(X,p=18,type="const", season=365)
#summary(lsfit)
## predictions
preds=predict(lsfit,n.ahead=151)

#plot
plot(seq(3804,4169,1),df$NORTH_LOAD[3804:4169], type = "b", main = "VAR 1/1/21 - 5/31/21 Forecast Validation: NORTH Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4019,4169,1),preds$fcst$cload[1:151,1],type = "b", pch = 15, col='red')

## ASE of 151 day forecast vs actual
ASE_NORTH_LONG <- mean((df$NORTH_LOAD[4019:4169] - preds$fcst$cload[1:151])^2)






#### NORTH_CENTRAL ####

## 151 Day Forecast starting 1/1/2021##

cload = df$NORTH_CENTRAL_LOAD[3288:4018]
ctmax = df$NORTH_CENTRAL_TMAX[3288:4018]
ctmin = df$NORTH_CENTRAL_TMIN[3288:4018]
cpop = df$NORTH_CENTRAL_POP_IMPUTED[3288:4018]
X = cbind(cload, ctmax, ctmin, cpop)

#check what AIC picks for p
VARselect(X, lag.max = 20, type = "const",season = 365, exogen = NULL)

lsfit=VAR(X,p=3,type="const", season=365)
#summary(lsfit)
## predictions
preds=predict(lsfit,n.ahead=151)

#plot
plot(seq(3804,4169,1),df$NORTH_CENTRAL_LOAD[3804:4169], type = "b", main = "VAR 1/1/21 - 5/31/21 Forecast Validation: NORTH_CENTRAL Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4019,4169,1),preds$fcst$cload[1:151,1],type = "b", pch = 15, col='red')

## ASE of 151 day forecast vs actual
ASE_NORTH_CENTRAL_LONG <- mean((df$NORTH_CENTRAL_LOAD[4019:4169] - preds$fcst$cload[1:151])^2)






#### SOUTHERN ####

## 151 Day Forecast starting 1/1/2021##

cload = df$SOUTHERN_LOAD[3288:4018]
ctmax = df$SOUTH_TMAX[3288:4018]
ctmin = df$SOUTH_TMIN[3288:4018]
cpop = df$SOUTH_POP_IMPUTED[3288:4018]
X = cbind(cload, ctmax, ctmin, cpop)

#check what AIC picks for p
VARselect(X, lag.max = 30, type = "const",season = 365, exogen = NULL)

lsfit=VAR(X,p=28,type="const", season=365)
#summary(lsfit)
## predictions
preds=predict(lsfit,n.ahead=151)

#plot
plot(seq(3804,4169,1),df$SOUTHERN_LOAD[3804:4169], type = "b", main = "VAR 1/1/21 - 5/31/21 Forecast Validation: SOUTHERN Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4019,4169,1),preds$fcst$cload[1:151,1],type = "b", pch = 15, col='red')

## ASE of 151 day forecast vs actual
ASE_SOUTHERN_LONG <- mean((df$SOUTHERN_LOAD[4019:4169] - preds$fcst$cload[1:151])^2)







#### SOUTH_CENTRAL ####

## 151 Day Forecast starting 1/1/2021## *Uses 3 Years*

cload = df$SOUTH_CENTRAL_LOAD[2923:4018]
ctmax = df$SOUTH_CENTRAL_TMAX[2923:4018]
ctmin = df$SOUTH_CENTRAL_TMIN[2923:4018]
cpop = df$SOUTH_CENTRAL_POP_IMPUTED[2923:4018]
X = cbind(cload, ctmax, ctmin, cpop)

#check what AIC picks for p
VARselect(X, lag.max = 30, type = "const",season = 365, exogen = NULL)

lsfit=VAR(X,p=3,type="const", season=365)
#summary(lsfit)
## predictions
preds=predict(lsfit,n.ahead=151)

#plot
plot(seq(3804,4169,1),df$SOUTH_CENTRAL_LOAD[3804:4169], type = "b", main = "VAR 1/1/21 - 5/31/21 Forecast Validation: SOUTH_CENTRAL Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4019,4169,1),preds$fcst$cload[1:151,1],type = "b", pch = 15, col='red')

## ASE of 151 day forecast vs actual
ASE_SOUTH_CENTRAL_LONG <- mean((df$SOUTH_CENTRAL_LOAD[4019:4169] - preds$fcst$cload[1:151])^2)









#### WEST ####

## 151 Day Forecast starting 1/1/2021## *Uses 3 Years*

cload = df$WEST_LOAD[2923:4018]
ctmax = df$WEST_TMAX[2923:4018]
ctmin = df$WEST_TMIN[2923:4018]
cpop = df$WEST_POP_IMPUTED[2923:4018]
X = cbind(cload, ctmax, ctmin, cpop)

#check what AIC picks for p
VARselect(X, lag.max = 25, type = "const",season = 365, exogen = NULL)

lsfit=VAR(X,p=20,type="const", season=365)
#summary(lsfit)
## predictions
preds=predict(lsfit,n.ahead=151)

#plot
plot(seq(3804,4169,1),df$WEST_LOAD[3804:4169], type = "b", main = "VAR 1/1/21 - 5/31/21 Forecast Validation: WEST Zone", xlab = "Day", ylab="Total Load (MWh)")
points(seq(4019,4169,1),preds$fcst$cload[1:151,1],type = "b", pch = 15, col='red')

## ASE of 151 day forecast vs actual
ASE_WEST_LONG <- mean((df$WEST_LOAD[4019:4169] - preds$fcst$cload[1:151])^2)





##### ASE for 151-Day VAR Forecasts #####
ASE_COAST_LONG
ASE_EAST_LONG
ASE_FAR_WEST_LONG
ASE_NORTH_LONG
ASE_NORTH_CENTRAL_LONG
ASE_SOUTHERN_LONG
ASE_SOUTH_CENTRAL_LONG
ASE_WEST_LONG
